-- Retro Gadgets
local linalg = require"linalg"
local controller = require"objects/lua/controller_obj"

rast = {state={vecs={},screenVec={}}} --rasterize
utils = {}
local ITTER;
local Texture={}
log = _G.log or print



function utils.cot( x )
	return 1 / math.tan( x )
end



----console screen
local screen = {
  buffer = {},
	depth = {},
  WIDTH = 128,
  HEIGHT = 32
}

local ESC = string.char(27)
local block = string.char(219)

function screen.clear(r,g,b)
  r,g,b = r or 0, g or 0, b or 0
  local x = screen.pixel(r,g,b)
  for h=1,screen.HEIGHT do
    screen.buffer[h] = {}
		screen.depth[h] = {}
    for w=1,screen.WIDTH do
      screen.buffer[h][w] = x
			screen.depth[h][w] = math.huge
    end
  end
end

function screen.pixel(r,g,b)
  return (ESC.."[38;2;%d;%d;%dm"..block):format(r,g,b)
end

function screen.setPixel(x,y, r,g,b)
  screen.buffer[y][x] = screen.pixel(r,g,b)
end

function screen.draw()
  print()
  for h,row in ipairs(screen.buffer) do
    local line = table.concat(row)
    print(line)
  end
end

function screen.drawDepth()
  print()
  for h,row in ipairs(screen.depth) do
    local line = {}
		for i,v in ipairs(row) do
			local g = (v*v)*255
			g = math.max(0,math.min(255,math.floor(g)))
			table.insert(line,screen.pixel(g,g,g))
		end
    print(table.concat(line))
  end
end

screen.clear()


--shader logic
shaders = {
	mkCam = function(env)
		local cot = utils.cot
		local rad = math.rad
			
		local far,near = env.far,env.near
		
		local fovW,fovH = env.fovW, env.fovH
			
		local vec,mat = linalg.vec,linalg.mat
		local rotMat = linalg.rotateMatrix
		local translate = linalg.transform
				
		-- local cam = mat(
		-- 	vec( cot(rad(fovW/2)), 0,0,0 ), 
		-- 	vec( 0, cot(rad(fovH/2)),0,0 ),
		-- 	vec( 0,0,-far/(far-near),   0 ),
		-- 	vec( 0,0,far*near/(near-far), 0)
		-- )
		local fx = cot(rad(fovH/2))
		local fy = cot(rad(fovW/2))

		local proj = mat(
			vec( fx,  0 ,0,0 ), 
			vec(  0, fy, 0,0 ),
			vec(  0,  0, (far+near)/(near-far),   (2*far*near)/(near-far) ),
			vec(  0,  0, -1, 0)
		)
		
		local view = linalg.identity(linalg.newMatrix(4,4))

		view = rotMat(view,vec(0,0,1), rad(env.roll))
		view = rotMat(view,vec(0,0,1), rad(env.pitch))
		view = rotMat(view,vec(0,0,1), rad(env.yaw))
		
		view = translate(view, vec( linalg.scaleVec(env.camPos,-1),0))

		return proj, view
	end,


	
	vert = function( env )
		local pos = env.pos
		local transform = env.transform
		local proj = env.projection
		local view = env.view
		local m = linalg.matrixMult
		--local s = linalg.vecSwizzle
		local vec = linalg.vec
		
		local p = m(m(view,transform), vec(pos,1))
		return m(proj, p)
	end,
	
	frag = function( env )
		local subv = linalg.subVec
		local addv = linalg.addVec
		local dot = linalg.dot
		local scale = linalg.scaleVec
		local barPos = env.barPos

		local lightVec = subv( 
			env.lightPos,
			env.vertPos
		)
		local factor = math.abs(
			dot(env.normal,lightVec)
		)
		factor = math.min(1,factor + .1)
		-- local clr = linalg.vec( env.uvPos, 1 )
		--env.color
		return scale(barPos, factor)
	end
}
-- rasterization
function rast.size()
	return 
		--gdt.VideoChip0.Width, gdt.VideoChip0.Height
		screen.WIDTH, screen.HEIGHT
end

function rast.glToScreen( x, y, z )
	local w, h = rast.size()
	return (x/2+.5)*w, (-y/2+.5)*h, z
end

function rast.screenToGL( x, y )
	local w, h = rast.size()
	return (x/w-.5)*2, (y/h-.5)*-2
end

--vertex approch https://en.wikipedia.org/wiki/Barycentric_coordinate_system
function rast.screenToBary( x, y )
	local a = rast.state.vecs[1]
	local b = rast.state.vecs[2]
	local c = rast.state.vecs[3]
	local x1,x2,x3 = a[1],b[1],c[1]
	local y1,y2,y3 = a[2],b[2],c[2]
	local z1,z2,z3 = a[3],b[3],c[3]
	
	local det = x1 * (y2-y3) + x2 * (y3-y1) +  x3 * (y1-y2)
  --det is 2A
	local f = 1/(det)
	
	if not ITTER.lhs then
		local lhs =  { 
			{ x2*y3  - x3*y2,     y2-y3,     x3-x2 },
			{ x3*y1  - x1*y3,     y3-y1,     x1-x3  },
			{ x1*y2  - x2*y1,     y1-y2,     x2-x1  }
		}
		for r=1,3 do for c=1,3 do
			lhs[r][c] = lhs[r][c] * f
		end end		
		ITTER.lhs = lhs
	end
  local res = {{},{},{}}	
  local v = {{1},{x},{y}}
	for r=1,3 do for c=1,1 do
		for i=1,3 do
			res[r][c] = (res[r][c] or 0) + 
					ITTER.lhs[r][i] * v[i][c]
		end
	end	end 
  return res[1][1], res[2][1], res[3][1]
end

--https://stackoverflow.com/a/2049593
function rast.sign(p1, p2, p3)
	return (p1[1]-p3[1]) * (p2[2]-p3[2]) -(p2[1] -p3[1]) * (p1[2]-p3[2])
end
--https://stackoverflow.com/a/2049593
function rast.isInTri( x, y, p1, p2, p3 )
	
	local t = {x,y}
	local d1 = rast.sign( t, p1, p2 )
	local d2 = rast.sign( t, p2, p3 )
	local d3 = rast.sign( t, p3, p1 )
	local hasNeg = math.min(d1,d2,d3) < 0
	local hasPos = math.max(d1,d2,d3) > 0
	return not (hasNeg and hasPos)
end

function rast.setVecs( vecs )
	rast.state = {vecs={},screenVec={}}
	local state = rast.state
	state.vecs = {}

	state.vecs = vecs

	local v= state.vecs
	local minX = math.min( v[1][1], v[2][1], v[3][1] )
	local minY = math.min( v[1][2], v[2][2], v[3][2] )
	local maxX = math.max( v[1][1], v[2][1], v[3][1] )
	local maxY = math.max( v[1][2], v[2][2], v[3][2] )
	minX, minY = rast.glToScreen( minX, minY )
	maxX, maxY = rast.glToScreen( maxX, maxY )
	minX, minY = math.floor( minX ), math.floor( minY )
	maxX, maxY = math.ceil( maxX ), math.floor( maxY )

	minX, maxX = math.min(minX, maxX), math.max(minX, maxX)
	minY, maxY = math.min(minY, maxY), math.max(minY, maxY)
	
  state.screenVec = {}
	for i=1,3 do
		state.screenVec[i] = {rast.glToScreen( table.unpack(state.vecs[i]) )}
	end
	
	--can do face culling here if z out of range
	ITTER = {
		x = minX,
		y = minY,
		min = {minX, minY},
		max= {maxX, maxY}
	}
end


rast.USE_DEPTH = true

function rast.itterate( onFind )
 -----------------------------
	local a,b,c = table.unpack( rast.state.screenVec )
  
  local x, y = ITTER.x, ITTER.y
  if rast.isInTri( x+.5, y+.5, a,b,c ) then
    local bar = {rast.screenToBary( rast.screenToGL(x,y) )}
		local depth = rast.state.screenVec[1][3] * bar[1] + rast.state.screenVec[2][3] * bar[2] + rast.state.screenVec[3][3] * bar[3]
		local isDepthOk = depth > 0
		local WID, HEI = rast.size()
		if 1 <= x and x <= WID 
			and 1 <= y and y <= HEI then

			if isDepthOk and depth >= -1 then
				if rast.USE_DEPTH then
					local cdepth = screen.depth[y][x]
					if cdepth > depth then
						screen.depth[y][x] = depth
					else
						isDepthOk = false
					end
				end
			end

			if isDepthOk then
				onFind( bar, x, y )
			end
		end
  end
  ITTER.x = x+1
  if x > ITTER.max[1] then
    ITTER.x = ITTER.min[1]
    ITTER.y = ITTER.y+1
    if ITTER.y > ITTER.max[2] then
      ITTER = nil
			return false
    end
  end
 	return true --has more to draw
end

---------------------------------------
local tri = {
	{
		xyz=linalg.vec(-0.5,-0.5, 0.0),
		 uv=linalg.vec(0,0)
	},{
		xyz=linalg.vec( 0.5,-0.5, 0.0),
		 uv=linalg.vec(1,0)
	},{
		xyz=linalg.vec( -.5, 0.5,-0.3),
		 uv=linalg.vec(.5,1)
	}
}
local tri2 = {
	{
		xyz=linalg.vec( 0.0,-0.5, -1.3),
		 uv=linalg.vec(0,0)
	},{
		xyz=linalg.vec( 1.0,-0.5, -1.3),
		 uv=linalg.vec(1,0)
	},{
		xyz=linalg.vec( 0.5, 0.5, -1.6),
		 uv=linalg.vec(.5,1)
	}
}
local tri3 = {
	{
		xyz=linalg.vec( -1.0,-0.5, 1.3),
		 uv=linalg.vec(0,0)
	},{
		xyz=linalg.vec( 0.0,-0.5, 1.3),
		 uv=linalg.vec(1,0)
	},{
		xyz=linalg.vec( -0.5, 0.5, 1.1),
		 uv=linalg.vec(.5,1)
	}
}

function draw(tri)
	local w,h = rast.size()
	local aspect = h/w
	local fovH = 90
	local fovW = math.deg(2*math.atan(aspect*math.tan(math.rad(fovH)/2)))
	local env = {
		near=.1, far=20,
		fovW = fovW,
		fovH = fovH,
		camPos=linalg.vec(0,0,2),
		yaw=0,pitch=0,roll=0
	}
	
	local cross,sub = linalg.cross, linalg.subVec
	
	local proj, view = shaders.mkCam(env)
	env.projection=proj
	env.view = view

	env.transform =
		linalg.identity(linalg.newMatrix(4,4))
	
	log"calc norm"
	env.normal =
		cross(
			sub(tri[2].xyz,tri[1].xyz),
			sub(tri[3].xyz,tri[2].xyz)
		)
	
	log"verts"
	local tf = {}
	local pointIn = false
	for i=1,3 do
		local vertex = tri[i]
		env.pos = vertex.xyz
		tf[i] = shaders.vert( env )
		local sw = linalg.vecSwizzle
		local sc = linalg.scaleVec
		local w = 1/sw(tf[i],"w")
		local abs = math.abs
		if w <= 0 then return end
		if not (abs(tf[i].val[1]) > 1
		and abs(tf[i].val[2]) > 1
		and abs(tf[i].val[3]) > 1) then 
			pointIn = true
		end
		--TODO optionally include clip space coords to frag
		local tmp = sc(tf[i], w )
		tf[i] = sw( tmp, "xyz" )
	end
	if not pointIn then return end
	
	log"frag"
	env.lightPos = linalg.vec(0,100,30)
	env.color = linalg.vec(255,0,0)

	rast.setVecs{ tf[1].val, tf[2].val, tf[3].val }
	local function onPixel(bar,x,y)
		local xyz = linalg.newVector(3)
		local uv = linalg.newVector(2)
			
		for i=1,3 do --bar factor
			--xyz
			xyz.val[1] = xyz.val[1] + 
					tri[i].xyz.val[1] * bar[i]
			xyz.val[2] = xyz.val[2] + 
					tri[i].xyz.val[2] * bar[i]
			xyz.val[3] = xyz.val[3] + 
					tri[i].xyz.val[3] * bar[i]
			--uv
			uv.val[1] = uv.val[1] + 
					tri[i].uv.val[1] * bar[i]
			uv.val[2] = uv.val[2] + 
					tri[i].uv.val[2] * bar[i]
		end
			
		env.vertPos = xyz
		env.uvPos = uv
		env.barPos = linalg._emptyVector(3)
		env.barPos.val = bar

			
		local c = shaders.frag( env )
		for i=1,3 do
			c.val[i] = math.max(0,math.min(255,math.floor(c.val[i]*255)))
		end
		screen.setPixel(x,y, table.unpack(c.val))
		-- c = Color( table.unpack(c.val) )
		-- gdt.VideoChip0:SetPixel(vec2(x,y),c)
	end
	while rast.itterate( onPixel ) do
		--yield()
	end
	screen.draw()
end



-- update function is repeated every time tick
-- function update()
	
-- 	if gdt.LedButton1.ButtonDown then
		draw(tri)
		draw(tri2)
		draw(tri3)

		screen.drawDepth()
-- 	end
	
-- end

