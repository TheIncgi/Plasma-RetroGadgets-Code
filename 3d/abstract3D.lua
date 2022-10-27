-- Retro Gadgets
linalg = {}
rast = {state={vecs={},screenVec={}}} --rasterize
utils = {}
local ITTER;
log = _G.log or print

-- Linear Algebra Library
-- pcall in evaluation 
swizzleIndexes = {
	--positional
	x = 1,
	y = 2,
	z = 3,
	w = 4,
	--color
	r = 1, 
	g = 2,
	b = 3,
	a = 4,
	--texture
	u = 1,
	v = 2
}

function utils.cot( x )
	return 1 / math.tan( x )
end

function linalg._emptyMatrix( rows, cols )
	return {
		type="mat",
		rows=rows, 
		cols=cols, 
		val = {{}}
	}
end
function linalg._emptyVector( size )
	return {type="vec", size=size, val = {}}
end
function linalg.newMatrix( rows, cols )
	local out = linalg._emptyMatrix( rows, cols )
	for r=1,rows do
		out.val[r]={}
		for c=1, cols do
			out.val[r][c] = 0
		end
	end
	
	return out
end

function linalg.newVector( size )
	local out = linalg._emptyVector( size )
	for i=1,size do
		out.val[i] = 0
	end
	return out
end

function linalg.vec( ... )
	local vec = linalg._emptyVector( 0 )
	local args = {...}
	local n = 1
	for i = 1, #args do
		local v = args[i]
		if type(v) == "number" then
			vec.val[n] = v
			n = n + 1
		elseif v.type=="vec" then
			for j = 1, v.size do
				vec.val[n] = v.val[j]
				n = n + 1
			end
		else
			error("Invalid use of vec function with type "..type(v))
		end
	end
	if n-1 <= 1 then error("vec(...) requires 2+ args") end
	vec.size = n-1
	return vec
end

function linalg.mat( ... )
	local val = {}
	local size;
	for i,vec in ipairs{...} do
		if vec.type ~="vec" then error("matrix construction expects vectors for rows") end
		if i==1 then
			size = vec.size
		elseif size~=vec.size then
			error"Can not create a matrix out of mixed vector sizes"
		end
		val[i] = {}
		for j = 1, vec.size do
			val[i][j] = vec.val[j]
		end
	end
	local mat = linalg._emptyMatrix(#val, size)
	mat.val = val
	return mat
end

--this op is in place!
function linalg.identity( matrix )
	for r = 1, matrix.rows do
		for c = 1, matrix.cols do
			matrix.val[r][c] = r==c and 1 or 0
		end
	end
	return matrix
end

function linalg.subMat( x, y )
	if not (x.type=="mat" or y.type=="mat") then
		error("Compiler issue: at least one arg must be matrix",2)
	end
	
	local out = linalg._emptyMatrix( x.rows or y.rows, x.cols or y.cols )
	for r=1, out.rows do
		out[r] = {}
		for c=1, out.cols do
			local a = x.type == "mat" and x.val[r][c] or x.val
			local b = y.type == "mat" and y.val[r][c] or y.val
			out.val[r][c] = a-b
		end
	end
	return out
end

function linalg.addMat( x, y )
	return linalg.subMat(x, -y)
end

function linalg.subVec( x, y )
	if not x then error("not x",2) end
	if not y then error("not y",2) end
	if not (x.type=="vec" or y.type=="vec") then
		error("Compiler issue: at least one arg must be vec",2)
	end

	out = linalg._emptyVector( x.size or y.size )
	for i=1,out.size do
		local a = x.type == "vec" and x.val[i] or x.val
		local b = y.type == "vec" and y.val[i] or y.val
		out.val[i] = a - b
	end
	return out
end

function linalg.addVec( x, y )
	return linalg.subVec( x, -y )
end

function linalg.matrixMult( matA, matB )
	if matA.type=="vec" then
		matA = linalg.vecToRow(matA)
	end
	if matB.type=="vec" then
		matB = linalg.vecToCol(matB)
	end
	if matA.cols ~= matB.rows then
		error("matrix size mismatch")
	end
	local out = linalg._emptyMatrix( matA.rows, matB.cols )
	
	for r = 1, matA.rows do
		out.val[r] = {}
		for c = 1, matB.cols do
			local sum = 0
			for i = 1, matA.cols do
				sum = sum + matA.val[r][i] * matB.val[i][c]
			end
			out.val[r][c] = sum
		end
	end

	if out.rows==1 then
		out = linalg.rowToVec(out,1)
	elseif out.cols==1 then
		out = linalg.colToVec(out,1)
	end
	return out
end

function linalg.magnitude( vec )
	local sum = 0
	for i=1,vec.size do
		sum = sum + vec.val[i] * vec.val[i]
	end
	return math.sqrt( sum )
end

function linalg.normalize( vec )
	local m = linalg.magnitude( vec )
	local out = linalg._emptyVector( vec.size )
	for i = 1, vec.size do
		out.val[i] = m==0 and 0 or vec.val[i] / m
	end
	return out
end

function linalg.dot( a, b )
	if a.size ~= b.size then
		error("vector size mismatch")
	end
	a = linalg.normalize(a)
	b = linalg.normalize(b)

	local sum = 0
	for i = 1, a.size do
		sum = sum + a.val[i] * b.val[i]
	end
	return sum
end

-- x  y  z  x  y
-- a  b  c  a  b
-- d  e  f  d  e
--
-- x = bf - ce
function linalg.cross( a, b )
	if a.size ~= b.size then
		error("vector size mismatch")
	end
	
	a = linalg.normalize(a)
	b = linalg.normalize(b)

	local s = a.size
	local out = linalg._emptyVector( s )

	for i = 1, s do
		local p1 = i % s + 1  --+1, wrapped
		local p2 = p1 % s +1  --+2 wrapped
		local m1 = (i+s-2) % s + 1 -- -1 wrapped (by adding)
		local m2 = (m1+s-2) % s + 1 -- -2 wrapped (by adding)
		out.val[i] = a.val[p1] * b.val[p2] - a.val[m1] * b.val[m2]
	end
	
	return out
end

 --https://math.stackexchange.com/a/4155115
 function linalg.newRotateMatrix( vec, amount )
	if amount == 0 then
		return linalg.identity(linalg.newMatrix( 4, 4 ))
	end

	local a = linalg.normalize( vec )
	a = {x=a.val[1], y=a.val[2], z=a.val[3],} --easier to read
	a.X, a.Y, a.Z = a.x*a.x,  a.y*a.y,  a.z*a.z --^2, found on identity
	local C = math.cos( amount.val )
	local S = math.sin( amount.val )
	local U = 1 - C
	local mat = linalg._emptyMatrix( 4, 4 )
	mat.val[1] = { U * a.X       + C,          U * a.x * a.y - S * a.z,   U * a.x * a.z + S * a.y,   0 }
	mat.val[2] = { U * a.x * a.y + S * a.z,    U * a.Y       + C      ,   U * a.y * a.z - S * a.x,   0 }
	mat.val[3] = { U * a.x * a.z - S * a.y,    U * a.y * a.z + S * a.x,   U * a.Z       + C      ,   0 }
	mat.val[4] = {                        0,                          0,                        0,   1 }
	
	return mat
end

function linalg.transform( mat, offset )
	if mat.rows ~= offset.size then
		error("sizes don't match")
	end
	local out = linalg.copyMatrix( mat )
	for i=1,mat.rows do
		out.val[i][mat.cols] = mat.val[i][mat.cols] + offset.val[i]
	end
	return out
end

function linalg.transpose( mat )
	mat = linalg.copyMatrix( mat )
	for r=1, mat.rows do
		for c=1, mat.cols do
			if c > r then
				local t = mat.val[r][c]
				mat.val[r][c] = mat.val[c][r]
				mat.val[c][r] = t
			end
		end
	end
end

function linalg.rotateMatrix( matrix, axis, degrees )
	local rot = linalg.newRotateMatrix( axis, degrees )
	return linalg.matrixMult( matrix, rot )
end

function linalg.scaleMatrix( matrix, scaleVec )
	local m = math.min( matrix.rows, matrix.cols )
	if scaleVec.size < m then
		error("scale vec too small (number of numbers)")
	end
	local out = linalg.copyMatrix( matrix )
	for i=1,m do
		out.val[i][i] = matrix.val[i][i] * scaleVec.val[i]
	end
	return
end

function linalg.scaleVec( vec, scale )
	local out = linalg._emptyVector( vec.size )
	for i=1,out.size do
		out.val[i] = vec.val[i] * scale
	end
	return out
end

-- n / vec
function linalg.vecUnder( vec, over )
	local out = linalg._emptyVector( vec.size )
	for i=1, out.size do
		out.val[i] = over.val / vec.val[i]
	end
	return out
end

-- n  /mat
function linalg.matUnder( mat, over )
	local out = linalg._emptyMatrix( mat.rows, mat.cols )
	for r=1, out.rows do
		for c=1, out.cols do
			out.val[r][c] = over.val / mat.val[r][c]
		end
	end
	return out
end

function linalg.vecToCol( vec )
	local out = linalg._emptyMatrix( vec.size, 1 );
	for i = 1, vec.size do
		out.val[i] = {vec.val[i]}
	end
	return out
end
function linalg.vecToRow( vec )
	local out = linalg._emptyMatrix( vec.size, 1 );

	for i = 1, vec.size do
		out.val[1][i] = vec.val[i]
	end
	return out
end

function linalg.vecSwizzle( vec, swizzle )
	if #swizzle == 1 then
		local si = swizzleIndexes[swizzle]
		local v = vec.val[ si ]
		return v
	elseif #swizzle == 0 then
		error"Swizzle with len 0"
	end
	local val = {}
	for i = 1, #swizzle do
		local si = swizzleIndexes[ swizzle:sub(i,i) ]
		if not si then error("Invalid swizzle letter '"..swizzle:sub(i,i).."'") end
		val[i] = vec.val[ si ]
		if not val[i] then error("swizzle index '"..swizzle:sub(i,i).."' out of bounds on type "..vec.type) end
	end
	local out = linalg._emptyVector( #swizzle )
	out.val = val
	out.ref = {var=vec, path=swizzle}
	return out
end

function linalg.copyMatrix( from, to )
	to = to or linalg._emptyMatrix( from.rows, from.cols )
	for r=1, from.rows do
		to.val[r] = {}
		for c=1, from.cols do
			to.val[r][c] = from.val[r][c]
		end
	end
	return to
end

function linalg.copyVector( from, to )
	to = to or linalg._emptyVector( from.size )
	for i = 1, from.size do
		to.val[i] = from.val[i]
	end
	return to
end

function linalg.colToVec( matrix, col )
	local out = linalg._emptyVector( matrix.rows )
	for i = 1, matrix.rows do
		out.val[i] = matrix.val[i][col]
	end
	return out
end

function linalg.rowToVec( matrix, row )
	local out = linalg.newVector( matrix.cols )
	for i = 1, matrix.cols do
		out.val[i] = matrix.val[row][i]
	end
	return out
end

function linalg.vecEquals( a, b )
	if a.type ~= b.type then return false end
	for i=1, a.size do
		if a.val[i] ~= b.val[i] then return false end
	end
	return false
end

function linalg.matEquals( a, b )
	if a.type ~= b.type then return false end
	for r=1, a.rows do
		for c=1, a.cols do
			if a.val[r][c] ~= b.val[r][c] then return false end
		end
	end
	return true
end

function linalg.push( matrix, channel )
	linalg.stacks = linalg.stacks or {}
	linalg.stacks[channel] = linalg.stacks[channel] or {}
	table.insert( linalg.stacks[channel], matrix )
end

function linalg.get( channel )
	local s = linalg.stacks[channel]
	return s[#s]
end

function linalg.pop( channel )
	local s = linalg.stacks[channel]
	return table.remove( s )
end

function linalg.resetStacks()
	linalg.stacks = {}
end

function linalg.toString( val )
	local v = tostring( val )
	local t = val.type

	if t == "vec" then
		local vec = val.val
		
		v = ("<%s>"):format( table.concat(vec,",") )
	elseif t == "mat" then
		local mat = val.val
		v = {"\n"}
		for i,row in ipairs( mat ) do
			for j,val in ipairs( row ) do
				v[#v+1] = val .. ","
			end
			v[#v+1] = "\n"
		end
		v[#v] = nil
		v = table.concat( v )
	end
	return ("%s:%s"):format(t, tostring(v))
end

----console screen
local screen = {
  buffer = {},
	depth = {},
  WIDTH = 64,
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
			local g = v /10*255
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
		--todo fovW = 2*atan(aspect*tan(fovH/2))
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
		xyz=linalg.vec( 0.0, 0.5,-0.3),
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
	local env = {
		near=.1, far=20,
		fovW = 90,
		fovH = 90,
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
	for i=1,3 do
		local vertex = tri[i]
		env.pos = vertex.xyz
		tf[i] = shaders.vert( env )
		local sw = linalg.vecSwizzle
		local sc = linalg.scaleVec
		local tmp = sc(tf[i], 1/sw(tf[i],"w"))
		tf[i] = sw( tmp, "xyz" )
	end
	
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

