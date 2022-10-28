state = false

function strMask( src, mask, start )
	return src:sub( 1, start-1) .. mask .. src:sub(start+#mask)
end

function size()
	return read_var"bufwid", read_var"bufhei"
end

function glToScreen( x, y )
	local w, h = size()
	return (x/2+.5)*w, (y/2+.5)*h
end

function screenToGL( x, y )
	local w, h = size()
	return (x/w-.5)*2, (y/h-.5)*2
end

--vertex approch https://en.wikipedia.org/wiki/Barycentric_coordinate_system
function screenToBary( x, y )
	local a = state.vecs[1]
	local b = state.vecs[2]
	local c = state.vecs[3]
	local x1,x2,x3 = a[1],b[1],c[1]
	local y1,y2,y3 = a[2],b[2],c[2]
	local z1,z2,z3 = a[3],b[3],c[3]
	
	local det = x1 * (y2-y3) + x2 * (y3-y1) +  x3 * (y1-y2)
  --det is 2A
	local f = 1/(det)
	
	local lhs = { --KEEP LHS AFTER DONE, no need to recompute
		{ x2*y3  - x3*y2,     y2-y3,     x3-x2 },
		{ x3*y1  - x1*y3,     y3-y1,     x1-x3  },
		{ x1*y2  - x2*y1,     y1-y2,     x2-x1  }
	}
	for r=1,3 do for c=1,3 do
		lhs[r][c] = lhs[r][c] * f
	end end		
  local res = {{},{},{}}	
  local v = {{1},{x},{y}}
	for r=1,3 do for c=1,1 do
		for i=1,3 do
			res[r][c] = (res[r][c] or 0) + lhs[r][i] * v[i][c]
		end
	end	end 
  return res[1][1], res[2][1], res[3][1]
end

--https://stackoverflow.com/a/2049593
function sign(p1, p2, p3)
	return (p1[1]-p3[1]) * (p2[2]-p3[2]) -(p2[1] -p3[1]) * (p1[2]-p3[2])
end
--https://stackoverflow.com/a/2049593
function isInTri( x, y, p1, p2, p3 )
	
	local t = {x,y}
	local d1 = sign( t, p1, p2 )
	local d2 = sign( t, p2, p3 )
	local d3 = sign( t, p3, p1 )
	local hasNeg = math.min(d1,d2,d3) < 0
	local hasPos = math.max(d1,d2,d3) > 0
	return not (hasNeg and hasPos)
end

function setVecs()
	state = state or {}
	state.vecs = {}
	for i=1,3 do
		state.vecs[i] = {
			read_var("x"..i),
			read_var("y"..i),
			read_var("z"..i)
		}
	end
	local v= state.vecs
	local minX = math.min( v[1][1], v[2][1], v[3][1] )
	local minY = math.min( v[1][2], v[2][2], v[3][2] )
	local maxX = math.max( v[1][1], v[2][1], v[3][1] )
	local maxY = math.max( v[1][2], v[2][2], v[3][2] )
	minX, minY = glToScreen( minX, minY )
	maxX, maxY = glToScreen( maxX, maxY )
	minX, minY = math.floor( minX ), math.floor( minY )
	maxX, maxY = math.ceil( maxX ), math.floor( maxY )
	
  state.screenVec = {}
	for i=1,3 do
		state.screenVec[i] = {glToScreen( table.unpack(state.vecs[i]) )}
	end
	
	--can do face culling here if z out of range
	ITTER = {
		x = minX,
		y = minY,
		min = {minX, minY},
		max= {maxX, maxY}
	}
end

function done()
  trigger(8)
end
function outBar(bar, px, py)
  for i=1,3 do
    write_var( bar[i], "bc"..i)
  end
  write_var(px,"pixelx")
  write_var(py,"pixely")
  trigger(1)
end
function itterate()
	local a,b,c = table.unpack( state.screenVec )
  local found = false
  for i = 1, 20 do
    local x, y = ITTER.x, ITTER.y
    if isInTri( x+.5, y+.5, a,b,c ) then
      local bar = {screenToBary( screenToGL(x,y) )}
      outBar( bar, x, y )
      found = true
    end
    ITTER.x = x+1
    if x > ITTER.max[1] then
      ITTER.x = ITTER.min[1]
      ITTER.y = ITTER.y+1
      if ITTER.y > ITTER.max[2] then
        ITTER = nil
        done()
      end
    end
    if found then break end
  end
end

local fakeVars = {
  x1 = -.5,
  y1 = -.7446,
  z1 = 3,
  x2 = .5,
  y2 = -.7446,
  z2 = 3,
  x3 = 0,
  y3 = .7446,
  z3 = 3.3010,
  bufwid = 340/4,
  bufhei = 256/4
}
function read_var( x )
  return fakeVars[ x ]
end
function write_var( val, name )
  print(("[WRITE_VAR] %s -> %s"):format( tostring(val), name ))
end
function output( val, pin )
  print(("[OUT] %s -> %d"):format(tostring(val), pin))
end
function trigger( pin )
  print(("[TRIGGER] %d"):format(pin))
end

setVecs()
while true do
  itterate()
end