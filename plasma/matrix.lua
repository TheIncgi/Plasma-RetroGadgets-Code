local NIL = {}

--id stack, **matrices only**
local stack = {1}
local resourceID = 1 --already used in stack, increments then gets
--vectors/matrices
local stuff = {
	{type="matrix",val={
		{1,0,0,0},
		{0,1,0,0},
		{0,0,1,0},
		{0,0,0,1},
	}}
} --stuff[id] = {type="",val=?}


--wrapped at end
local lib = {}

local function done( ... )
	output = output or function() end
	ret = { ... }
	for i =1, 4 do
		local outName = "out"..i
		write_var( ret[ i ], outName )
	end
	output( nil, 1 ) -- notify updated
	output( nil, 2 ) -- call done
	return ...
end

local function check( bundle, expected, lvl )
	if bundle == nil then
		error("Invalid ID", 2)
	elseif bundle.type == expected then
		return bundle.val
	end
	error(("Expected ID of %s, got %s"):format(expected, bundle.type), (lvl or 1)+1)
end

local function reg( resource, typeName )
	resourceID = resourceID + 1
	stuff[ resourceID ] = { type=typeName, val=resource }
	return resourceID
end

local function mult( a, b ) 
	if #a[1] ~= #b then 
		error("matrix size mis-match",2) 
	end 
	local out = {} 
	for y = 1, #a do 
		out[y] = {} 
		for x = 1, #a[y] do 
			out[y][x] = 0 
			for i = 1, #a do 
				out[y][x] = out[y][x] + a[y][i] * b[i][x] 
			end 
		end 
	end 
	return out 
end 

local function _magnitude( vec )
	local x,y,z = vec[1], vec[2], vec[3]
	return math.sqrt( x*x + y*y + z*z )
end
local function _normalized( vec )
	local m = _magnitude( vec )
	return { vec[1] / m, vec[2] / m, vec[3] / m, vec[4] }
end

 --https://math.stackexchange.com/a/4155115
local function _mkRotateMatrix( vec, amount )
	local a = _normalized( vec )
	a = {x=a[1], y=a[2], z=a[3],} --easier to read
	a.X, a.Y, a.Z = a.x*a.x,  a.y*a.y,  a.z*a.z --^2, found on identity
	local C = math.cos( amount )
	local S = math.sin( amount )
	local U = 1 - C
	local mat = {
		{ U * a.X       + C,          U * a.x * a.y - S * a.z,   U * a.x * a.z + S * a.y,   0 },
		{ U * a.x * a.y + S * a.z,    U * a.Y       + C      ,   U * a.y * a.z - S * a.x,   0 },
		{ U * a.x * a.z - S * a.y,    U * a.y * a.z + S * a.x,   U * a.Z       + C      ,   0 },
		{                        0,                          0,                        0,   1 }
	}
	return mat
end

function test()
	done()
end

function lib.testw()
end

function lib.rtype( id )
	if id == nil then
		error"id is nil"
	end
	return stuff[ id ] and stuff[id].type or "nil "..t
end

function lib.set( id )
	local mat = check( stuff[id], "matrix" )
	
	stack[math.max(1,#stack)] = mat
	return mat
end

--returns id of a matrix
function lib.get()
	return stack[#stack]
end

function lib.del( id )
	for i=1,#stack-1 do
		if stack[i] == id then error("Attempt to delete value in stack") end
	end
	stuff[ id ] = nil
end

function lib.read( id )
	id = id or get()
	local t = lib.rtype( id )
	local v = "?"
	if t == "vector" then
		local vec = check( stuff[id], "vector" )
		v = ("<%d, %d, %d, %d>"):format( table.unpack(vec) )
	elseif t == "matrix" then
		local mat = check( stuff[id], "matrix" )
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


function lib.write( str )
end

function lib.identity( id ) 
	id = id or lib.get()
	local mat = check( stuff[id], "matrix",2 )

	stuff[id].val = { 
		{1,0,0,0},
		{0,1,0,0},
		{0,0,1,0},
		{0,0,0,1}
	}
	return id
end 

function lib.transform( matrixID, xyzVecID )
	local mat = stuff[ matrixID or lib.get() ]
	local xyz = stuff[ xyzVecID ]
	mat = check(mat,"matrix")
	xyz = check(xyz,"vector")

	for i = 1, 3 do
		mat[i][4] = mat[i][4] + xyz[i]
	end
	return matrixID
end

function lib.transpose( id ) 
	id = id or lib.get()
	local a = check( stuff[id], "matrix" )
	
	for y = 1, #a do 
		for x = 1, #a[y] do 
			if x > y then 
				local t = a[y][x] 
				a[y][x] = a[x][y] 
				a[x][y] = t 
			end 
		end 
	end 
	
	return id
end 

function lib.rotate( matID, rotVecID, amount ) 
	matID = matID or lib.get()
	local mat = check( stuff[matID], "matrix",2 )
	local vec = check( stuff[rotVecID], "vector",2 )

	local rotMat = _mkRotateMatrix( vec, amount )
	
	local r = mult( mat, rotMat ) --TODO check order
	stuff[ matID ].val = r
end 

function lib.scale( matID, scaleVecID )
	matID = matID or lib.get()
	local mat = check( stuff[matID], "matrix" )
	local vec = check( stuff[scaleVecID], "vector" )

	for i=1,3 do
		mat[i][i] = mat[i][i] * vec[i]
	end
	return matID
end

function lib.apply( matID, pointVecID ) 
	local mat = check( stuff[matID], "matrix",2 )
	local vec = check( stuff[pointVecID], "vector", 2)

	local colVec = {{vec[1]},{vec[2]},{vec[3]},{vec[4] or 1}}

	local r = mult( mat, colVec )
	stuff[pointVecID].val = { r[1][1], r[2][1], r[3][1] }
	return pointVecID
end 

function lib.magnitude( vecID )
	local vec = check( stuff[vecID], "vector", 2)
	return _magnitude( vec )
end

function lib.normalize( vecID )
	local vec = check( stuff[vecID], "vector", 2)
	stuff[vecID].val = _normalized( vec )
	return vecID
end

function lib.copy( id )
	local t = lib.rtype( id )
	if not t then
		error("resource missing",2)

	elseif t == "vector" then
		local copy = {}
		for j,v in ipairs( stuff[id].val ) do
			copy[j] = v
		end
		copyID = reg( copy, t )
		return copyID

	elseif t == "matrix" then
		local copy = {}
		for i,r in ipairs( stuff[id].val ) do
			copy[i] = {}
			for j,v in ipairs( r ) do
				copy[i][j] = v
			end
		end
		copyID = reg( copy, t )
		return copyID
	end
end

function lib.push( id )
	id = id or lib.get()
	local a = check( stuff[id], "matrix" )

	local copyID = lib.copy( id )
	stack[#stack+1] = copyID
	return copyID
end

function lib.pop( id )
	local id = stack[#stack]
	if not id then error("pop on Empty stack!") end
	stuff[ id ] = nil
	stack[#stack] = nil
	if #stack == 0 then
		error("Stack emptied!")
	end
	return
end

function lib.mkVec( x,y,z,w )
	return reg( {x,y,z,w or 1}, "vector" )
end

function setPart( id, value, a, b )
	local t = lib.rtype( id )
	if t == "matrix" then
		if not y then error"ROW and COL are required to set matrix value" end
		local mat = check( id, "matrix" )
		mat[a][b] = value
	elseif t == "vector" then
		local vec = check( id, "vector" )
		vec[a] = value
	end
	return id
end

function getPart( id, a, b )
	local t = lib.rtype( id )
	local v
	if t == "matrix" then
		if not y then error"ROW and COL are required to set matrix value" end
		local mat = check( id, "matrix" )
		v = mat[a][b]
	elseif t == "vector" then
		local vec = check( id, "vector" )
		v = vec[a]
	end
	return v
end

--wrapping
--inputs from V1-8
--outputs from return value
for k,v in pairs( lib ) do
	_G[k] = function()
		return done( v(V1,V2,V3,V4,V5,V6,V7,V8) )
	end
end

--local id = get().id
--V1 = 1 V2 = 0 V3 = 0
--local vec = mkVec(1,0,0).id
--print(id)
--print(vec)
--V1 = id
--V2 = vec
--V3 = 90
--rotate()
--print( read() )
