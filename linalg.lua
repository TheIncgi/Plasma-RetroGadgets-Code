local linalg = {}

-- Linear Algebra Library
local swizzleIndexes = {
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

return linalg