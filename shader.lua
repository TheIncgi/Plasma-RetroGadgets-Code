--Shader Module
--Version: 1.0a
--Author: TheIncgi
--Oct 2022
--Plasma Demo

--todo copy function in shader for matrix/vector

local TIMEOUT = 20 --loops before exit to continue next tick

--test code
local makeCam = [[
	in num fovWidth;
	in num fovHeight;
	in num near;
	in num far;
	in vec3 pos;
	in vec3 rot; //{yaw, pitch, roll}

	out mat4_4 camera;

	//source https://stackoverflow.com/a/62243585/11295774
	void mkCam() {
		print("#FF00FF","HELLO WORLD");
		camera = mat(
			vec( cot(rad(fovWidth))/2, 0, 0, 0 ),
			vec( 0, cot(rad(fovHeight))/2, 0, 0 ),
			vec( 0, 0, far/(far-near), 0 ), //changed to 0, z pos
			vec( 0, 0, far*near / (near-far), 0 )
		);
		camera = rotateMatrix( camera, vec(0,0,1), rad( rot.z ) ); //roll
		camera = rotateMatrix( camera, vec(1,0,0), rad( rot.y ) ); //pitch
		camera = rotateMatrix( camera, vec(0,1,0), rad( rot.x ) ); //yaw (Y+ is up)

		camera[1,4] = pos.x;
		camera[2,4] = pos.y;
		camera[3,4] = pos.z;
		
		print( camera );
		return; //required in current version, no warnings though
	}

]]

local vert = [[
	in vec3 pos;
	in mat4_4 transform;
	in mat4_4 camera;
	out vec4 camPos;

	vec3 vert() {
		camPos = vec( camera[1,4], camera[2,4], camera[3,4], camera[4,4] );
		vec3 res = (camera * transform * vec(pos,1)).xyz;
		print("BREAKPOINT"); //watching for text in debug
		res.x = 50;
		res[2] = 60;
		res[3] = 70;
		print("#0000FF", "Tf'd: "+res );
		return res;
	}
]]

local frag = [[
	in vec3 color;    //user value
	in vec3 lightPos; // user value
	in vec3 camPos;   //computed in vert shader
	in vec3 norm;    //special to fragment
	in vec3 vertPos; //special to fragment
	in vec2 uvPos;   //special to fragment
	
	
	num global_offset( num v ) {
		return v + 0.1;
	}

	vec4 frag() {
		vec3 lightVec = lightPos - vertPos;
		num factor = abs(norm * lightVec);   //dot product
		//factor += 0.1;
		factor = global_offset( factor ); //does not work yet
		factor = min( 1, factor );
		return color * factor;
	}
]]
----------------------------------------------------------------------------
-- Render steps:
-- 1. Depth buffer computed, only visible geometry is processed after
-- 2. Geometry passed to vertex shader, 
--    transformations and converts to screen space
-- 3. Interpolated vertex pos passed to frag shader, color computed
-- 4. Resulting color info is applied to the buffer
----------------------------------------------------------------------------
-- Code notes:
-- the `in` loads values from a previous step or from the received props table
-- the `out` provides values to the next step
--
-- AXIS
-- x is right
-- y is up
-- z is towards you (out of screen)
--
-- Auto yielding could be done better if coroutines were available, 
-- could also simplify a lot of the logic then too...
--
-- Colors are always 0 to 1 for RGBA
-- all lines are either declerations, asignments or functions 
-- as such they must each start with one of the following:
--  [ in ]  [ out  ]  [  type*  ]  [  return  ]  [  if/while/for  ]  [  variableName  ] [ func ]
-- followed by one of these with an optional space before it
-- [=]  [*=] [ /= ] [+=] [-=] [!=] [ () ] [ name** ] 
-- ** space required
--
-- functions starting with _ are not meant to be accessed by the node
----------------------------------------------------------------------------
-- *** Outline ***
-- example shaders (above)
-- ---------------------
-- shader globals
-- linear algebra code
-- ---------------------
-- keywords, ops & types
-- TIMEOUT setting - tick friendly compling
--    Higher value, faster compling, more lag
-- local functions
-- tokenization
-- compiling
-- ---------------------
-- running
-- ---------------------
-- setup
-- loop
-- ---------------------
-- test code
----------------------------------------------------------------------------
--allow multiple prints in 1 tick...
_nativePrint = print
PRINT = {"checking scrolling","a","b","c","d","e","f","g","h","i","j","k","l","m",} --used in loop
print = function( ... )
	local args = {...}
	if args[1]:match"^#[0-9a-fA-F]+$" then
		local openTag = "<color="..args[1]..">"
		local closeTag = "</color>"
		table.remove( args, 1 )
		local s = table.concat(args)
		args = {}
		for line in s:gmatch"[^\n]+" do
			table.insert( args, openTag..line..closeTag)
		end
	end
	-- if #PRINT>0 then
	-- 	table.insert(PRINT,"\n")
	-- end
	
	for i=1,#args do 
		table.insert(PRINT, tostring(args[i]))
	end
end

--more stuff missing in moonsharp...
if not pcall then
	pcall = function( f,... )
		return true, f( ... )
	end
end
---------------------------------------------------------------------------
local programs = {}
--linear algebra lib, vectors and matrices
local linalg = {}

-- functions added here are available to the compiled shader
local function _makeGlobals()
	local g = {}
	for name, val in pairs( math ) do
		if type(val) == "function" then
			g[ name ] = function(...)
				local thisFunc = name --debug
				local args = {...}
				for i=1, #args do
					args[i] = args[i].val
				end
				
				local returns = {val(table.unpack(args))}
				for i=1, #returns do
					if type( returns[i] ) ~= "table" or not returns[i].type then
						returns[i] = _wrapVal( returns[i] )
					end
				end
				return true, table.unpack( returns )
			end
		end
	end
	g.read_var = function(...) return true, read_var(...) end
	
	g.vec = function( ... )
		local vec = linalg.vec( ... )
		return true, vec
	end

	g.mat = function( ... )
		local mat = linalg.mat( ... )
		return true, mat
	end

	g.row = function( vec )
		return true, linalg.vecToRow( vec )
	end

	g.col = function( vec )
		return true, linalg.vecToCol( vec )
	end

	--co tangent
	g.cot = function( angleRadians )
		if angleRadians.type ~= "num" then
			error("Expected number, got "..angleRadians.type)
		end
		return true, _wrapVal( 1 / math.tan( angleRadians.val ), "num" )
	end

	g.print = function( val, color )
		if color then
			val,color = color,val.val
		end
		_printVal( "DBUG", val.name or "?", val, color )
		return true
	end

	g.rotateMatrix = function(...) return true, linalg.rotateMatrix(...) end
	g.len = function(...) return true, linalg.magnitude end
	g.normalize = function(...) return true, linalg.normalize end
	

	
	--TODO hsv rgb conversion
	--TODO wrap some math functions to work on vectors
	-- min, max
	-- function len( vec )

	return g
end
----------------------------------------------------------------------------
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

function linalg._emptyMatrix( rows, cols )
	return {type="mat"..rows.."_"..cols, rows=rows, cols=cols, val = {{}}}
end
function linalg._emptyVector( size )
	return {type="vec"..size, size=size, val = {}}
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
		if v.type=="num" then
			vec.val[n] = v.val
			n = n + 1
		elseif v.type:sub(1,3)=="vec" then
			for j = 1, v.size do
				vec.val[n] = v.val[j]
				n = n + 1
			end
		else
			error("Invalid use of vec function with type "..v.type)
		end
	end
	if n-1 <= 1 then error("vec(...) requires 2+ args") end
	vec.size = n-1
	vec.type = "vec"..vec.size
	return vec
end

function linalg.mat( ... )
	local val = {}
	local size;
	for i,vec in ipairs{...} do
		if vec.type:sub(1,3) ~="vec" then error("matrix construction expects vectors for rows") end
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
	if not (x.type:sub(1,3)=="mat" or y.type:sub(1,3)=="mat") then
		error("Compiler issue: at least one arg must be matrix",2)
	end
	
	local out = linalg._emptyMatrix( x.rows or y.rows, x.cols or y.cols )
	for r=1, out.rows do
		out[r] = {}
		for c=1, out.cols do
			local a = x.type:sub(1,3) == "mat" and x.val[r][c] or x.val
			local b = y.type:sub(1,3) == "mat" and y.val[r][c] or y.val
			out.val[r][c] = a-b
		end
	end
	return out
end

function linalg.addMat( x, y )
	return linalg.subMat(x, -y)
end

function linalg.subVec( x, y )
	if not (x.type:sub(1,3)=="vec" or y.type:sub(1,3)=="vec") then
		error("Compiler issue: at least one arg must be matrix",2)
	end

	out = linalg._emptyVector( x.size or y.size )
	for i=1,out.size do
		local a = x.type:sub(1,3) == "vec" and x.val[i] or x.val
		local b = y.type:sub(1,3) == "vec" and y.val[i] or y.val
		out.val[i] = a - b
	end
	return out
end

function linalg.addVec( x, y )
	return linalg.subVec( x, -y )
end

function linalg.matrixMult( matA, matB )
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
	return {type="num", val=sum}
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
		_nativePrint( ("%f * %f - %f * %f"):format(a.val[p1] , b.val[p2] , a.val[m1] , b.val[m2]) )
		out.val[i] = a.val[p1] * b.val[p2] - a.val[m1] * b.val[m2]
	end
	return out
end

 --https://math.stackexchange.com/a/4155115
 function linalg.newRotateMatrix( vec, amount )
	if amount.val == 0 then
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
		out.val[i] = vec.val[i] * scale.val
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
	local out = linalg._emptyMatrix( vec.size )
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
	swizzle = swizzle.val
	if #swizzle == 1 then
		local si = swizzleIndexes[swizzle]
		local v = _wrapVal( vec.val[ si ] )
		if vec.varName then 
			v.ref = {var=vec, index={si}}
		end
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
	linalg.stacks = nil
end

function linalg.toString( val )
	local v = tostring( val )
	local t = val.type

	if t:sub(1,3) == "vec" then
		local vec = val.val
		
		v = ("<%s>"):format( table.concat(vec,",") )
	elseif t:sub(1,3) == "mat" then
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
----------------------------------------------------------------------------
local types = {
	["void"]=true, --for functions
	["vec2"]=true,
	["vec3"]=true,
	["vec4"]=true,
	--["mat2_2"]=true, --uses swizzle indexing if dot -> .[row,col]
	
	["num"]=true,
	["str"]=true,
	["bool"]=true,
	["tbl"]=true,
	["tex4"]=true, --texture, string, but reading produces num/vec2/vec3/vec4
	["tex3"]=true, --color is in 0-1 space
	["tex2"]=true,
	["tex1"]=true,
	["func"]=true  --already in vars space, so why not :)
}
for r=2,4 do
	for c=2,4 do
		types[ ("mat%d_%d"):format(r,c) ] = true
	end
end
--highligher is buggy with quotes
local SINGLE_QUOTE = string.char(39)
local DOUBLE_QUOTE = string.char(34)
local patterns = {
	str = { "'^[^'\n]+'$", '^"[^"\n]+"$' },
	num = { 
		int="^[0-9]+$", 
		float="^[0-9]*%.[0-9]*$",
		hex="^0x[0-9a-fA-F]+$",
		bin="^0b[01]+$"
	},
	var = { "^[a-zA-Z_][a-zA-Z0-9_]*$" },
	op = { "^[()%{%}%[%]%%.%!%#%^%*%/%+%-%=%~%&|;,%<%>]+$" }
}


local keywords = {
	["if"] = "control",
	["else"] = "control",
	["elseif"] = "control",
	["while"] = "control",
	["for"] = "control",
	["true"] = "reserved",
	["false"] = "reserved",
	["continue"] = "control",
	["break"] = "control",
	["return"] = "control"
}
local ops = {
 --op, priority (highest first)
	--()
	["("] = -10,
	[")"] = -10, 
	["["] = -10,
	["]"] = -10,
	["{"] = -10,
	["}"] = -10,
	--access/swizzle
	["."] = 9,
	--not,len
	["!"] = 8,
	["#"] = 8,
	--math
	["^"] = 7, --exponent
	["**"] = 6, --cross
	["/"] = 6,
	["*"] = 6,
	["%"] = 6, --mult/dot
	["+"] = 5,
	["-"] = 5,
	--compare
	["=="] = 4,
	["!="] = 4,
	["~="] = 4,
	["<"] = 4,
	[">"] = 4,
	[">="] = 4,
	["<="] = 4,
	--logic
	["&&"] = 3,
	["||"] = 3,
	--assign
	["+="] = 1, ["-="] = 1, ["*="] = 1, ["/="] = 1, ["="] = 1,
	--non op
	[SINGLE_QUOTE] = false,
	[DOUBLE_QUOTE] = false, --quotes, highligher is buggy with '
	["//"] = false, -- comment
	[";"] = false, --line end
	["\n"] = false, --for debug line numbers
	[" "] = false, --prevent = = from being read as ==
	[","] = 0,
}

rightAssociate = {
	["^"] = true
}

function _isOp( text )
	return ops[ text ] ~= nil
end

function _chunkType( text )
	--if not text then return false end
	if text:match"[ \t\n\r]" and text~="\n" and not text:match"^['\"]" then
		return false
	elseif text=="\n" then
		return "line"
	elseif text:match"//.+" then 
		return false
	end
	for _, name in ipairs{ "op","num","var","str" } do
		local group = patterns[ name ]
		local txt =  text
		if name == "str" then
			--hide escaped quotes for testing
			txt = txt:gsub("\\'",""):gsub('\\"',"")
		end
		for _,pat in pairs( group ) do
			if txt:match( pat ) then
				return name
			end
		end
	end
	return false
end

unfinished = {}
taskQueue = {}
local function callYielding( resumeSelf, call )
	local nuf = #unfinished+1
	local rets = {call.func( table.unpack(call.args))}
	if rets[1] then
		return table.unpack( rets )
	end
	table.insert(unfinished, nuf, resumeSelf)
	return table.unpack( rets )
end

local function enqueueJob( label, func, ... )
	table.insert( taskQueue, {label=label, func=func, args={...}} )
end

local function yield( func,label, ... )
	if #unfinished > 20 then
		print("YIELD: "..label)
	end
	table.insert(unfinished, {
	--unfinished[1] = {
		func = func,
		label = label, --for debugging
		args = {...}
	})
end

local timeout = TIMEOUT
function autoYield( func, label, ... )
	timeout = timeout - 1
	if timeout <= 0 then
		yield( func, label, ... )
		return true
	end
	return false
end

function _progress( amount )
	PROGRESS = amount
end

function _tokenize( src, state )
	state = state or {}
	if state.tokenized then return true end
	state.area = state.area or {1,0}
	state.tokens = state.tokens or {}
	
	local inst = state.instructions
	local tokens = state.tokens
	
	local moreTokens = true
	--print( ">tokenize> "..state.area[1]..", "..state.area[2] )
	
	while state.area[2] <= #src do
		local area = state.area
		_progress(area[2]*100/#src)
		
		local chunk = src:sub( area[1], area[2] )
		local chunk2 = src:sub( area[1], area[2]+1 )
		
		--prevent bad op merging
		--ex y=-4 being =- or () not being split
		local chunkType2 = _chunkType( chunk2 )
		if chunkType2 == "op" and #chunk == 1 then
			chunkType2 = ops[ chunk2 ] ~= nil
		end
		if not chunkType2
		  and (chunk2:sub(1,1) == '"' or chunk2:sub(1,1) == "'")
			and _chunkType(chunk or "")~="str" then
				chunkType2 ="unfinished_str"
 		end
		if not chunkType2 then --whitespace, end token
			if #chunk > 0 then
				table.insert( tokens, chunk )
				area[1] = area[2] + 1 --symbol that broke the chunk
			else --prob whitespace
				area[1] = area[1]+1
			end
			area[2] = area[1] - 1 --empty selection
		else
			area[2] = area[2]+1 --was still valid
		end
		if autoYield( _tokenize,"_tokenize", src, state ) then return false end
	end
	local chunk = src:sub( state.area[1], state.area[2] )
	if #chunk > 0 then --whitespace or chunk
		if _chunkType( chunk ) then
			table.insert( state.tokens, chunk )
		end
	end
	state.tokenized = true
	return true
end

function _computeLineNums( state, index, lineNum, comment )
	state._computeLines = state._computeLines or {
		comment = false,
		lineNum = 1,
		index = 1
	}
	local tokens = state.tokens
	state.lineNums = state.lineNums or {}
	index = state._computeLines.index
	
	while tokens[ state._computeLines.index ] do
		_progress( state._computeLines.index * 100 / #tokens )
		local token  = tokens[state._computeLines.index]
		
		if token == "\n" then
			state._computeLines.comment = false
			state._computeLines.lineNum = state._computeLines.lineNum + 1
			table.remove( tokens, state._computeLines.index )
		elseif state._computeLines.comment or token == "//" then
			state._computeLines.comment = true
			table.remove( tokens, state._computeLines.index )
		else
			state.lineNums[ state._computeLines.index ] = state._computeLines.lineNum
			state._computeLines.index = state._computeLines.index + 1
		end
		
		if autoYield( _computeLineNums,"_computeLineNums", state ) then 
			return false
		end
	end
	return true
end

local function cerr( line, msg )
	error(("Compile error: #%d: %s"):format(line, msg),2)
end

local function rerr( line, msg )
	error(("Runtime error: #%d: %s"):format(line, msg),2)
end

local function _closePar( openPar )
	return (  { ["("]=")", ["["]="]", ["{"]="}" }) [ openPar ]
end

--list of instructions or false to yield
function _evalInstr( state, first, last, resume )
	state._evalInst = state._evalInst or {
		i = first,
		out = {},
		stack = {}
	}
	local tokens = state.tokens
	local i = state._evalInst.i
	--last or nil
	local out = state._evalInst.out
	local stack = state._evalInst
	while tokens[i] and tokens[ i ] ~= ";" and (type(last)~="number" or i <= last) do
		--print(" evi: "..i.." | "..tokens[i])
		local t0 = i-1 >= first and tokens[ i-1 ]
		local t1 = tokens[ i ]
		local t2 = tokens[ i+1 ]
		local chunkType1 = _chunkType( t1 )
		local lineNum = state.lineNums[ i ]


		if chunkType1 == "var" then
			if ("([{"):find(t2,1,true) then --call
				table.insert( stack, {
					op="CALL",
					val=t1, 
					openWith=t2, 
					closeWith=_closePar(t2),
					lvl = ops[t2]
				})
				i = i+1 --another +1 at bottom of loop
				table.insert( out, {op="CALL_END"})
			else --var
				table.insert( out, {op="var", val=t1} )
			end

		elseif t1 == "(" or t1 == "{" or t1 == "[" then
			table.insert( 
				stack, {
					op=t1, 
					closeWith = _closePar (t1),
					lvl = ops[ t1 ]
				}
			)

		elseif t1 == ")" or t1 == "]" or t1 == "}" then
			while #stack > 0
			  and stack[#stack].closeWith ~= t1 do
				local pop = table.remove( stack )
				-- pop.closeWith = nil --hide, not needed anymore
				table.insert( out, pop )	
			end
			if stack[#stack].op=="CALL" then
				table.insert( out, table.remove( stack ) ) --move call
			else
				table.remove( stack ) --don't need )
			end
		

		elseif chunkType1 == "num" then
			local n
			if t1:match( patterns.num.bin ) then
				n = tonumber( t1:sub(2), 2 ) --0b
			else
				n = tonumber( t1 ) --handles 0x already :)
			end
			if not n then
				cerr( lineNum )
			end
			table.insert( out, {op="val", val = _wrapVal( n )  } )

		elseif chunkType1 == "str" then
			table.insert( out, {op="val", val=_wrapVal(t1:sub(2,-2):gsub('\\"','"'):gsub("\\'","'"), "str") } )

		elseif t1 == "-" and ( not t0 or _chunkType(t0)=="op" ) then
			-- negate
			table.insert( out, {op="negate"}) --no lvl needed, comparison only against stack

		elseif t1 == "," then
			while #stack > 0 and stack[#stack].op~="CALL" do
				local pop = table.remove( stack )
				if pop.closeWith then
					cerr( state.lineNums[first], "Missing "..pop.closeWith )
				end
				table.insert( out, pop )
			end

		else
			local lvl = ops[ t1 ]
			local stackOp = stack[#stack]

			if rightAssociate[ t1 ] then
				lvl = lvl + .5
			end

			while #stack > 0 
			  and lvl < stack[#stack].lvl
			  and not stack[#stack].closeWith do
				local pop = table.remove( stack )
				-- x.closeWith = nil
				table.insert( out, pop )
			end

			table.insert( stack, {op=t1, lvl=ops[ t1 ] } )
		end

		i = i+1
		state._evalInst.i = i
		--or false is important to fix a bug when running with moonsharp...
		if autoYield( _evalInstr,"_evalInstr", state, first, last or false, true ) then return end
	end


	while #stack > 0 do
		local pop = table.remove( stack )
		if pop.closeWith then
			cerr( state.lineNums[first], "Missing "..pop.closeWith )
		end
		table.insert( out, pop )
	end
	
	if not resume then
		state._evalInst = nil
		
	end

	return {
		op="EVAL",
		postfix = out
	}, i

end

local function _findClosingPar( state, i, par, fromResume )
	state.findClosing = state.findClosing or {
		lvl = 0
	}
	par = par or ")"
	lvl = lvl or 0
	while state.tokens[ i ] and ( lvl > 0 or state.tokens[ i ] ~= par) do
		local t = state.tokens[i]
		if ("([{"):find(t,1,true) then
			state.findClosing.lvl  = state.findClosing.lvl+1
		elseif (")]}"):find(t,1,true) then
			state.findClosing.lvl = state.findClosing.lvl-1
		end
		if autoYield( _findClosingPar,"_findClosingPar", state, i, par, true ) then return false end
	end
	if not fromResume then
		state.findClosing = nil
	end
	return not not state.tokens[ i ], i
end

function _buildInstructions( state )
	local index = state.buildIndex or 1
	local inst = 
		state.inFunc and state.functions[state.inFunc].instructions 
		or state.instructions --{{line1},{line2},...}
	local ctrlStack = state.ctrlStack or {}
	local scope = state.scope
	local tokens = state.tokens
	
	while tokens[index] do
		_progress( index*100 / #tokens )
		local lineNum = state.lineNums[ index ]
		local t1 = tokens[index]
		local t2,t3,t4 = tokens[ index + 1 ], tokens[ index + 2 ], tokens[index + 3]
		local i2,i3,i4,i5 = index+1, index+2, index+3, index+4

		--if t1 then print("BuildInst: "..t1) end

		if t1 == "{" then
			state.scope = state.scope+1
			table.insert( inst, {op="SCOPE_UP",line=lineNum})
			index = i2

		elseif t1 == "in" then
			if state.inFunc then
				cerr(lineNum,"input must be declared outside function")
			end
			if not types[ t2 ] then 
				cerr(lineNum, "expected type")
			end
			table.insert( inst, {
				op="DECLARE", 
				type=t2, 
				name=t3, 
				scope=state.scope, 
				line=lineNum,
				stepIndex = #inst+1
			} )
			table.insert( inst, {
				op="INPUT", 
				name=t3, 
				scope=state.scope, 
				line=lineNum,
				stepIndex = #inst+1
			} )
			if t4 == ";" then
				index =i5
			else
				cerr(lineNum, "expected ;")
			end
			
		elseif t1 == "out" then
			if state.inFunc then
				cerr(lineNum,"output must be declared outside function")
			end
			if not types[ t2 ] then 
				cerr(lineNum, "expected type")
			end
			table.insert( inst, {
				op="DECLARE", 
				type=t2, 
				name=t3, 
				scope=state.scope, 
				line=lineNum,
				stepIndex = #inst+1
			} )
			table.insert( state.outputs, { type=t2, name=t3 } )
			if t4 == ";" then
				index = i5
			elseif t4 == "=" then
				index = i3 --var name
			end
			
		elseif types[ t1 ] then --type or function declaration
			if t3 == "(" then --function (or indexing)
				local params = {}
				local i = i4
				while tokens[i] == ")" or #params <= 32 do
					if tokens[i] == ")" then
						i = i+1
						break
					elseif tokens[i] == "," then
						if #params == 0 then 
							cerr(state.lineNums[i],"expected type or )")
						end
						i = i+1
					elseif not types[tokens[i]] then
						cerr(state.lineNums[i], "param type or ) expected")
					elseif _isOp(tokens[i+1]) then
						cerr(state.lineNums[i], "param name expected")
					else
						table.insert( params, {type = tokens[i], name=tokens[i+1]})
						i = i+2
						if tokens[i] ~= ")" and tokens[i] ~= "," then
							cerr( state.lineNums[i], "expected , or )")
						end
					end
				end
				local name = t2
				if #params > 32 then
					cerr(lineNum, name..", too many params")
				end
				if state.inFunc then
					cerr( lineNum, "nested function not allowed" )
				end
				if state.scope > 1 then
					cerr( lineNum, "func "..name.." was declared in a block")
				end
				state.inFunc = name
				state.functions[ name ] = {
					returnType=t1, 
					name = t2, 
					params = params,
					instructions = {},
					line=lineNum
				}
				--forward instructions to function
				inst = state.functions[ name ].instructions

				if tokens[ i ] ~= "{" then
					cerr(state.lineNums[i],"expected {")
				end
				
				index = i

			else -- regular var, no in/out
				table.insert( inst, {
					op="DECLARE", 
					type=t1, 
					name=t2, 
					scope=state.scope,
					func = state.inFunc,
					line=lineNum,
					stepIndex = #inst+1
				})
				if t3 == ";" then
					index = i4
				elseif t3 == "=" then
					index = i2  -- var name
				else
					cerr(lineNum,"expected ; or =")
				end
			end

		elseif keywords[t1] then
			if t1 == "break" or t1 == "continue" then
				local loopInst;
				for z=state.scope, 1, -1 do
					local op = ctrlStack[z].op
					if op == "FOR" or op == "WHILE" then
						loopInst = op
					end
				end
				if not loopInst then
					cerr( lineNum, "break/continue not in loop")
				end
				table.insert( inst, {
					op = t1:upper(),
					scope = state.scope,
					line = lineNum,
					loopInst = loopInst,
					stepIndex = #inst+1
				})
			elseif t1 == "return" then
				local eval,lastToken = _evalInstr( state, i2 )
				if eval then
					eval.op = "RETURN"
					eval.stepIndex = #inst+1
					eval.line = lineNum
					table.insert( inst, eval )
					index = lastToken + 1
				end --else needs more itteration

			elseif t1 == "if" or t1 == "while" or t1 == "elseif" then
				if t2 ~= "(" then 
					cerr(lineNum,"expected ( for if/while")
				end
				if not state._at then
					local done, at = callYielding({
						func=_buildInstructions,
						label="_buildInstructions-for",
						args={state,index}
					},{
						func = _findClosingPar,
						args = { state, i2, ")" }
					})

					if done then
						state._at = at	
					end
				end
				if state._at then
					local eval = _evalInstr( state, index, state._at )
					if eval then
						eval.op = t1:upper()
						eval.stepIndex = #inst+1
						eval.line = lineNum
						table.insert( inst, eval )
						ctrlStack[ state.scope+1 ] = eval --when scope ends, set skip point
						index = state._at + 1
						state._at = nil
					end
				end

			elseif t1 == "for" then
				if t2 ~= "(" then 
					cerr(lineNum,"expected ( after for")
				end
				if not state._at then
					local done, at = callYielding({
						func=_buildInstructions,
						label="_buildInstructions-for",
						args={state,index}
					},{
						func = _findClosingPar,
						args = { state, i2, ")" }
					})

					if done then
						state._at = at	
					end
				end
				if state._at then
					state._for = state._for or {}
					state._i = state._i or i3
					for z=1,3 do
						if not state._for[z] then
							local eval,last = _evalInstr( state, state._i, state._at-1 )
							if eval then
								state._i = last+1
								state._for[z] = eval.postfix
							end
						end
					end
					if state._for[3] then
						table.insert( inst, {
							op = "FOR",
							scope = state.scope,
							line = lineNum,
							init = state._for[1],
							test = state._for[2],
							inc  = state._for[3],
							stepIndex = #inst+1
						})
						ctrlStack[ state.scope+1 ] = inst[#inst] --when scope ends, set skip point
						state._at = nil
						state._for = nil
						state._i = nil
					end
				end
			end

		elseif t1 == "}" then
			if ctrlStack[ state.scope ] then
				ctrlStack[ state.scope ].skip = #inst+1
			end
			state.scope = state.scope - 1
			if state.scope < 1 then
				cerr(lineNum,"too many }");
			end
			table.insert( inst, {op="SCOPE_DOWN", line=lineNum})
			if state.scope == 1 then
				state.inFunc = false
				inst = state.instructions
			end
			index = i2

		else --statment (variable/function call)
			local endWith = false
			-- if (not t2:match"[=-/*]]?=") and t2 ~= "(" then
			-- 	cerr(lineNum,"expected assignment or call")
			-- end
			local eval,lastToken = callYielding({
				func=_buildInstructions,
				label="_buildInstructions",
				args = {state, index}
			},{
				func=_evalInstr,
				args={state, index, false}
			})
			
			if eval then
				eval.stepIndex = #inst+1
				eval.line = lineNum
				table.insert( inst, eval )
				index = lastToken + 1
			else --else needs more itteration
				return false
			end
		end
		
		state.buildIndex = index
		if autoYield( _buildInstructions,"_buildInstructions", state, true ) then return false end
	end
	return true
end

function _compile( src, prgmName, main, state )
	if not state then print"> tokenize" end
	state = state or {
		instructions = {}, --global
		functions = {}, --like instructions, name={instr...}
		tokens = {},
		stack = {},
		inFunc = false, --nested functions prohibited in shader code
		scope = 1, --block level
		outputs = {},
		needsToken = false,
		step = "tokenize",
		main = main,
		line = 1
	}
	
	if state.step == "tokenize" then
		if callYielding( { --just imagine... coroutine.yield()
			func=_compile,
			label="_compile-tokenize",
			args={src,prgmName,main,state}
		}, {
			func=_tokenize,
			args={src, state}
		} ) then
			state.step = "computeLines"
			print"> computeLines"
		else
			return false
		end
		
	end
	
	if state.step == "computeLines" then
		if callYielding( {
			func=_compile,
			label="_compile-computeLines",
			args={src,prgmName,main,state}
		},{
			func=_computeLineNums,
			args={ state }
		}) then
			state.step = "buildInstructions"
			print"> buildInstructs"
		else
			return false
		end
	end
	
	if state.step == "buildInstructions" then
		if callYielding({
			func=_compile,
			label="_compile-computeLines",
			args={src,prgmName,main,state}
		},{
			func=_buildInstructions,
			args = {state}
		}) then
			programs[ prgmName ] = state
			print("> "..prgmName.." compiled")
			return true
		else
			return false
		end
	end
end


----------------------------------------------------------------------------
-- Running
-- heavy lifting done by _evaluate and _doStep
----------------------------------------------------------------------------

function _typeCheck( value, typeName, varName, lineNum )
	--TODO type checking
	if value.type ~= typeName then
 		rerr( lineNum, ("assignment to %s '%s' with type %s"):format(typeName, varName, value.type) )
	end
	return value
end

function _truthy( prgmVal )
	return not (
		prgmVal.val == false
		or prgm.val == 0
	)
end

function _wrapVal( val, typeName )
	if typeName then
		local v = { val=val, type=typeName }
		if typeName:sub(1,3) == "vec" then
			v.size = tonumber( typeName:sub(4) )
		elseif typeName:sub(1,3) == "mat" then
			v.rows = tonumber( typeName:match"^mat([0-9]+)" )
			v.cols = tonumber( typeName:match"_([0-9]+)^" )
		end
		return v
	end
	local t = "?"
	if type(val) == "string" then
		return _wrapVal( val, "str" )

	elseif type(val) == "number" then
		return _wrapVal( val, "num" )

	elseif type(val) == "function" then
		return _wrapVal( val, "func" )

	elseif type(val) == "boolean" then
		return _wrapVal( val, "bool" )

	else
		error("Missing type wrapper for "..type(val))
	end
end

--includeUpScope false on function call
function _insertCallStack( cs, includeUpScope, fName )
	local global = cs[1]

	local vars = {}
	--setmetatable() oof, not available

	local new = {
		vars = vars,
		parent = cs[#cs],
		onEndBlock = false,
	}
	if not includeUpScope then
		new.stepIndex = 1
		--call info is stored in parent because `new` will be popped off stack when done
		new.parent.callInfo = {
			returnVals = false,
		}
	end
	--checks self
	--optionally checks parent
	--checks global
	new.get = function( x )
		return vars[x] 
		  or (includeUpScope and new.parent and new.parent.get( x ))
		  or global.vars[x]
	end
	new.getStepIndex = function()
		return new.stepIndex or new.parent and new.parent.getStepIndex()
	end
	new.setStepIndex = function( i )
		if new.stepIndex then
			new.stepIndex = i
		else
			new.parent.setStepIndex( i )
		end
	end

	new.fName = fName or new.parent and new.parent.fName
	

	--not needed for global, no return values in global
	--removed, handled by yield
	-- new.setCallContinue = function( stuff )
	-- 	if not new.callInfo then
	-- 		if not new.parent then
	-- 			error("Nowhere to set call continue")
	-- 		end
	-- 		return new.parent.setCallContinue( stuff )
	-- 	else
	-- 		new.callInfo.continue = { stuff }
	-- 	end
	-- end

	new.setCallReturn = function( ... )
		if not new.callInfo then
			if not new.parent then
				error("Nowhere to set call return")
			end
			return new.parent.setCallReturn( ... )
		else
			new.callInfo.returnVals = { ... }
		end
	end

	table.insert( cs, new )

	--should only be cleared when processing the result in evaluate->call, so the current stack will have the info
	-- new.clearCallInfo = function()
	-- 	if not new.callInfo then
	-- 		if not new.parent then
	-- 			error("Nowhere to clear call info")
	-- 		end
	-- 		return new.parent.clearCallInfo( stuff )
	-- 	else
	-- 		new.callInfo = nil
	-- 	end
	-- end
end

local evalOps = {
	add = function( a, b, lineNum )
		local c;
		if a.type=="str" or b.type=="str" then
			c = _wrapVal( _toString(a) .. _toString(b) )
		elseif a.type:sub(1,3) == "vec" or b.type:sub(1,3)=="vec" then
			c = linalg.addVec(a, b)
		elseif a.type:sub(1,3) == "mat" or b.type:sub(1,3) == "mat" then
			c = linalg.addMat( a, b )
		elseif a.type=="num" and b.type=="num" then
			c = {type="num", val=b.val + a.val}
		else
			rerr(lineNum, ("%s + %s not implemented"))
		end
		return c
	end,

	sub = function( a,b, lineNum )
		local c;
		if a.type:sub(1,3) == "vec" or b.type:sub(1,3)=="vec" then
			c = linalg.subVec(a, b)
		elseif a.type:sub(1,3) == "mat" or b.type:sub(1,3) == "mat" then
			c = linalg.subMat( a, b )
		elseif a.type=="num" and b.type=="num" then
			c = {type="num", val=b.val-a.val}
		else
			rerr(lineNum, ("%s - %s not implemented"))
		end
		return c
	end,

	mult = function( a,b, lineNum )
		local c;
		if a.type:sub(1,3) == "vec" and b.type:sub(1,3)=="vec" then
			c = linalg.dot(a, b)
		elseif a.type:sub(1,3) == "vec" and b.type == "num" then
			c = linalg.scaleVec( a, b )
		elseif a.type == "num" and b.type:sub(1,3) == "vec" then
			c = linalg.scaleVec( b, a )
		elseif a.type:sub(1,3) == "mat" or b.type:sub(1,3) == "mat" then
			if a.type=="num" then
				c = linalg.scaleMatrix( b, a )
			elseif b.type =="num" then
				c = linalg.scaleMatrix( a, b )
			elseif a.type:sub(1,3) == "mat" and b.type:sub(1,3) == "mat" then
				c = linalg.matrixMult( a, b )
			elseif a.type:sub(1,3) == "mat" and b.type:sub(1,3) == "vec" then
				c = linalg.colToVec( linalg.matrixMult( a, linalg.vecToCol(b)), 1 )
			elseif a.type:sub(1,3) == "vec" and b.type:sub(1,3) == "mat" then
				c = linalg.rowToVec( linalg.matrixMult( linalg.vecToRow(a), b ), 1 )
			else
				rerr(lineNum, "can't multiply "..a.type.." with "..b.type)
			end
		elseif a.type=="num" and b.type=="num" then
			c = {type="num", val=b.val * a.val}
		else
			rerr(lineNum, ("%s * %s not implemented"))
		end
		return c
	end,

	div = function( a,b, lineNum )
		local c;
		if a.type:sub(1,3) == "vec" and b.type == "num" then
			c = linalg.scaleVec( a, 1/b )
		elseif a.type == "num" and b.type:sub(1,3) == "vec" then
			c = linalg.vecUnder( b, a )
		elseif a.type:sub(1,3) == "mat" or b.type:sub(1,3) == "mat" then
			if a.type=="num" then 
				c = linalg.matrixUnder( b, a )
			elseif b.type =="num" then
				c = linalg.scaleMatrix( a, 1/b )
			-- no matrix divide matrix
			-- elseif a.type:sub(1,3) == "mat" and b.type:sub(1,3) == "mat" then
			-- 	c = linalg.matrixMult( a, b )
			else
				rerr(lineNum, "can't divide "..a.type.." with "..b.type)
			end
		elseif a.type=="num" and b.type=="num" then
			c = {type="num", val=b.val / a.val}
		else
			rerr(lineNum, ("%s / %s not implemented"))
		end
		return c
	end,

	assign = function( a, val, lineNum )
		if a.ref then
			-- if a.ref.var.type:sub(1,3)=="vec" then
			-- 	local valIsVec = val.type:sub(1,3)=="vec"
			-- 	if not valIsVec and not val.type=="num" then rerr(lineNum,"can't assign to vec with "..val.type) end
			-- 	for i=1,#a.ref.path do
			-- 		local s = a.ref.path:sub(1,1)
			-- 		local si = swizzleIndexes[ s ]
			-- 		if si > a.size then rerr(lineNum,"swizzle assignment out of bounds") end
			-- 		a.ref.var.val[ si ] = valIsVec and val.val[si] or val.val
			-- 		if a.type=="num" then
			-- 			a.val = a.ref.var.val[si]
			-- 		else
			-- 			a.val[si] = a.ref.var.val[si]
			-- 		end
			-- 	end
			-- end
		
			local x = a.ref.var.val
			local index = a.ref.index
			for i=1,#index-1 do
				x = x[a.ref.index[i]]
			end
			if val.type~="num" then rerr(lineNum,"can't assign "..val.type.." to "..a.type.." cell") end
			x[ index[#index] ] = val.val
		end
		a.val = val.val
	end,

	eq = function( a, b, lineNum )
		if a.type ~= b.type then
			return false

		elseif a.type == "num" then
			return a.val == b.val

		elseif a.type:sub(1,3) == "vec" then
			return linalg.vecEquals( a, b )

		elseif a.type:sub(1,3) == "mat" then
			return linalg.matEquals( a, b )

		elseif a.type:sub(1,3) == "tex" then
			return a.val == b.val --that could be a lot of text...

		elseif a.type == "bool" then
			return a.val == b.val

		elseif a.type == "str" then
			return a.val == b.val

		end
		error("== not implemented for type "..a.type)
	end,

	lt = function( a, b, lineNum )
		if a.type ~= b.type or a.type:sub(1,3) == "mat" or a.type:sub(1,3) == "tex" then
			rerr(lineNum, "can preform comparison with type "..a.type.." and "..b.type)
		elseif a.type == "num" then
			return a.val < b.val

		elseif a.type:sub(1,3) == "vec" then
			return linalg.magnitude( a ) < linalg.magnitude( b )

		elseif a.type == "bool" then
			return (a and 1 or 0) < (b and 1 or 0)

		elseif a.type == "str" then
			return a.val < b.val

		end
		error("< not implemented for type "..a.type)
	end,

	gt = function( a, b, lineNum )
		return (not evalOps.lt(a,b,lineNum) and (not evalOps.eq(a,b,lineNum)))
	end,

	lte = function( a, b, lineNum )
		return evalOps.lt(a,b,lineNum) or evalOps.eq(a,b,lineNum)
	end,

	gte = function( a, b, lineNum )
		return evalOps.gt(a,b,lineNum) or evalOps.eq(a,b,lineNum)
	end
}

--@yielding
function _evaluate( postfix, prgmState, resume )
	local env = prgmState.callStack[#prgmState.callStack]
	local i = env._eval_index or 1
	env.stack = env.stack or {}
	env.callMarkers = env.callMarkers or {}
	local callMarkers = env.callMarkers
	local stack = env.stack

	-- if not resume then
	-- 	print("POSTFIX")
	-- 	for j=1,#postfix do
	-- 		print( i..":"..j.." "..postfix[j].op.." "..tostring(postfix[j].val))
	-- 	end
	-- end

	local lineNum = _getInstructions( prgmState )[ env.getStepIndex() ].line
	while i <=#postfix do
		local step = postfix[i]
		if step.op == "val" then
			table.insert( stack, step.val)
		elseif postfix[i+1] and postfix[i+1].op=="." and step.op=="var" then
			table.insert( stack, {type="index", val=step.val})
		elseif step.op == "var" then
			local x = env.get(step.val)
			x.varName = step.val
			table.insert( stack, x )
		elseif step.op == "-" then
			local b = table.remove( stack )
			local a = table.remove( stack )
			local c = evalOps.sub( a,b,lineNum );
			
			table.insert( stack, c )
		elseif step.op == "+" then
			local b = table.remove( stack )
			local a = table.remove( stack )
			local c = evalOps.add(a,b,lineNum)
			
			table.insert( stack, c )
		elseif step.op == "*" then
			local b = table.remove( stack )
			local a = table.remove( stack )
			local c = evalOps.mult( a,b,lineNum );
			
			table.insert( stack, c )
		elseif step.op == "**" then
			local b = table.remove( stack )
			local a = table.remove( stack )
			local c;
			if a.type:sub(1,3) == "vec" and b.type:sub(1,3) == "vec" then
				c = linalg.cross( a, b )
			else
				rerr(lineNum, "cant do cross product on "..a.type.." and "..b.type)
			end
			table.insert( stack, c )
		elseif step.op == "/" then
			local b = table.remove( stack )
			local a = table.remove( stack )
			local c = evalOps.div( a,b,lineNum );
			
			table.insert( stack, c )
		elseif step.op == "==" then
			local b = table.remove( stack )
			local a = table.remove( stack )
			local c = evalOps.eq( a,b,lineNum );
			
			table.insert( stack, c )
		elseif step.op == "~=" or step.op == "!=" then
			local b = table.remove( stack )
			local a = table.remove( stack )
			local c;
			error"not implemented"
		elseif step.op == ">" then
			local b = table.remove( stack )
			local a = table.remove( stack )
			local c evalOps.gt( a, b, lineNum )
			table.insert( stack, c )
			
		elseif step.op == "<" then
			local b = table.remove( stack )
			local a = table.remove( stack )
			local c = evalOps.lt( a, b, lineNum )

			table.insert( stack, c )
		elseif step.op == ">=" then
			local b = table.remove( stack )
			local a = table.remove( stack )
			local c = evalOps.gte( a, b, lineNum )
			table.insert( stack, c )

		elseif step.op == "<=" then
			local b = table.remove( stack )
			local a = table.remove( stack )
			local c = evalOps.lte( a, b, lineNum )
			table.insert( stack, c )
			
		elseif step.op == "=" then --copy ref/assign
			local b = table.remove( stack )
			local a = stack[#stack]
			evalOps.assign( a, b )

		elseif step.op == "+=" then
			local b = table.remove( stack )
			local a = stack[#stack]
			local c = evalOps.add( a, b, lineNum )
			evalOps.assign( a, c )
			
		elseif step.op == "-=" then
			local b = table.remove( stack )
			local a = stack[#stack]
			local c = evalOps.sub( a, b, lineNum )
			evalOps.assign( a, c )

		elseif step.op == "/=" then
			local b = table.remove( stack )
			local a = stack[#stack]
			local c = evalOps.div( a, b, lineNum )
			evalOps.assign( a, c )

		elseif step.op == "*=" then
			local b = table.remove( stack )
			local a = stack[#stack]
			local c = evalOps.mult( a, b, lineNum )
			evalOps.assign( a, c )

		elseif step.op == "." then
			local b = table.remove( stack )
			local a = table.remove( stack )
			local c;
			if a.type:sub(1,3) == "vec" then
				c = linalg.vecSwizzle( a, b )
			else
				error("swizzle/.indexing not implemented for type "..a.type)
			end
			table.insert( stack, c )
		elseif step.op == "CALL_END" then
			table.insert(callMarkers, #stack+1) --last arg of function call
			--print("EOC marked")
		elseif step.op == "CALL" then ----------------------------------------------------------todo () type or [] type
			if step.openWith == "[" then --actually indexing, not a call
				local mark = table.remove( callMarkers )
				if not mark then
					rerr( lineNum, "Compiler issue: missing end of call args op")
				end
				-- local args = {}
				local valName = step.val
				local var = env.get( valName )
				local val = var.val
				local args = {}
				while #stack >= mark do
					local v = table.remove( stack )
					if not v then rerr(lineNum, "Can't use type "..v.type.." for indexing") end
					table.insert( args, 1, v.val)
				end

				if var.type=="str" then
					if #args > 2 then rerr( lineNum, "Incorrect usage of substring indexing" ) end
					val = _wrapVal( val:sub( args[1],args[2] ) )
				else
					for i=1,#args do
						val = val[ args[i] ]
					end
					val = _wrapVal( val, type(val)=="table" and ("vec"..#val) or nil )
				end
				val.ref = {var=var,index = args}
				table.insert( stack, val )
			else --regular call
				--if already done (previously called async)
				if env.callInfo and env.callInfo.returnVals then
						table.insert( stack, env.callInfo.returnVals[1] )
						env.callInfo = nil
				elseif env.callInfo then
						return false --continuing is handled by yield system
				else
					--print("#CM: "..#callMarkers)
					local mark = callMarkers[#callMarkers]
					if not mark then
						rerr( lineNum, "Compiler issue: missing end of call args op")
					end
					local args = {}
					local fName = step.val
					while #stack >= mark do
						table.insert( args, 1, table.remove( stack ))
					end
					
					--actual call
					local done, c = env.get( fName )( table.unpack(args) )

					if not done then --100% of shader functions 
						return false --needs continuation, handled in caller
					end
					table.remove(callMarkers)
					if c then
						if not c.type then
							local t;
							if type(c) == "number" then
								t = "num"
							elseif type(c)=="string" then
								t = "str"
							elseif type(c)=="boolean" then
								t = "bool"
							else
								error("Function err: missing type tag from call result on line "..lineNum.."for type "..type(c))
							end
							c = {type=t,val=c}
						end
						table.insert( stack, c )
					end
				end
			end

		elseif step.op == "&&" then
			local b = table.remove( stack )
			local a = table.remove( stack )
			error"not implemented"
		elseif step.op == "||" then
			local b = table.remove( stack )
			local a = table.remove( stack )
			error"not implemented"
		else
			error"not implemented"
		end

		i = i+1
		env._eval_index = i
		if autoYield( _evaluate,"_evaluate", postfix, prgmState, true ) then 
			print"yielding in eval"
			return false 
		end
	end
	if not resume then
		env.stack = {}
		env.callMarkers = {}
		env._eval_index = nil
	end
	return true, table.unpack( stack )
end

function _toString( x )
	local str = tostring( x.val )
	if x.type:sub(1,3)=="vec" or x.type:sub(1,3)=="mat" then
		str = linalg.toString( x )
	end
	return str
end
function _printVal( note, name, x, color )
	if not x then x = {type="void"} end
	local str = _toString( x )
	if color then
		print(color, ("%s: %s %s = %s"):format(note, name, x.type, str))
	else
		print(("%s: %s %s = %s"):format(note, name, x.type, str))
	end
end

function _getInstructions( prgmState )
	local callStack = prgmState.callStack[ #prgmState.callStack ]
	return  prgmState.prgm.functions[ callStack.fName or prgmState.func ] and  prgmState.prgm.functions[ callStack.fName or prgmState.func ].instructions or prgmState.inst
end

--@yielding
function _doStep( inputs, prgmState )
	--DECLARE, INPUT, EVAL, IF, FOR, WHILE, RETURN, BREAK, CONTINUE
	--do not use callYielding for evaluation here
	--use a normal call and return true,lineNum / false
	--otherwise program can get stuck in a 3 phase loop (see "EVAL")
	
	local stepIndex = prgmState.callStack[ #prgmState.callStack ].getStepIndex()
	local globals = prgmState.callStack[1].vars
	local callStack = prgmState.callStack[ #prgmState.callStack ]
	local steps = _getInstructions( prgmState )
	local step = steps[ stepIndex ]
	local vars = callStack.vars
	
	--less spam
	if not _WAS_EVAL then
		print("_doStep:"..step.op)
	end
	_WAS_EVAL = step.op=="EVAL"

	if step.op == "DECLARE" then
		if vars[ step.name ] then
			rerr( step.line, "var "..step.name.." is already defined in this scope")
		else
			local vt = step.type
			local t = vt:sub(1,3)
			if t == "vec" then
				vars[ step.name ] = linalg.newVector( tonumber( vt:sub(4) ) )
			elseif t=="mat" then
				local rows = tonumber( vt:match"^mat([0-9]+)" )
				local cols = tonumber( vt:match"_([0-9]+)$" )
				vars[ step.name ] = linalg.newMatrix( rows, cols )
			-- elseif t=="tex" then
			else
				vars[ step.name ] = {type=step.type, name=step.name} --val=nil
			end
		end
		return true, stepIndex + 1

	elseif step.op == "INPUT" then
		if not globals[step.name] then
			rerr( step.line, "var "..step.name.." wasn't declared yet");
		end
		local ty = globals[ step.name ].type
		local v = vars[ step.name ]
		
		if not inputs[ step.name ] then rerr( step.line, "Missing input '"..step.name.."'") end
		_printVal( "IN", step.name, inputs[ step.name ])
		v.val = _copyVar( _typeCheck( inputs[ step.name ], ty, step.name, step.line ) ).val
		
		-- if ty:sub(1,3) == "vec" then
		-- 	v.size = #v.val
		-- 	if v.size ~= tonumber( ty:match("[0-9]+$") ) then
		-- 		rerr( step.line, "incorrect vector size used for input")
		-- 	end
		-- elseif ty:sub(1,3)=="mat" then
		-- 	v.rows = #v.val
		-- 	v.cols = #v.val[1]
		-- 	if v.rows ~= tonumber(ty:match"^mat([0-9]+)_") or
		-- 	v.cols ~= tonumber(ty:match"([0-9]+)$") then
		-- 		rerr( step.line, "incorrect matrix size used for input")
		-- 	end
		-- end
		
		return true,stepIndex + 1

	elseif step.op == "EVAL" then
		--do not use callYielding, causes 3 phase loop
		--Eval, yields, resumes evaluate, resumes doStep, clears result, 
		--returned value not used
		--prgm loop runs this section expecting a result and instead starts a new eval
		local done = _evaluate(step.postfix, prgmState)
		
		return done, stepIndex + (done and 1 or 0)
		
	elseif step.op == "SCOPE_UP" then
		_insertCallStack( prgmState.callStack, true )

	elseif step.op == "SCOPE_DOWN" then
		if callStack.onEndBlock then
			local done, to = callStack.onEndBlock()
			if not done then return false, stepIndex end
			table.remove( prgmState.callStack ) --pop
			return true, to
		else
			table.remove( prgmState.callStack ) --pop
		end

	elseif step.op == "IF" then
		local done, val = _evaluate( step.postfix, prgmState )
		
		if not done then return false, stepIndex end

		local truthy = _truthy( val )
		step.ran = truthy --used by else/elseif
		local nextStep = steps[step.skip]
		if nextStep.op == "ELSE" or
		   nextStep.op == "ELSEIF" then
			nextStep.ran = truthy
		end

		if truthy then
			-- prgmState.lvl = prgmState.lvl + 1 --no idea what this was about, old unchanged code maybe?
		else
			return true, step.skip
		end

	elseif step.op == "FOR" then
		if not callStack.forInit then
			local done, init = _evaluate( step.init, prgmState )
			if not done then return false, stepIndex end
			callStack.forInit = true
		end
		local nx = prgmState.stepIndex + 1

		local testDone,testVal =_evaluate( step.test, prgmState )
		
		if not testDone then return false, stepIndex end

		callStack.forInit = nil
		if not _truthy( testVal ) then
			return true, step.skip
		else
			callStack.onEndBlock = function( done, test )
				if not done then
					done, test =callYielding({
						func=callStack.onEndBlock, --upval to this cs, but checked when called, should exist
						label="for-onEndBlock-test",
						args={}
					},{
						func=_evaluate,
						args={ step.test, prgmState }
					}) 
					if not done then return false end
				end
				if _truthy( test ) then
					local done2 =_evaluate( step.inc, prgmState )
					
					if not done2 then return false end --@ end of block
					return true,nx
				else
					return true,step.skip
				end
			end

			return true,stepIndex + 1
		end
		
	elseif step.op == "ELSE" then
		if step.ran then --previous block ran
			return true, step.skip
		else
			return true, stepIndex + 1
		end
	elseif step.op == "ELSEIF" then
		local nextStep = steps[step.skip]
		local ran = step.ran
		local nxt = stepIndex + 1
		if ran then
			nxt = step.skip
		else
			local done, val = _evaluate( step.postfix, prgmState )
			
			if not done then return false, stepIndex end

			local truthy = _truthy( val )
			ran = truthy

			if not truthy then
				nxt = step.skip
			end
		end

		--notify following blocks if current or previous ran
		if nextStep.op == "ELSE" or
		   nextStep.op == "ELSEIF" then
			nextStep.ran = ran
		end

		return true, nxt

	elseif step.op == "WHILE" then
		local val = _evaluate( step.postfix, prgmState )
		
		if not val then return false, stepIndex end

		local truthy = _truthy( val )
		
		if truthy then
			return true, stepIndex + 1
		else
			return true, step.skip
		end

	elseif step.op == "RETURN" then
		local done, val = _evaluate( step.postfix, prgmState )
		 --only one return value used if multiple for some reason
		if done then
			
			if #prgmState.callStack == 2 then
				prgmState.RESULT = { val }
				return true --
			elseif callStack.setCallReturn then
				callStack.setCallReturn( val ) --sets in parent cs
				while #prgmState.callStack > 2 do
					local tmp = table.remove( prgmState.callStack ) --pop
					if tmp and tmp.parent and tmp.parent.callInfo then 
						break
					end
				end
			elseif #prgmState.callStack == 1 then
				rerr(step.line, "return statement outside function")
			end
			return --
		end
		return false, stepIndex

	elseif step.op == "BREAK" then
		local loopInst = step.loopInst
		if not loopInst.skip then rerr( step.line, "break not in loop") end
		return true, loopInst.skip
		
	elseif step.op == "CONTINUE" then
		local loopInst = step.loopInst
		if not loopInst.continue then rerr( step.line, "continue not in loop") end
		if loopInst.onEndBlock then
			local done, to = callYielding({
				func=_doStep,
				label="_doStep-continue",
				args={inputs,prgmState}
			},{
				func = loopInst.onEndBlock,
				args = {}
			})
			if not done then return false, stepIndex end
			return true, to
		end
		return true, loopInst.skip

	else
		rerr(step.line, "Unhandled op type '"..step.op.."'")
	end
	return true, stepIndex + 1
end

function _copyVar( var )
	if var.type:sub(1,3) == "mat" then
		return linalg.copyMatrix( var )
	elseif var.type:sub(1,3) == "vec" then
		return linalg.copyVector( var )
	end
	
	local copy = {type = var.type, val = var.val}
	return copy
end

--loads functions from source code into env, not the globals stuff
function _loadFunctions( prgmName,prgmState )
	local prgm = programs[prgmName]
	local cs = prgmState.callStack[1]
	local stepIndex = prgmState.callStack[#prgmState.callStack].getStepIndex()
	

	for name,data in pairs( prgm.functions )do
		if not cs.vars[name] then
			cs.vars[name] = function( ... )
				local lineNum = prgmState.inst[stepIndex].line
				local args = {...}
				local inst = data.instructions
				local params = data.params
				_insertCallStack( prgmState.callStack, false, name )
				prgmState.inst = prgm.functions[ name ].instructions
				local thisLvl = #prgmState.callStack
				fstack = prgmState.callStack[ thisLvl ]

				--add params to block
				for i=1, #params do
					local ptype = params[i].type
					local pname = params[i].name
					if ptype ~= args[i].type then
						rerr(lineNum, "arg "..i.." expected "..ptype..", got "..args[i.type])
					end

					--mat/vec are by ref
					if type(args[i].val) ~= "table" then
						fstack.vars[ pname ] = _copyVar(args[i])
					else
						fstack.vars[ pname ] = args[i]
					end
				end

				-- let main loop catch the changes

			end
		end
	end
end

--only prgm and locals are expected to be passed in
function _run( prgmName, main, inputs, prgmState )
	local prgm = programs[ prgmName ]
	prgmState = prgmState or {
		init = false,
		inst = prgm.instructions,
		prgm = prgm,
		inputs = inputs,
		callStack = {
			{vars=_makeGlobals(), onEndBlock = false, stepIndex = 1}
		}, --{name="",vars={},loopInfo={}?}
		lvl = 1
	}
	local cs = prgmState.callStack

	if not cs[1].get then
		cs[1].get = function(x)
			return prgmState.callStack[1].vars[ x ]
		end
		cs[1].getStepIndex = function()
			return cs[1].stepIndex
		end
		cs[1].setStepIndex = function( i )
			cs[1].stepIndex = i
		end
		-- cs[1].setCallReturn = function( val )
		-- 	prgmState.RESULT = val
		-- end
	end	

	_loadFunctions(prgmName, prgmState)
	--init steps
	if not prgmState.init then
		while _getInstructions( prgmState )[ cs[#cs].getStepIndex() ] do
			local ok, stepDone, nextIndex;
			
			ok, stepDone, nextIndex = pcall(	callYielding, {
				func=_run,
				label="_run-init",
				args={prgmName, main, inputs, prgmState}
			}, {
				func=_doStep,
				args={inputs, prgmState}
			})
			
			if not ok then
				prgm.ERROR = stepDone
				error(stepDone)
			end

			if not stepDone then return end

			cs[#cs].setStepIndex( nextIndex )
		end
		prgmState.init = true
		prgmState.inst = prgm.functions[ main ].instructions
		cs[1].setStepIndex( 1 )
		prgmState.func = main
	end

	--main call
	if prgmState.init then
		while _getInstructions( prgmState )[ cs[#cs].getStepIndex() ] do
			local ok, stepDone, nextIndex = pcall(	callYielding, {
				func=_run,
				label="_run-init",
				args={prgmName, main, inputs, prgmState}
			}, {
				func=_doStep,
				args={inputs, prgmState}
			})
		
			if not ok then
				prgm.ERROR = stepDone
				error(stepDone)
			end

			if not stepDone then return end

			cs[#cs].setStepIndex( nextIndex )
		end
	end

	print"Complete, collecting outs"
	--update variables declared as `out`
	local out = prgm.outData or {}
	for i, info in pairs( prgm.outputs ) do
		local varName = info.name
		local typeName = info.type
		out[ varName ] = prgmState.callStack[1].get( varName )
	end
	prgm.outData = out

	--return value from called function
	prgm.RESULT = prgmState.RESULT
end

function _parseProp( val )
	local v;
	if not val then
		v = _wrapVal(nil, "void")
	else
		local typeName = val:sub(1, val:find" ")
		local valStr = val:sub(val:find" "+1)

		if typeName == "num" then
			v = _wrapVal( tonumber(valStr) )

		elseif typeName == "str" then
			v = _wrapVal( valStr )

		elseif typeName == "bool" then
			v = _wrapVal( valStr == "true" )

		elseif typeName:sub(1,3) == "vec" then
			local size = tonumber( typeName:sub(4) )
			v = linalg.newVector(size)
			local i = 1
			for g in valStr:gmatch"[^,]+" do
				v.val[i] = tonumber(g)
				i = i + 1
			end
		elseif typeName:sub(1,3) == "mat" then
			local rows = tonumber( typeName:match"^mat([0-9]+)" )
			local cols = tonumber( typeName:match"([0-9]+)$")
			v = linalg.newMatrix( rows, cols )
			local it = valStr:gmatch"[^,]+"
			for r = 1, rows do
				for c = 1, cols do
					local g = it()
					if not g then break end
					v.val[r][c] = tonumber( g )
				end
			end
		elseif typeName:sub(1,3) == "tex" then
			v = _wrapVal( valStr, typeName )
		elseif typeName == "tbl" then
			error"not implemented"
		end
	end
end

----------------------------------------------------------------------------
INPUTS = {}
BUSY = false
function setup()
	output( BUSY, 1 )
end

function loop()
	if #unfinished > 1024 then
		error("improbable queue size (> 1024)")
	end
	-- print("#uf:"..#unfinished)
	timeout = TIMEOUT
	if not BUSY and (#unfinished > 0 or #taskQueue > 0) then
		BUSY = true
		output( BUSY, 1 )
	end
	if #unfinished == 0 and #taskQueue > 0 then
		local task = table.remove( taskQueue, 1 )
		print("Task: "..(task.label or "?"))
		task.func( table.unpack(task.args) )
	end
	while #unfinished > 0 and timeout > 0 do
		local uf = table.remove( unfinished )
		--print("resume "..(uf.label or ""))
		if not uf.func( table.unpack( uf.args ) ) then break end
		if #unfinished == 0 and #taskQueue > 0 then
			local task = table.remove( taskQueue, 1 )
			print("Task: "..(task.label or "?"))
			task.func( table.unpack(task.args) )
		end
	end
	output( PROGRESS or 0, 7 )
	if #unfinished == 0 and #taskQueue == 0 and BUSY then
		BUSY = false
		print("Task queue emptied")
		output( BUSY, 1 )
	end
	
	-- local pchunk = {}
	-- for i = 1,8 do  --screen line limit
	-- 	local c = table.remove(PRINT)
	-- 	if not c then break end
	-- 	table.insert(pchunk, c)
	-- end
	if #PRINT > 0 then
		-- _nativePrint( table.concat( pchunk, "\n") )
		_nativePrint( table.remove(PRINT, 1) )
	end
end

--public interfaces

--not used
function setProp()
	local name, val = read_var"name", read_var"v1"
	INPUTS[ name ] = _parseProp( val )
end

--not used
function clearProps()
	INPUTS = {}
end

function compile( source, programName, main )
	 source, programName, main = 
	 		source or read_var"v1",
			programName or read_var"name",
			main or read_var"v2"
	enqueueJob( "compile",function()
		print("Compiling "..programName)
		_compile( source, programName, main )
	end)
end

local function _inputVec( size, offset )
	local name = read_var"name"
	if type(name)~="string" then error"invalid name" end
	local vec = linalg.newVector( size )
	offset = offset or 0
	for i=1, size do
		local x = read_var("v"..(i+offset))
		if type(x)~="number" then error("input vec["..i.."] was not a num") end
		vec.val[i] = read_var("v"..(i+offset))
	end

	enqueueJob("input "..vec.type,function()
		INPUTS[ name ] = vec
		print("-> "..vec.type.." "..(read_var"name" or "?"))
	end)
end

local function _inputMat( row, col, offset )
	offset = offset or 0
	local name = read_var"name"
	if type(name)~="string" then error"invalid name" end
	local mat = linalg._emptyMatrix(row, col)
	for r=1, mat.rows do
		mat.val[r] = {}
		for c=1, mat.cols do
			local iName = "v"..((c-1)*mat.cols + r + offset)
			local x = read_var( iName )
			if type(x)~="number" then error(("input mat[%d,%d] (%s) was not a num"):format(r,c, iName)) end
			mat.val[r][c] = x
		end
	end

	enqueueJob("input "..mat.type,function()
		INPUTS[ name ] = mat
		print("-> "..mat.type.." "..(read_var"name" or "?"))
	end)
end

function inputVec2()
	_inputVec( 2 )
end

function inputVec3()
	_inputVec( 3 )
end

function inputVec4()
	_inputVec( 4 )
end

function inputMat2_2()
	_inputMat( 2, 2 )
end

function inputMat3_3()
	_inputMat( 3, 3 )
end

function inputMat4_4()
	_inputMat( 4, 4 )
end

function inputTex()
	local name = read_var"name"
	if type(name)~="string" then
		error"invalid name"
	end

	local channels = read_var"v2"
	if type(channels)~="number" or channels < 1 then
		error"invalid channel count"
	end
	local val = _wrapVal( read_var"v1" or "", "tex"..channels)
	enqueueJob("input "..val.type, function()
		INPUTS[ name ] = val
		print("-> "..val.type.." "..name)
	end)
end

function inputStr()
	local name = read_var"name"
	if type(name)~="string" then
		error"invalid name"
	end

	local str = read_var"v1" or ""
	if type(str) ~= "string" then
		error"string expected"
	end

	enqueueJob("input str",function()
		INPUTS[ name ] = _wrapVal( str )
		print("-> str "..name)
	end)
end

function inputBool()
	local name = read_var"name"
	if type(name)~="string" then
		error"invalid name"
	end

	local bool = not not read_var"v1"
	
	enqueueJob("input bool", function()
		INPUTS[ name ] = _wrapVal( bool )
		print("-> bool "..name)
	end)
end

function inputNum()
	local name = read_var"name"
	if type(name)~="string" then
		error"invalid name"
	end

	local num = read_var"v1"

	enqueueJob("input num",function()
		INPUTS[ name ] = _wrapVal( num )
		print("-> num "..name)
	end)
end

function transfer(p1, p2)
	local programName1, programName2 = p1 or read_var"v1", p2 or read_var"v2"

	enqueueJob(programName1.."->"..programName2, function()
		local prgm1 = programs[ programName1 ]
		local prgm2 = programs[ programName2 ]
		for name, val in pairs(prgm1.outData) do
			_printVal("outData -> ", name, val)
			INPUTS[ name ] = val
		end
		if prgm1.verts then
			print("-> verts")
			prgm2.verts = {}
			for i=1,#prgm1.verts do
				prgm2.verts[i] = linalg.copyVector( prgm1.verts[i] )
				_printVal("-->", tostring(i), prgm2.verts[i])
			end
		end
		if prgm1.uvs then
			print("-> uvs")
			prgm2.uvs = {}
			for i=1,#prgm1.uvs do
				prgm2.uvs[i] = linalg.copyVector( prgm1.uvs[i] )
				_printVal("-->", tostring(i), prgm2.uvs[i])
			end
		end
		print("transfer complete")
	end)
end

function run( programName )
	local programName = programName or read_var"name"
	local prgm = programs[programName]
	if prgm.main == "vert" then
		
		_runVert( programName )
		
	elseif prgm.main == "frag" then

		_runFrag( programName )

	else
		_run( programName, prgm.main, INPUTS )
	end
	
	yield(function() end,"end-run")
end

--has output, can't enqueue
function _runVert( programName, state )
	local prgm = programs[ programName ]
	if not prgm then
		print"Programs:"
		for k in pairs(programs) do
			print("-> "..k)
		end
		error("Could not locate program '"..tostring(programName).."'")
	end

	local verts = {
		linalg.newVector(3),
		linalg.newVector(3),
		linalg.newVector(3)
	}
	local uvs = {
		linalg.newVector(2),
		linalg.newVector(2),
		linalg.newVector(2)
	}
	
	--caputure inputs
	for i=1,3 do
		verts[i].val[1] = read_var("posx"..i)
		verts[i].val[2] = read_var("posy"..i)
		verts[i].val[3] = read_var("posz"..i)
		uvs[i].val[1] = read_var("posu"..i)
		uvs[i].val[2] = read_var("posv"..i)
	end

	--init
	enqueueJob("vert-init-"..programName,function()
		prgm.verts = verts
		prgm.uvs = uvs
		prgm.outData = {}
	end)

	enqueueJob("calc-normal",function()
		_printVal("norm","1",verts[1])
		_printVal("norm","2",verts[2])
		_printVal("norm","3",verts[3])
		local d1 = linalg.subVec(verts[2], verts[1])
		local d2 = linalg.subVec(verts[3], verts[2])
		_printVal("norm","d1",d1)
		_printVal("norm","d2",d2)
		local normal = linalg.normalize( linalg.cross(d1, d2) )
		prgm.outData.normal = normal
		_printVal("","normal",normal,"#00FF00")
	end)

	for vertexID = 1, 3 do
		enqueueJob("transform-"..vertexID,function()
			INPUTS.pos = prgm.verts[ vertexID ]
			prgm.RESULT = nil
			_run(programName, prgm.main, INPUTS)
		end)

		enqueueJob("result-"..vertexID,function()
			prgm.verts[ vertexID ] = prgm.RESULT[1]
		end)
	end
	
	--output
	enqueueJob("output",function()
		for i=1, 3 do
			local n = (i-1)*3+1
			write_var( prgm.verts[i].val[1], "out"..(n    ) )
			write_var( prgm.verts[i].val[2], "out"..(n + 1) )
			write_var( prgm.verts[i].val[3], "out"..(n + 2) )
		end
		trigger(8)
	end)
end

function _runFrag( programName, state )
	state = state or {
		step = "init"
	}
	local prgm = programs[ programName ]
	
	if state.step == "init" then
		local fac1 = read_var"v1" or 0
		local fac2 = read_var"v2" or 0
		local fac3 = read_var"v3" or 0
		if not prgm.verts then
			error"no verts from transfer"
		end
		local barPos = linalg.newVector(3)
		local barUV = linalg.newVector(2)
		for i=1,3 do
			barPos.val[i] = barPos.val[i] + prgm.verts[i].val[i] / 3
		end
		for i=1,2 do
			barUV.val[i] = barUV.val[i] + prgm.uvs[i].val[i] / 3
		end
		INPUTS.vertPos = barPos
		INPUTS.uvPos = barUV

		state.step = "color"	
		if autoYield( _runFrag,"_runFrag-init", programName, state ) then return end
	end

	if state.step == "color" then
		prgm.RESULT = nil

		_run(programName, prgm.main, INPUTS)
		state.step = "result"
		if autoYield( _runFrag,"_runFrag-color", programName, state ) then return end
	end

	if state.step == "result" then
		if prgm.RESULT then
			local r = prgm.RESULT
			for i=1,4 do
				write_var( r.val[i] or 0, "out"..i )
			end
			trigger( 8 )
		else
			yield( _runFrag,"_runFrag-result", programName, state )
			return
		end
	end
end

print("Shader module loaded")

-- ----------------------------------------------------------------------------
-- -- vs code only ------------------------------------------------------------
-- ----------------------------------------------------------------------------
if io then
	local fakeVars = {}
	function read_var( x )
		return fakeVars[ x ]
	end
	function write_var( val, name )
		_nativePrint(("[WRITE_VAR] %s -> %s"):format( tostring(val), name ))
	end
	function output( val, pin )
		_nativePrint(("[OUT] %s -> %d"):format(tostring(val), pin))
	end
	function trigger( pin )
		_nativePrint(("[TRIGGER] %d"):format(pin))
	end
	setup()


	local tri = {
		linalg._emptyVector(3),
		linalg._emptyVector(3),
		linalg._emptyVector(3)
	}
	tri[1].val = {-.5, -.5, 0}
	tri[2].val = { .5, -.5, 0}
	tri[3].val = {0.0, 0.5, -.3}

	local d1 = linalg.subVec( tri[2], tri[1] )
	local d2 = linalg.subVec( tri[3], tri[2] )
	local norm = linalg.normalize( linalg.cross( d1, d2 ) )

	local vec = linalg.vec
	local mat = linalg.mat
	local w = _wrapVal
	--simulate command
	_nativePrint"COMPILE Camera"
	_compile( makeCam, "makeCam" )
	repeat
		loop()
	until #unfinished == 0 and #taskQueue == 0 and #PRINT == 0

	_nativePrint"COMPILE VERT"
	_compile( vert, "vert1" )
	repeat
		loop()
	until #unfinished == 0 and #taskQueue == 0 and #PRINT == 0


	_nativePrint"COMPILE FRAG"
	_compile( frag, "frag1" )
	repeat
		loop()
	until #unfinished == 0 and #taskQueue == 0 and #PRINT == 0

	_nativePrint"RUN makeCam"
	_run("makeCam", "mkCam",{
		fovWidth = w(90),
		fovHeight = w(90),
		pos = vec(w(0),w(0),w(10)),
		rot = vec(w(0),w(0),w(0)),
		near = w(.1),
		far = w(50)
	})

	repeat
		loop()
	until #unfinished == 0 and #taskQueue == 0 and #PRINT == 0

	_nativePrint"RUN VERT"
	--simulate command

	_run( "vert1", "vert", {
		pos = vec(w(-.5),w(-.5), w(0)),
		--object transform
		transform = mat(vec(w(1),w(0),w(0),w(0)), vec(w(0),w(1),w(0),w(0)),vec(w(0),w(0),w(1),w( 0)), vec(w(0),w(0),w(0),w(1))), --identity
		camera =    mat(vec(w(1),w(0),w(0),w(0)), vec(w(0),w(1),w(0),w(0)),vec(w(0),w(0),w(1),w(10)), vec(w(0),w(0),w(0),w(1))), --+10 z
	})
	repeat
		loop()
	until #unfinished == 0 and #taskQueue == 0 and #PRINT == 0
	--TODO store RESULT in program instead
	--TODO `transfer` copy `out` values from `prgm1` to `prgm2`

	_nativePrint"RUN FRAG"
	_run( "frag1", "frag", { --TODO automate size from imported inputs during DECLARE and INPUT
		color    = vec( w(1), w(0), w(0) ), --red
		lightPos = vec( w(0), w(100), w( 0) ),
		camPos   = vec( w(0), w(  0), w(10) ),
		norm     = vec( w(0), w(  1), w( 0) ),
		vertPos  = vec( w(0), w(  0), w( 0) ),
		uvPos    = vec( w(0), w(  0) ),
	} )
	repeat
		loop()
	until #unfinished == 0 and #taskQueue == 0 and #PRINT == 0

	_nativePrint"Done!"
	_printVal( "DONE ","result",programs.frag1.RESULT[1] )
	repeat
		loop()
	until #unfinished == 0 and #taskQueue == 0 and #PRINT == 0
end