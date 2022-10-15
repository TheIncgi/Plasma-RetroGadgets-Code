--Author: TheIncgi
--Oct 2022
--Plasma Demo

--test code
local vert = [[
	in vec3 pos;
	in mat4 transform;
	in mat4 camera;
	out vec4 camPos;

	vec3 vert() {
		camPos.xyzw = camera.[1,4],[2,4],[3,4],[4,4];
		return camera * transform * pos;
	}
]]

local frag = [[
	in vec3 color;
	in vec3 lightPos;
	in vec3 camPos;   //computed in vert shader
	in vec3 norm;
	in vec3 vertPos;
	
	vec4 frag() {
		vec3 lightVec = lightPos - vertPos;
		num factor = abs(norm * lightVec);   //dot product
		factor += 0.1;
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
local programs = {}
--linear algebra lib, vectors and matrices
local linalg = {}

-- functions added here are available to the compiled shader
local function _makeGlobals()
	local g = {}
	for name, val in pairs( math ) do
		if type(val) == "function" then
			g[ name ] = function(...)
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
	
	
	--TODO hsv rgb conversion
	--TODO wrap some math functions to work on vectors
	-- min, max
	-- function len( vec )

	return g
end
----------------------------------------------------------------------------
-- Linear Algebra Library
-- pcall in evaluation 

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
end

function linalg.identity( matrix )
	for r = 1, matrix.rows do
		for c = 1, matrix.cols do
			matrix.val[r][c] = r==c and 1 or 0
		end
	end
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
		out[r] = {}
		for c = 1, matB.cols do
			local sum = 0
			for i = 1, #matA.size do
				sum = sum + matA.val[r][i] * matB.val[i][c]
			end
			out.val[r][c] = 0
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
		out.val[i] = vec.val[i] / m
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
		out[i] = a[p1] * b[p2] - a[m1] * b[m2]
	end
	return out
end

 --https://math.stackexchange.com/a/4155115
 function linalg.newRotateMatrix( vec, amount )
	local a = linalg.normalized( vec )
	a = {x=a[1], y=a[2], z=a[3],} --easier to read
	a.X, a.Y, a.Z = a.x*a.x,  a.y*a.y,  a.z*a.z --^2, found on identity
	local C = math.cos( amount )
	local S = math.sin( amount )
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
	return linalg.mult( matrix, rot )
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
	local out = linalg.newVector( matrix.rows )
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

	if t == "vector" then
		local vec = val.val
		v = ("<%d, %d, %d, %d>"):format( table.unpack(vec) )
	elseif t == "matrix" then
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
		types[ ("mat%d_%d"):format(4,c) ] = true
	end
end
--highligher is buggy with quotes
local SINGLE_QUOTE = string.char(39)
local DOUBLE_QUOTE = string.char(34)
local patterns = {
	str = { "'[^'\n]'", '"[^"\n]"' },
	num = { 
		int="^[0-9]$", 
		float="^[0-9]*%.[0-9]*$",
		hex="^0x[0-9a-fA-F]+$",
		bin="^0b[01]+$"
	},
	var = { "^[a-zA-Z_][a-zA-Z0-9_]*$" },
	op = { "^[()%{%}%[%]%%.%!%#%^%*%/%+%-%=%~%&|;,%<%>]+$" }
}



local TIMEOUT = 200 --loops before exit to continue next tick
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
	if text:match"[ \t\n\r]" and text~="\n" then
		return false
	elseif text=="\n" then
		return "line"
	end
	for _, name in ipairs{ "op","num","var" } do
		local group = patterns[ name ]
		local txt =  text
		if name == "str" then
			--hide escaped quotes for testing
			txt = txt:gsub("\\'",""):gsub('\\"')
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
local function yield( func, ... )
	table.insert(unfinished,  {
	--unfinished[1] = {
		func = func,
		args = {...}
	})
end

local timeout = TIMEOUT
function autoYield( func, ... )
	timeout = timeout - 1
	if timeout <= 0 then
		yield( func, ... )
		return true
	end
	return false
end

function _tokenize( src, state, quoting )
	state = state or {}
	state.area = state.area or {1,0}
	state.tokens = state.tokens or {}
	state.quoting = quoting or false
	
	local inst = state.instructions
	local tokens = state.tokens
	
	local moreTokens = true
	while state.area[2] <= #src do
		local area = state.area
		
		local chunk = src:sub( area[1], area[2] )
		local chunk2 = src:sub( area[1], area[2]+1 )
		
		--prevent bad op merging
		--ex y=-4 being =- or () not being split
		local chunkType2 = _chunkType( chunk2 )
		if chunkType2 == "op" and #chunk == 1 then
			chunkType2 = ops[ chunk2 ] ~= nil
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
		if autoYield( _tokenize, src, state ) then return false end
	end
	local chunk = src:sub( state.area[1], state.area[2] )
	if #chunk > 0 then --whitespace or chunk
		if _chunkType( chunk ) then
			table.insert( state.tokens, chunk )
		end
	end
	return true
end

function _computeLineNums( state, index, lineNum, comment )
	local tokens = state.tokens
	comment = comment or false
	state.lineNums = {}
	lineNum = lineNum or 1
	index = index or 1
	
	while tokens[ index ] do
		local token  = tokens[index]
		
		if token == "\n" then
			comment = false
			lineNum = lineNum + 1
			table.remove( tokens, index )
		elseif comment or token == "//" then
			comment = true
			table.remove( tokens, index )
		else
			state.lineNums[ index ] = lineNum
			index = index + 1
		end
		
		if autoYield( _computeLineNums, state, index, lineNum, comment ) then 
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
function _evalInstr( state, first, last, i, out, stack )
	local tokens = state.tokens
	i = i or first
	--last or nil
	out = out or {}
	stack = stack or {}
	while tokens[ i ] ~= ";" and (last==nil or i <= last) do
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
				table.remove( table(stack) ) --don't need )
			end
		

		elseif chunkType1 == "num" then
			local n
			if t1:match( patterns.num.bin ) then
				n = tonumber( t1:sub(2), 2 ) --0b
			else
				n =tonumber( t1 ) --handles 0x already :)
			end
			if not n then
				cerr( lineNum )
			end
			table.insert( out, {op="val", val = _wrapVal( n )  } )

		elseif chunkType1 == "str" then
			table.insert( out, {op="val", val=t1:sub(2,-2):gsub('\\"','"'):gsub("\\'","'") } )

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
		if autoYield( _evalInstr, state, first, last, i, out, stack ) then return end
	end


	while #stack > 0 do
		local pop = table.remove( stack )
		if pop.closeWith then
			cerr( state.lineNums[first], "Missing "..pop.closeWith )
		end
		table.insert( out, pop )
	end
	
	return {
		op="EVAL",
		postfix = out
	}, i

end

local function _findClosingPar( state, i, par, lvl )
	par = par or ")"
	lvl = lvl or 0
	while state.tokens[ i ] and ( lvl > 0 or state.tokens[ i ] ~= par) do
		local t = state.tokens[i]
		if ("([{"):find(t,1,true) then
			lvl = lvl+1
		elseif (")]}"):find(t,1,true) then
			lvl = lvl-1
		end
		if autoYield( _findClosingPar, state, i, par, lvl ) then return false end
	end
	return not not state.tokens[ i ], i
end

function _buildInstructions( state, index )
	local index = index or 1
	local inst = 
		state.inFunc and state.inFunc.instructions 
		or state.instructions --{{line1},{line2},...}
	local ctrlStack = state.ctrlStack or {}
	local scope = state.scope
	local tokens = state.tokens
	
	while tokens[index] do
		local lineNum = state.lineNums[ index ]
		local t1 = tokens[index]
		local t2,t3,t4 = tokens[ index + 1 ], tokens[ index + 2 ], tokens[index + 3]
		local i2,i3,i4,i5 = index+1, index+2, index+3, index+4

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
					local done, at = _findClosingPar( state, i2, ")" )
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
					local done, at = _findClosingPar( state, i2, ")" )
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
			if (not t2:match"[=-/*]?=") and t2 ~= "(" then
				cerr(lineNum,"expected assignment or call")
			end
			local eval,lastToken = _evalInstr( state, index )
			if eval then
				eval.stepIndex = #inst+1
				eval.line = lineNum
				table.insert( inst, eval )
				index = lastToken + 1
			end --else needs more itteration
		end
		
		if autoYield( _buildInstructions, state, index ) then return false end
	end
	return true
end

function _compile( src, prgmName, state )
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
		line = 1
	}
	
	if state.step == "tokenize" then
		if _tokenize( src, state ) then
			state.step = "computeLines"
		end
		if autoYield( _compile,  src, prgmName, state ) then return false end
	end
	
	if state.step == "computeLines" then
		if _computeLineNums( state ) then
			state.step = "buildInstructions"
		end
		if autoYield( _compile, src, prgmName, state ) then return false end
	end
	
	if state.step == "buildInstructions" then
		if _buildInstructions( state ) then
			programs[ prgmName ] = state
			return true
		end
		if autoYield( _compile, src, prgmName, state ) then return false end
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
function _insertCallStack( cs, includeUpScope )
	local global = cs[1]

	local vars = {}
	--setmetatable() oof, not available

	local new = {
		vars = vars,
		parent = cs[#cs],
		onEndBlock = false
	}
	if not includeUpScope then
		new.stepIndex = 1
		--call info is stored in parent because `new` will be popped off stack when done
		new.parent.callInfo = {
			returnVals = false
		}
	end
	--checks self
	--optionally checks parent
	--checks global
	new.get = function( x )
		return vars[x] 
		  or (includeUpScope and new.parent and new.parent.get( x ))
		  or global[x]
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
			return new.parent.setCallReturn( stuff )
		else
			new.callInfo.returnVals = { stuff }
		end
	end

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
	add = function( a, b )
		local c;
		if a.type:sub(1,3) == "vec" or b.type:sub(1,3)=="vec" then
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

	sub = function( a,b )
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

	mult = function( a,b )
		local c;
		if a.type:sub(1,3) == "vec" and b.type:sub(1,3)=="vec" then
			c = linalg.dot(a, b)
		elseif a.type:sub(1,3) == "vec" and b.type == "num" then
			c = linalg.scaleVec( a, b )
		elseif a.type == "num" and b.type:sub(1,3) == "vec" then
			c = linalg.scaleVec( b, a )
		elseif a.type(1,3) == "mat" or b.type:sub(1,3) == "mat" then
			if a.type=="num" then 
				c = linalg.scaleMatrix( b, a )
			elseif b.type =="num" then
				c = linalg.scaleMatrix( a, b )
			elseif a.type(1,3) == "mat" and b.type:sub(1,3) == "mat" then
				c = linalg.matrixMult( a, b )
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

	div = function( a,b )
		local c;
		if a.type:sub(1,3) == "vec" and b.type == "num" then
			c = linalg.scaleVec( a, 1/b )
		elseif a.type == "num" and b.type:sub(1,3) == "vec" then
			c = linalg.vecUnder( b, a )
		elseif a.type(1,3) == "mat" or b.type:sub(1,3) == "mat" then
			if a.type=="num" then 
				c = linalg.matrixUnder( b, a )
			elseif b.type =="num" then
				c = linalg.scaleMatrix( a, 1/b )
			-- no matrix divide matrix
			-- elseif a.type(1,3) == "mat" and b.type:sub(1,3) == "mat" then
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

	eq = function( a, b )
		error"not implemented"
	end
}

function _evaluate( postfix, prgmState, i )

	local env = prgmState.callStack[#prgmState.callStack]
	env.stack = env.stack or {}
	env.callMarkers = env.callMarkers or {}
	local callMarkers = env.callMarkers
	local stack = env.stack
	local i = i or 1
	local lineNum = prgmState.inst[ env.getStepIndex() ].line
	while i <=#postfix do
		local step = postfix[i]
		if step.op == "var" then
			table.insert( stack, env.get(step.val) )
		elseif step.op == "val" then
			table.insert( stack, step.val)
		elseif step.op == "-" then
			local b = table.remove( stack )
			local a = table.remove( stack )
			local c = evalOps.sub( a,b );
			
			table.insert( stack, c )
		elseif step.op == "+" then
			local b = table.remove( stack )
			local a = table.remove( stack )
			local c = evalOps.add(a,b)
			
			table.insert( stack, c )
		elseif step.op == "*" then
			local b = table.remove( stack )
			local a = table.remove( stack )
			local c = evalOps.mult( a,b );
			
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
			local c = evalOps.div( a,b );
			
			table.insert( stack, c )
		elseif step.op == "==" then
			local b = table.remove( stack )
			local a = table.remove( stack )
			local c = evalOps.eq( a,b );
			
			table.insert( stack, c )
		elseif step.op == "~=" or step.op == "!=" then
			local b = table.remove( stack )
			local a = table.remove( stack )
			local c;
			error"not implemented"
			local b = table.remove( stack )
			local a = table.remove( stack )
			local c;
		elseif step.op == ">" then 
			local b = table.remove( stack )
			local a = table.remove( stack )
			local c;
			error"not implemented"
		elseif step.op == "<" then
			local b = table.remove( stack )
			local a = table.remove( stack )
			local c;
			error"not implemented"
		elseif step.op == ">=" then
			local b = table.remove( stack )
			local a = table.remove( stack )
			local c;
			error"not implemented"
		elseif step.op == "<=" then
			local b = table.remove( stack )
			local a = table.remove( stack )
			local c;
			error"not implemented"
			
		elseif step.op == "=" then --copy ref/assign
			local b = table.remove( stack )
			local a = stack[#stack]
			a.val = b.val

		elseif step.op == "+=" then
			local b = table.remove( stack )
			local a = stack[#stack]
			local c = evalOps.add( a, b )
			a.val = c.val
			
		elseif step.op == "-=" then
			local b = table.remove( stack )
			local a = stack[#stack]
			local c = evalOps.sub( a, b )
			a.val = c.val

		elseif step.op == "/=" then
			local b = table.remove( stack )
			local a = stack[#stack]
			local c = evalOps.div( a, b )
			a.val = c.val

		elseif step.op == "*=" then
			local b = table.remove( stack )
			local a = stack[#stack]
			local c = evalOps.mult( a, b )
			a.val = c.val

		elseif step.op == "." then
			local b = table.remove( stack )
			local a = table.remove( stack )
			error"not implemented"
		elseif step.op == "CALL_END" then
			table.insert(callMarkers, #stack+1) --last arg of function call
		elseif step.op == "CALL" then ----------------------------------------------------------todo () type or [] type

			--if already done (previously called async)
			if env.callInfo and env.callInfo.returnVals then
					table.insert( stack, env.callInfo.returnVals[1] )
					env.callInfo = nil
			elseif env.callInfo then
					return false --continuing is handled by yield system
			else

				local mark = table.remove( callMarkers )
				if not mark then
					rerr( lineNum, "Compiler issue: missing end of call args op")
				end
				local args = {}
				local fName = step.val
				while #stack >= mark do
					table.insert( args, table.remove( stack ))
				end
				
				--actual call
				local done, c = env.get( fName )( table.unpack(args) )

				if not done then --100% of shader functions 
					return false --needs continuation
				end
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
		if autoYield( _evaluate, postfix, prgmState, i ) then 
			return false 
		end
	end
	env.stack = {}
	env.callMarkers = {}
	return true, table.unpack( stack )
end

function _doStep( inputs, prgmState )
	--DECLARE, INPUT, EVAL, IF, FOR, WHILE, RETURN, BREAK, CONTINUE
	local steps = prgmState.inst
	local stepIndex = prgmState.callStack[ #prgmState.callStack ].getStepIndex()
	local step = steps[ stepIndex ]
	local callStack = prgmState.callStack[ prgmState.lvl ]
	local globals = prgmState.callStack[1].vars
	local vars = callStack.vars

	if step.op == "DECLARE" then
		if vars[ step.name ] then
			rerr( step.line, "var "..step.name.." is already defined in this scope")
		else
			vars[ step.name ] = {type=step.type, name=step.name} --val=nil
			local v = vars[step.name]
			local t = v.type:sub(1,3)
			if t == "vec" then
				v.size = tonumber( v.type:sub(4) )
			elseif t == "mat" then
				v.rows = tonumber(v.type:match"^mat([0-9]+)")
				v.cols = tonumber(v.type:match"_([0-9]+)$")
			elseif t == "tex" then

			end
		end
		return stepIndex + 1

	elseif step.op == "INPUT" then
		if not globals[step.name] then
			rerr( step.line, "var "..step.name.." wasn't declared yet");
		end
		local ty = globals[ step.name ].type
		local v = vars[ step.name ]
		v.val = _typeCheck( inputs[ step.name ], ty, step.name, step.line )
		v.val.type = nil --only used while inputing, type stored in v
		if ty:sub(1,3) == "vec" then
			v.size = #v.val
			if v.size ~= tonumber( ty:match("[0-9]+$") ) then
				rerr( step.line, "incorrect vector size used for input")
			end
		elseif ty:sub(1,3)=="mat" then
			v.rows = #v.val
			v.cols = #v.val[1]
			if v.rows ~= tonumber(ty:match"^mat([0-9])+_") or
			v.cols ~= tonumber(ty:match"([0-9]+)$") then
				rerr( step.line, "incorrect matrix size used for input")
			end
		end
		
		return stepIndex + 1

	elseif step.op == "EVAL" then
		local done = _evaluate( step.postfix, prgmState )
		
	elseif step.op == "SCOPE_UP" then
		_insertCallStack( callStack, true )

	elseif step.op == "SCOPE_DOWN" then
		table.remove( callStack ) --pop

	elseif step.op == "IF" then
		local done, val = _evaluate( step.postfix, prgmState )
		local truthy = _truthy( val )
		step.ran = truthy --used by else/elseif
		local nextStep = steps[step.skip]
		if nextStep.op == "ELSE" or
		   nextStep.op == "ELSEIF" then
			nextStep.ran = truthy
		end

		if truthy then
			prgmState.lvl = prgmState.lvl + 1
		else
			return step.skip
		end

	elseif step.op == "FOR" then
		local done, init = _evaluate( step.init, prgmState )
		local nx = prgmState.stepIndex + 1

		callStack.onEndBlock = function()
			local test = _evalInstr( step.test, prgmState )
			if _truthy( test ) then
				local done = _evaluate( step.inc, prgmState ) --@ end of block
				return nx
			else
				return step.skip
			end
		end
	elseif step.op == "ELSE" then
		if step.ran then --previous block ran
			return step.skip
		else
			return stepIndex + 1
		end
	elseif step.op == "ELSEIF" then
		local nextStep = steps[step.skip]
		local ran = step.ran
		local nxt = stepIndex + 1
		if ran then
			nxt = step.skip
		else
			local val = _evaluate( step.postfix, prgmState )
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

		return nxt

	elseif step.op == "WHILE" then
		local val = _evaluate( step.postix, prgmState )
		local truthy = _truthy( val )
		
		if truthy then
			return stepIndex + 1
		else
			return step.skip
		end

	elseif step.op == "RETURN" then
		local done, val = _evaluate( step.postfix, prgmState ) --only one return value used if multiple for some reason
		if done then
			if callStack.setCallReturn then
				callStack.setCallReturn( val ) --sets in parent cs
				table.remove( callStack ) --pop
			elseif #prgmState.callStack == 1 then
				prgmState.RESULT = { val }
				return --
			else
			end
			return --
		end
		return stepIndex

	elseif step.op == "BREAK" then
		local loopInst = step.loopInst
		if not loopInst.skip then rerr( step.line, "break not in loop") end
		return loopInst.skip
		
	elseif step.op == "CONTINUE" then
		local loopInst = step.loopInst
		if not loopInst.continue then rerr( step.line, "continue not in loop") end
		return 
				loopInst.onEndBlock 
			and loopInst.onEndBlock() 
			 or loopInst.skip

	else
		rerr(step.line, "Unhandled op type '"..step.op.."'")
	end
	return stepIndex + 1
end

function _copyVar( var )
	if var.type:sub(1,3) == "mat" then
		return linalg.copyMatrix( var )
	elseif var.type:sub(1,3) == "vec" then
		return linalg.copyVector( var )
	end
	
	local copy = {type = var.type}
	return copy
end

--loads functions from source code into env, not the globals stuff
function _loadFunctions( prgmName,prgmState )
	local prgm = programs[prgmName]
	local cs = prgmState.callStack[1]
	local stepIndex = prgmState.callStack[#prgmState.callStack].getStepIndex()
	local lineNum = prgmState.inst[stepIndex].line

	for name,data in pairs( prgm.functions )do
		cs.vars[name] = function( ... )
			local args = {...}
			local inst = data.instructions
			local params = data.params
			_insertCallStack( prgmState.callStack, false )
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
	end	

	_loadFunctions(prgmName, prgmState)
	--init steps
	if not prgmState.init then
		while prgmState.inst[ cs[#cs].getStepIndex() ] do
			cs[#cs].setStepIndex( _doStep( inputs, prgmState ) )
			if autoYield( _run, prgmName, inputs, prgmState ) then return false end
		end
		prgmState.init = true
		prgmState.inst = prgm.functions[ main ].instructions
		cs[1].setStepIndex( 1 )
		prgmState.func = main
	end

	--main call
	if prgmState.init then
		while prgmState.inst[ cs[#cs].getStepIndex() ] do
			cs[#cs].setStepIndex( _doStep( inputs, prgmState ) )
			if autoYield( _run, prgmName, inputs, prgmState ) then return false end
		end
	end

	--update variables declared as `out`
	local out = {}
	for i, info in pairs( prgm.outputs ) do
		local varName = info.name
		local typeName = info.type
		out[ varName ] = prgmState.globals[ varName ]
	end

	--return value from called function
	RESULT = prgmState.RESULT
end

----------------------------------------------------------------------------
function setup()
end

function loop()
	timeout = TIMEOUT
	if #unfinished > 0 then
		local uf = unfinished[#unfinished]
		unfinished[#unfinished] = nil
		uf.func( table.unpack( uf.args ) )
	end
end

--public interfaces

function compile( source, programName )
	_compile( source, programName )
end

-- ----------------------------------------------------------------------------
-- -- vs code only ------------------------------------------------------------
-- ----------------------------------------------------------------------------
setup()

--simulate command
_compile( vert, "vert1" )
_compile( frag, "frag1" )
repeat
	loop()
until #unfinished == 0

--simulate command
_run( "vert1", "vert", {
	pos = {-.5,-.5, 0, type="vec3"},
	--object transform
	transform = {{1,0,0,0},{0,1,0,0},{0,0,1,0},{0,0,0,1}  type="mat4"}, --identity
	camera = {{1,0,0,0},{0,1,0,0},{0,0,1,10},{0,0,0,1}  type="mat4"}, --+10 z
})

--TODO store RESULT in program instead
--TODO `transfer` copy `out` values from `prgm1` to `prgm2`

_run( "frag1", "frag", { --TODO automate size from imported inputs during DECLARE and INPUT
	color = { 1, 0, 0,type="vec3"}, --red
	lightPos = {0, 100, 0,type="vec3"},
	camPos = {0, 0, 10, type="vec3"},
	norm = {0, 1, 0,type="vec3"},
	vertPos= {0, 0, 0,type="vec3"},
} )
repeat
	loop()
until #unfinished == 0

print"Done!"
print( RESULT )
