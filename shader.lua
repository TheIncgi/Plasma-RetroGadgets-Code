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
	local g = {
		sync = {}, --returns value instantly
		async = {}, --returns true/false, value, uses yielding
	}
	for name, val in pairs( math ) do
		if type(val) == "function" then
			g.sync[ name ] = val
		end
	end
	g.sync.read_var = g.read_var
	g.sync.read = g.read_var   --alias
	
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
	return {type="mat"..size, rows=rows, cols=cols}
end
function linalg._emptyVector( size )
	return {type="vec"..size, size=size}
end
function linalg.newMatrix( rows, cols )
	local out = linalg._emptyMatrix( rows, cols )
	for r=1,rows do
		out[r]={}
		for c=1, cols do
			out[r][c] = 0
		end
	end
	
	return out
end

function linalg.newVector( size )
	local out = linalg._emptyVector( size )
	for i=1,size do
		out[i] = 0
	end
end

function linalg.identity( matrix )
	for r = 1, matrix.rows do
		for c = 1, matrix.cols do
			matrix[r][c] = r==c and 1 or 0
		end
	end
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
				sum = sum + matA[r][i] * matB[i][c]
			end
			out[r][c] = 0
		end
	end
	return out
end

function linalg.magnitude( vec )
	local sum = 0
	for i=1,vec.size do
		sum = sum + vec[i] * vec[i]
	end
	return math.sqrt( sum )
end

function linalg.normalize( vec )
	local m = linalg.magnitude( vec )
	local out = {}
	for i = 1, vec.size do
		vec[i] = vec[i] / m
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
	mat[1] = { U * a.X       + C,          U * a.x * a.y - S * a.z,   U * a.x * a.z + S * a.y,   0 }
	mat[2] = { U * a.x * a.y + S * a.z,    U * a.Y       + C      ,   U * a.y * a.z - S * a.x,   0 }
	mat[3] = { U * a.x * a.z - S * a.y,    U * a.y * a.z + S * a.x,   U * a.Z       + C      ,   0 }
	mat[4] = {                        0,                          0,                        0,   1 }
	
	return mat
end

function linalg.transform( mat, offset )
	if mat.rows ~= offset.size then
		error("sizes don't match")
	end
	local out = linalg.copyMatrix( mat )
	for i=1,mat.rows do
		out[i][mat.cols] = mat[i][mat.cols] + offset[i]
	end
	return out
end

function linalg.transpose( mat )
	mat = linalg.copyMatrix( mat )
	for r=1, mat.rows do
		for c=1, mat.cols do
			if c > r then
				local t = mat[r][c]
				mat[r][c] = mat[c][r]
				mat[c][r] = t
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
		out[i][i] = matrix[i][i] * scaleVec[i]
	end
	return
end

function linalg.vecToCol( vec )
	local out = linalg._emptyMatrix( vec.size, 1 );
	for i = 1, vec.size do
		out[i] = {vec[i]}
	end
	return out
end
function linalg.vecToRow( vec )
	local out = linalg._emptyMatrix( vec.size, 1 );
	out[1] = {}
	for i = 1, vec.size do
		out[1][i] = {vec[i]}
	end
	return out
end

function linalg.copyMatrix( from, to )
	to = to or linalg.newMatrix( from.rows, from.cols )
	for r=1, from.rows do
		to[r] = {}
		for c=1, from.cols do
			to[r][c] = from[r][c]
		end
	end
	return to
end

function linalg.colToVec( matrix, col )
	local out = linalg.newVector( matrix.rows )
	for i = 1, matrix.rows do
		out[i] = matrix[i][col]
	end
	return out
end

function linalg.rowToVec( matrix, row )
	local out = linalg.newVector( matrix.cols )
	for i = 1, matrix.cols do
		out[i] = matrix[row][i]
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
		local vec = val
		v = ("<%d, %d, %d, %d>"):format( table.unpack(vec) )
	elseif t == "matrix" then
		local mat = val
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
	["tex1"]=true
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
	op = { "^[()%{%}%[%]%%.%!%#%^%*%/%+%-%=%~%&|;,]+$" }
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
					op="call",
					val=t1, 
					openWith=t2, 
					closeWith=_closePar(t2),
					lvl = ops[t2]
				})
				i = i+1 --another +1 at bottom of loop
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
			if stack[#stack].op=="call" then
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
			table.insert( out, {op="val", } )

		elseif chunkType1 == "str" then
			table.insert( out, {op="val", val=t1:sub(2,-2):gsub('\\"','"'):gsub("\\'","'") } )

		elseif t1 == "-" and ( not t0 or _chunkType(t0)=="op" ) then
			-- negate
			table.insert( out, {op="negate"}) --no lvl needed, comparison only against stack

		elseif t1 == "," then
			while #stack > 0 and stack[#stack].op~="call" do
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
			table.insert( inst, {op="SCOPE_UP"})
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
			table.insert( inst, {op="SCOPE_DOWN"})
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
	--checks self
	--optionally checks parent
	--checks global
	new.get = function( x )
		return vars[x] 
		  or (includeUpScope and new.parent and new.parent.x)
		  or global[x]
	end
end

function _evaluate( postfix, prgmState, i )
	local stack = {}
	local env = prgmState.callStack[#prgmState.callStack]
	local i = i or 1
	
	while i <=#postfix do
		local step = postfix[i]
		if step.op == "var" then
			table.insert( stack, env.get(step.val) )
		elseif step.op == "-" then
			local b = table.remove( stack )
			local a = table.remove( stack )
			if a.type:sub(1,3) == "vec" or b.type:sub(1,3) then
		elseif step.op == "=" then --copy ref/assign
			local b = table.remove( stack )
			local a = stack[#stack]
			a.val = b.val
		end

		i = i+1
	end
	
	return table.unpack( stack )
end

function _doStep( inputs, prgmState )
	--DECLARE, INPUT, EVAL, IF, FOR, WHILE, RETURN, BREAK, CONTINUE
	local steps = prgmState.inst
	local stepIndex = prgmState.stepIndex
	local step = steps[ stepIndex ]
	local callStack = prgmState.callStack[ prgmState.lvl ]
	local globals = prgmState.callStack[1].vars
	local vars = callStack.vars

	if step.op == "DECLARE" then
		if vars[ step.name ] then
			rerr( step.line, "var "..step.name.." is already defined in this scope")
		else
			vars[ step.name ] = {type=step.type, name=step.name} --val=nil
		end
		return stepIndex + 1

	elseif step.op == "INPUT" then
		if not globals[step.name] then
			rerr( step.line, "var "..step.name.." wasn't declared yet");
		end
		local ty = globals[ step.name ].type
		vars[ step.name ].val = _typeCheck( inputs[ step.name ], ty, step.name, step.line )
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
		local val = _evaluate( step.postix )
		local truthy = _truthy( val )
		
		if truthy then
			return stepIndex + 1
		else
			return step.skip
		end

	elseif step.op == "RETURN" then

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

--only prgm and locals are expected to be passed in
function _run( prgmName, main, inputs, prgmState )
	local prgm = programs[ prgmName ]
	prgmState = prgmState or {
		init = false,
		inst = prgm.instructions,
		stepIndex = 1, --execution point
		callStack = {
			{vars=_makeGlobals(), onEndBlock = false}
		}, --{name="",vars={},loopInfo={}?}
		lvl = 1
	}
	if not prgmState.callStack[1].get then
		prgmState.callStack[1].get = function(x)
			return prgmState.callStack[1].vars[ x ]
		end
	end	
	--init steps
	if not prgmState.init then
		while prgmState.inst[ prgmState.stepIndex ] do
			prgmState.stepIndex = _doStep( inputs, prgmState )
			if autoYield( _run, prgmName, inputs, prgmState ) then return false end
		end
		prgmState.init = true
		prgmState.inst = prgm.functions[ main ].instructions
		prgmState.stepIndex = 1
		prgmState.func = main
	end

	--main call
	if prgmState.init then
		while prgmState.inst[ prgmState.stepIndex ] do
			prgmState.stepIndex = _doStep( inputs, prgmState )
			if autoYield( _run, prgmName, inputs, prgmState ) then return false end
		end
	end

	local out = {}
	for i, info in pairs( prgm.outputs ) do
		local varName = info.name
		local typeName = info.type
		out[ varName ] = prgmState.globals[ varName ]
	end
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

-- ----------------------------------------------------------------------------
-- -- vs code only ------------------------------------------------------------
-- ----------------------------------------------------------------------------
setup()

--simulate command
_compile( frag, "frag1" )
repeat
	loop()
until #unfinished == 0

--simulate command
_run( "frag1", "frag", {
	color = { 1, 0, 0,type="vec3"}, --red
	lightPos = {0, 100, 0,type="vec3"},
	camPos = {0, 10, 0,type="vec3"},
	norm = {0, 1, 0,type="vec3"},
	vertPos= {0, 0, 0,type="vec3"},
} )
repeat
	loop()
until #unfinished == 0