local CLAW_TIME = 20
local Z_OFFSET = 57
local OUTPUTS = {
	x = {1, 5.0},
	y = {2, 5.0},
	z = {3, -2.5},
	yaw = 4,
	pitch = 5,
	claw = 6,
	nfcReset = 7,
	step = 8
}
local INPUTS = {
	dist = 1,
	nfc = 3,
	targetReached = 4
}

local targets = {
	pickup = { x = 94.9596, y = 287.5 },
	drop = {
		{x = 249.9997, y =   37.4600, yaw = 90},
		{x = 249.9997, y = 147.4590, yaw = 90},
		{x = 249.9997, y = 247.4582, yaw = 90},
	},
	reader={ x = 94.9596, y = 170 },
	platform = { x = 94.9596, y = 57.5 }
}

local state = {
	holding = false,
	target = false, --or { z={1.2, false}, x = {4.3,false}, y = {2.2,false}, ..yaw/pitch} --pos,reached
	step = "setup",
	timer = false
}

--returns true when done
local function makeTimer(  ticks, onDone, ... )
	if type(ticks)~="number" then error("expected # ticks",2) end
	if type(onDone) ~= "function" then error("missing onDone for timer",2) end
	local t = 0
	local args = { ... }
	return function()
		t = t + 1
		if t == ticks and onDone then
			onDone( table.unpack( args ) )
			return true
		end
		return t > ticks, ticks - t
	end
end

local function delayedSetStep( step )
	return function()
		state.step = step
	end
end

local function setOutputs( outs )
	for name,value in pairs( outs ) do
		local id = OUTPUTS[ name ]
		if type(id)=="table" then
			value = value / id[2]
			id = id[1]
		end
		output( value, id )
	end
end

local function getInput( name )
	return ({V1,V2,V3,V4,V5,V6,V7,V8})[ INPUTS[ name ] ]
end

local function getInputs()
	local t = {}
	for name, id in pairs( INPUTS ) do
		t[ name ] = getInput( name )
	end
	return t
end

local function outputTargets()
	local out = {}
	for axis, value in pairs( state.target ) do
		if OUTPUTS[ axis ] and not value[2] then
			out[axis] = value[1]
		end
	end
	if getInput"nfc" < 0 then
		out.nfcReset = nil
	end
	setOutputs( out )
end

function grab( s, after )
	after = after or function() end
	s = s == nil or s
	setOutputs{ claw = s }
	state.timer = makeTimer( CLAW_TIME, after )
end

function clearNFC( after )
	go( {nfcReset = 1}, after )
end

function go( pos, onReach )
	pos.x = pos.x or pos[1]
	pos.y = pos.y or pos[2]
	pos.z = pos.z or pos[3]
	if pos.x and state.target.x ~= pos.x then
		state.target.x = { pos.x , false }
	end
	if pos.y and state.target.y ~= pos.y then
		state.target.y = { pos.y, false }
	end
	if pos.z and state.target.z ~= pos.z then
		state.target.z = { pos.z , false }
	end
	if pos.yaw and state.target.yaw ~= pos.yaw then
		state.target.yaw = { pos.yaw, false}
	end
	if pos.pitch and state.target.pitch ~= pos.pitch then
		state.target.pitch = { pos.pitch, false}
	end
	if pos.nfcReset and getInput"nfc" >= 0 then
		state.target.nfcReset = { 0, false }
	--else
		--state.target.nfcReset = {nil, false }
	end
	state.target.onReach = pos.onReach or onReach
	--outputTargets()
end

function reached()
	local axis = getInput( "targetReached" )
	if state.target and state.target[ axis ] then
		state.target[ axis ][2] = true
	end
	for axis, target in pairs( state.target )do
		if type(target) == "table" then
			if not target[2] then return end
		end
	end
	if state.target.onReach then
		state.target.onReach()
	end
end

function isBox()
	local space = 232
	local dist = getInput"dist" * 100
	local d = space - dist
	return d > 5,  space - d/2 - Z_OFFSET
end

function setup()
	grab( false )
	state.target = {
		x = {0,false},
		y = {0,false},
		z = {0,false},
		yaw = {0,false},
		pitch = {0,false},
		nfcReset = {0, false}
	}
  state.target.onReach = delayedSetStep"initalized"
end

function loop()
 	if state.target == false then
   		setup()
	end
	if type(state) ~= "table" then
		error("state is not table ("..type(state)..")")
	end
	if type(state.target) ~= "table" then
		error("target is not table ("..type(state.target)..")")
	end

	if state.timer then
		local done, ticksRemaining = state.timer()
		if done then 
			state.timer = nil 
			return
		else
			output( state.step.."-"..ticksRemaining, 8 )
			return
		end
	elseif not state.target.z[2] then
		output( state.step.."-z", 8 )
		setOutputs{z = state.target.z[1]} return
	elseif
		not state.target.x[2] 
		or not state.target.y[2] 
		or not state.target.yaw[2]
		or not state.target.pitch[2]
		or not state.target.nfcReset[2]
		then
		
		setOutputs{ 
			x = state.target.x[1],
			y = state.target.y[1],
			yaw = state.target.yaw[1],
			pitch = state.target.pitch[1],
			nfcReset = state.target.nfcReset[1]
		}
		local axis = ""
		for name,val in pairs(state.target) do
			if #axis == 0 and type(val)=="table" and val[2]==false then
				axis = name .. "\n" .. tostring(val[1])
			end
		end
		output( state.step.."-"..axis, 8 ) return
	elseif state.step == "setup" then
		--
	elseif state.step == "initalized" then
		state.step = "pickup"
		state.flips = 0
	elseif state.step == "clearNFC" then
		if getInput"nfc" < 0 then
			state.target.onReach()
		end
	elseif state.step == "pickup" then
		go( targets.pickup, delayedSetStep"waitForBox" )
	elseif state.step == "waitForBox" then
		local is, target = isBox()
		if is then
			state.step = "pickup-lowering"
			go({ z = target }, delayedSetStep"pickup-lowered")
		end
	elseif state.step == "pickup-lowering" then
		--
	elseif state.step == "pickup-lowered" then
		grab( true, delayedSetStep"pickup-lift" )
		state.step = "grabbing"
	elseif state.step == "grabbing" then
		--timer
		error("Timer missing? "..tostring(state.timer))
	elseif state.step == "pickup-lift" then
		go({ z = 0 }, delayedSetStep"toReader")
	elseif state.step == "toReader" then
		go( targets.reader, delayedSetStep"reader-lowered" )
	elseif state.step == "reader-lowered" then
		go( {z=0}, delayedSetStep"reader-raised" )
	elseif state.step == "reader-raised" then
		local nfc = getInput"nfc"
		if nfc < 0 or nfc > #targets.drop then
			if state.flips >= 6 then
				state.step = "trash"
			end
		else
			local drop = targets.drop[ nfc ]
			go( drop, delayedSetStep"drop" )
		end
	elseif state.step == "trash" then
		go({x=0, y=0}, delayedSetStep"drop")
	elseif state.step == "drop" then
		grab( false, delayedSetStep"pickup" )
		state.step = "dropping"
	elseif state.step == "dropping" then
		--timer
	else
		error("Missing step handler '"..tostring(state.step).."'")
	end
	setOutputs{step = state.step}
end