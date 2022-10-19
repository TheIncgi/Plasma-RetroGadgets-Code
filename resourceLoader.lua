function step(action, name, ...)
	write_var(name, "name")		for pin,val in ipairs{...} do
		write_var( val, pin )
	end	
	output(action, 1)
end

local aspect = 256 / 340
--if x is right
--y is up
--z is out of screen
--right hand rule
--all *input* ops will return on the same tick and can be loaded quicker than this
local args = {
	--camera setup
	[ 1] = {"vec3", "pos",0,0,10},
	[2] = {"vec3","rot",0,0,0}, --yaw pitch roll
	[3] = {"num","fovWidth",90},
	[4] = {"num", "fovHeight", 90*aspect},
	[5] = {"num","near",0.1},
	[6] = {"num","far",30},
	[7] = {"run","mkCam"}, --this one will take a bit longer
	-- [8]
}

step( table.unpack( args[i] or {} ) )