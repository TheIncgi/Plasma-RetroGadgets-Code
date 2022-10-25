--Author: TheIncgi--... this could be neater ... 
function step(action, name, ...)
	write_var(name, "name")		for pin,val in ipairs{...} do
		write_var( val, "v"..(pin<10 and "0" or "")..pin )
	end	
	output(action, 1)
	output(action..(name and " "..name or ""), 2)
end

--cam is +Z
--normal is --normalize(vert 1 CROSS vert 2)
--RHR, counter-clockwise is visible
--  u v (0,0) is bottom left of texture
local triangle = {
  --    x       y        z      u      v
	-0.5, -0.5,   0.0,  0.0, 0.0, --lower left
	  0.5, -0.5,   0.0,   1.0, 0.0,--lower right
	  0.0,   0.5, -0.3,  0.5, 1.0 --upper center, sligtly tilted back to catch some light
}

local aspect = 256 / 340
--if x is right
--y is up
--z is out of screen
--right hand rule
--all *input* ops will return on the same tick and can be loaded quicker than this
local args = {
	--action, name, values...
	--camera setup
	[ 1] = {"vec3", "pos",0,0,3},
	[2] = {"vec3","rot",0,0,0}, --yaw pitch roll
	[3] = {"num","fovWidth",90},
	[4] = {"num", "fovHeight", 90*aspect},
	[5] = {"num","near",0.1},
	[6] = {"num","far",30},
	[7] = {"run","mkCam"}, --this one will take a bit longer
	--pass cam to vert
	[ 8] = {"transfer","vert","mkCam"}, --name dest, v1 src
	[ 9] = {"mat4_4","transform",  1,0,0,0,   0,1,0,0,   0,0,1,0,   0,0,0,1  }, --identity, aka nothing
	[10] = {"run","vert",table.unpack(triangle)},
	[11] = {"vec3","color", 1, 0 , 0 }, -- R G B
	[12] = {"vec3","lightPos", 0, 100, 0 }, --up 100
	[13] = {"transfer","frag","vert"}
	--at this point *almost* everything is ready for the fragment
	--shader, when run v 1/2/3 need to be set to the barycentric coords of	--the triangle
	--but before we can do that, we need to know what pixels are needed, and	--the really nice thing is that even though the points we have after the 
	--vertex shader are in screen space, the barymetric coords are the same	--(used internally by fragment shader)
		
	--shaders named 'vert' and 'frag' have special handing in the shader module
	--vert is run on all 3 input points (xyz uv)	--frag takes the barymetric coords and converts them into a 3d pos based on points from vert

	--in vec3 camPos;   //computed in vert shader
	--in vec3 norm;    //special to fragment
	--in vec3 vertPos; //special to fragment
	--in vec2 uvPos;   //special to fragment
}
if V1 == 10 then
	output( "vert shader", 3)
elseif V1 == 11 then
	output( "frag inputs", 3)
elseif V1 == 1 then
	output("cam inputs",3)
elseif V1 == 7 then
	output("make cam",3)
elseif V1 == 8 then
	output("vert inputs",3)
end
if args[V1] then
	step( table.unpack( args[V1] or {} ) )
elseif V1 == 14 then
	output( "rasterize", 3)
	output("bary",1 )
end

if V1 >= 15 then
	local n = V1-15
	n = n % 3 + 1
	if n == 1 then
		output("bary-next",1)
		output("bary-next",3)
	elseif n == 2 then
		local b1=read_var"bc1"
		local b2=read_var"bc2"
		local b3=read_var"bc3"
		output("frag",3)
		step( "run", "frag", b1, b2, b3)
	elseif n == 3 then
		output("plot",3)
		output("plot", 1)
	end
end