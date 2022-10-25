i = i or 1
local nSource = 3

local tri = {
	-0.5, -0.5,   0.0,   0.0, 0.0,
	  0.5,  -0.5,  0.0,    1.0, 0.0,
	  0.0,    0.5,  0.0,    0.5, 1.0
}
local aspect = 256 / 340

local steps = {
	-- { cmd="", name="", args={} }	
	{ cmd="inputVec3", name="pos", args={0,0,3} },
	{ cmd="inputVec3", name="rot", args={0,0,0} },
	{ cmd="inputNum", name="fovWidth", args={90} },
	{ cmd="inputNum", name="fovHeight", args={90*aspect} },
	{ cmd="inputNum", name="near", args={0.1} },
	{ cmd="inputNum", name="far", args={30} },
	{ cmd="run", name="mkCam", args={} },
	{ cmd="transfer", name="vert", args={"mkCam"} },
	{ cmd="inputMat4_4", name="transform", args={1,0,0,0,   0,1,0,0,   0,0,1,0,   0,0,0,1} },
	{ cmd="run", name="vert", args=tri },
	{ cmd="inputVec3", name="color", args={ 1,0,0} },
	{ cmd="inputVec3", name="lightPos", args={0,100,0} },
	{ cmd="transfer", name="frag", args={"vert"} },
	{ cmd="mkBuffer", name=1, args={340/4, 256/4}}, --buffer resolution
	{ cmd="rasterize", name="frag", args={ 1 } },
	{ cmd="getBuffer", name=1, args={}}	
}

if steps[i] then
	local step = steps[i]
	print(step.cmd..": "..step.name)
	write_var( step.name, "name" )
	for j, v in pairs( step.args ) do
		write_var( step.args[ j ], "v"..j )
	end
	trigger(3) --emit vars
	output(step.cmd, 1) --Do CMD
  i = i+1
end