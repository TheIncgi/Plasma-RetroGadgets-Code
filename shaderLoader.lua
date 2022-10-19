i = i or 1
local makeCam = [[
	in num fovWidth;
	in num fovHeight;
	in num near;
	in num far;
	in vec3 pos;
	in vec3 rot; //{yaw, pitch, roll}

	out mat4_4 cam;

	//source https://stackoverflow.com/a/62243585/11295774
	void mkCam() {
		cam = mat(
			vec( cot(rad(fovWidth))/2, 0, 0, 0 ),
			vec( 0, cot(rad(fovHeight))/2, 0, 0 ),
			vec( 0, 0, far/(far-near), 0 ), //changed to 0, z pos
			vec( 0, 0, far*near / (near-far), 0 )
		);
		cam = rotateMatrix( cam, vec(0,0,1), rad( rot.z ) ); //roll
		cam = rotateMatrix( cam, vec(1,0,0), rad( rot.y ) ); //pitch
		cam = rotateMatrix( cam, vec(0,1,0), rad( rot.x ) ); //yaw (Y+ is up)

		cam[1,4] = pos.x;
		cam[2,4] = pos.y;
		cam[3,4] = pos.z;
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
		return (camera * transform * vec(pos,1)).xyz;
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

prgms = {
     -- name, src, main
	[1] = {"mkCam", makeCam,"mkCam"},
	[2] = {"vert", vert,"vert"},
	[3] = {"frag",frag,"frag"}
}
if prgms[i] then
	if i == 4 then
		error"a"
	end
	print(tostring(i).."-"..tostring(prgms[i][1]) )	output( prgms[ i ][1], 1 )
	output( prgms[ i ][2], 2 )
	output( prgms[ i ][3], 3 )
	i = i+1
else
	trigger(4)
	print"all loaded"
end

output( i, 5 )