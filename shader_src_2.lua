--shader source code
--makeCam - makes a projection matrix
--vert                - applies transformations to triangle points individually
--frag               - says how to color a point on the triangle 
local makeCam = [[
	in num fovWidth;
	in num fovHeight;
	in num near;
	in num far;
	in vec3 pos;
	in vec3 rot; //{yaw, pitch, roll}

	out mat4_4 camera;
	out vec3 camPos;

	//source https://stackoverflow.com/a/62243585/11295774
	void mkCam() {
		num fw2 = fovWidth/2;
		num fh2 = fovHeight/2;
		print("#FF00FF","HELLO WORLD");
		print("Sanity Check" + (10/5));
		print("fov: "+fw2);
		print("rad: "+rad(fw2));
		print("#FFFF00","cot: "+cot(rad(fw2)));
		print("fov: "+fh2);
		print("rad: "+rad(fh2));
		print("#FFFF00","cot: "+cot(rad(fh2)));
		camera = mat(
			vec( cot(rad(fw2)), 0, 0, 0 ),
			vec( 0, cot(rad(fh2)), 0, 0 ),
			vec( 0, 0, far/(far-near), 0 ), //changed to 0, z pos
			vec( 0, 0, far*near / (near-far), 0 )
		);
		print( camera );
		camera = rotateMatrix( camera, vec(0,0,1), rad( rot.z ) ); //roll
		camera = rotateMatrix( camera, vec(1,0,0), rad( rot.y ) ); //pitch
		camera = rotateMatrix( camera, vec(0,1,0), rad( rot.x ) ); //yaw (Y+ is up)

		camera[1,4] = pos.x;
		camera[2,4] = pos.y;
		camera[3,4] = pos.z;
		camPos = pos;
		
		print( camera );
		return; //required in current version, no warnings though
	}
]]

local vert = [[
	in vec3 pos;
	in mat4_4 transform;
	in mat4_4 camera;
	//out vec4 camPos;

	vec3 vert() {
		//camPos = vec( camera[1,4], camera[2,4], camera[3,4], camera[4,4] );
		vec3 res = (camera * transform * vec(pos,1)).xyz;
		print("#0000FF", "Tf'd: "+res );
		return res;
	}
]]

local frag = [[
	in vec3 color;    //user value
	in vec3 lightPos; // user value
	in vec3 camPos;   //computed in vert shader
	in vec3 normal;    //special to fragment
	in vec3 vertPos; //special to fragment
	in vec2 uvPos;   //special to fragment
	
	
	num global_offset( num v ) {
		return v + 0.1;
	}

	vec4 frag() {
		vec3 lightVec = lightPos - vertPos; //direction of light to surface
		num factor = abs(normal * lightVec);   //dot product, how well it
		//matches the normal on a scale of 0 to 1

		factor = global_offset( factor ); //testing function call, adding a bit of
		//light everywhere
		
		factor = min( 1, factor ); //clamp
		vec4 fin = color * factor;  //scalar multiplication
		print("#55FF55","Pixel color:");
		print("#00FF00", fin );
		return fin;
	}
]]

prgms = {
     -- name, src, main
	[1] = {"mkCam", makeCam,"mkCam"},
	[2] = {"vert", vert,"vert"},
	[3] = {"frag",frag,"frag"}
}

i = i or 1
if i <= #prgms then
	local p = prgms[i]
	write_var(p[1],"name")
	write_var(p[2],"v1")
	write_var(p[3],"v2")
	trigger(3)
	output("compile", 1)
	i = i+1
else
	trigger(2)
end