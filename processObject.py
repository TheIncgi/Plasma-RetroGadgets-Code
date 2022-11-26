import os
from PIL import Image
from struct import pack

def processObj( file ):
    objFile = "objects/source/%s.obj" % file
    # mtlFile = "objects/source/%s.mtl" % file
    luaObjFile = "objects/lua/%s_obj.lua" % file
    # luaMtlFile = "3d/luaMtl/%s.lua" % file

    out = open(luaObjFile, "w")
    

    model = {
        'objects':[]
    }
    obj = {}
    smooth = False
    faceGroup = []

    if os.path.exists( objFile ):
        f = open( objFile, "r" )
        lines = f.readlines()

        for line in lines:
            if line.startswith("#"):
                continue
            parts = line.split(" ")
            parts[-1] = parts[-1].strip()
            thing = parts[0]

            if thing == "mtllib":
                model["mtl"] = parts[1][:-4]

            elif thing == "o":
                obj = {
                    'name': parts[1],
                    'verts':[],
                    'norms':[],
                    'uvs':  [],
                    'faces':{}
                }
                model["objects"].append(obj)

            elif thing == "v":
                obj["verts"].append( [parts[1], parts[2], parts[3]] )

            elif thing == "vn":
                obj["norms"].append( [parts[1], parts[2], parts[3]] )
            
            elif thing == "vt":
                obj["uvs"].append( [parts[1], parts[2]] )

            elif thing == "s":
                smooth = parts[1] == "1" or parts[1] == "on"

            elif thing == "usemtl":
                faceGroup = obj["faces"].get(parts[1],[])
                obj["faces"][ parts[1] ] = faceGroup

            elif thing == "f":
                face = {'s': "true" if smooth else "false"}
                
                for i in range(1,4):
                    data = (parts[i]+"//").split("/")
                    v = {
                        'v':data[0],
                        't':data[1],
                        'n':data[2],
                        
                    }
                    face[str(i-1)]  = v
                faceGroup.append( face )

        out.write("return {\n")
        for ob in model["objects"]:
            out.write("    mtllib=[[%s]],\n" % model["mtl"])
            out.write("  {\n")

            out.write("    verts={\n")
            for v in ob["verts"]:
                out.write("      {%s,%s,%s},\n" % (v[0],v[1],v[2]))
            out.write("    },\n")

            out.write("    norms={\n")
            for n in ob["norms"]:
                out.write("      {%s,%s,%s},\n" % (n[0],n[1],n[2]))
            out.write("    },\n")

            out.write("    uvs={\n")
            for n in ob["uvs"]:
                out.write("      {%s,%s},\n" % (n[0],n[1]))
            out.write("    },\n")


            out.write("    faces={\n")
            for name, group in ob["faces"].items():
                out.write("      [ [[%s]] ]={\n" % (name))
                for tri in group:
                    out.write("        {smooth=%s," % tri["s"])
                    for vi in range(0,3):
                        vertex = tri[str(vi)]
                        out.write("{v=%s,n=%s,t=%s}," % (vertex["v"], vertex["n"], vertex["t"]))
                    out.write("},\n")
                    
                out.write("      },\n")
                
            out.write("    },\n")

            out.write("  }\n")
        out.write("}\n")
        out.close()
        f.close()


def parseMap(out, parts):
    map = {}
    i = 1
    out.write("{\n")

    while i < len(parts):
        p = parts[i]
        if p.startswith("-"):
            opt = p[1:]
            if opt == "blendu":
                out.write("      blendu=%s,\n" % "true" if parts[2] == "on" else "false" )
                i = i+1

            elif opt == "blendv":
                out.write("      blendv=%s,\n" % "true" if parts[2] == "on" else "false" )
                i = i+1

            elif opt == "boost":
                out.write("      boost=%s,\n" % parts[2])
                i = i+1

            elif opt == "mm":
                out.write("      modMap={base=%s,gain=%s},\n" % parts[2], parts[3])
                i = i+2

            elif opt == "o":
                u = parts[2]
                v,w = "0","0"
                i = i+1
                if len(parts) > 3:
                    v = parts[3]
                    i = i+1
                if len(parts) > 4:
                    v = parts[4]
                    i = i+1

                out.write("      offset={%s,%s,%s},\n" % (u,v,w))


            elif opt == "s":
                u = parts[2]
                v,w = "0","0"
                i = i+1
                if len(parts) > 3:
                    v = parts[3]
                    i = i+1
                if len(parts) > 4:
                    v = parts[4]
                    i = i+1
                out.write("      scale={%s,%s,%s},\n" % (u,v,w))

            elif opt == "t":
                u = parts[2]
                v,w = "0","0"
                i = i+1
                if len(parts) > 3:
                    v = parts[3]
                    i = i+1
                if len(parts) > 4:
                    v = parts[4]
                    i = i+1
                out.write("      turbulence={%s,%s,%s},\n" % (u,v,w))

            elif opt == "texres":
                # wiki doesn't say what the format of resolution is
                pass

            elif opt == "clamp":
                out.write("      clamp=%s,\n" % "true" if parts[2] == "on" else "false" )
                i = i+1

            elif opt == "bm":
                out.write("      bumpMultiplier=%s,\n" % parts[2])
                i = i+1

            elif opt == "imfchan":
                out.write("      imfChannel=%s,\n" % parts[2])
                i = i+1

        else:
            out.write("      file=[[%s]],\n" % os.path.basename( p )[:-4]+".btx")
            i = i+1
    out.write("}")


def processMtl( file ):
    mtlFile = "objects/source/%s.mtl" % file
    luaMtlFile = "objects/lua/%s_mtl.lua" % file

    if os.path.exists( mtlFile ):
        f = open( mtlFile, "r" )
        lines = f.readlines()
        out = open(luaMtlFile,"w")
        

        out.write("return {\n")


        first = True

        names = {
            'Ka':{'name':'ambientColor','args':3},
            'Kd':{'name':'diffuseColor','args':3},
            'Ks':{'name':'specularColor','args':3},
            'Ns':{'name':'specularExponent','args':3},
            'Ke':{'name':'emissive','args':3},
            'Ni':{'name':'ior','args':1},
            'd':{'name':'opacity','args':1},
            'Tr':{'name':'transparency','args':1}, #1-opacity
            'illum':{'name':'mode','args':1},
            'Pr':{'name':'roughness','args':1},
            'Pm':{'name':'metallic','args':1},
            'Ps':{'name':'sheen','args':1},
            'Pc':{'name':'clearcoatThickness','args':1},
            'Pcr':{'name':'clearcoatRoughness','args':1},
            'Ke':{'name':'emissive','args':3},
            'aniso':{'name':'anisotropy','args':1},
            'anisor':{'name':'anisotropy rotation','args':1},
            'norm':{'name':'normal','args':0}, #always map
        }


        for line in lines:
            if line.startswith("#"):
                continue
            parts = line.strip().split(" ")
            if len(parts) == 0 or parts[0] == "":
                continue
            cmd = parts[0]

            #k color? N number
            if cmd == "newmtl":
                if not first:
                    out.write("  }, \n")
                out.write("  [ [[%s]] ] = {\n" % parts[1])
                first = False

            elif cmd == "Ka":
                out.write("    ambientColor={%s,%s,%s},\n" % (parts[1],parts[2],parts[3]))

            elif cmd == "Kd":
                out.write("    diffuseColor={%s,%s,%s},\n" % (parts[1],parts[2],parts[3]))

            elif cmd == "Ks":
                out.write("    specularColor={%s,%s,%s},\n" % (parts[1],parts[2],parts[3]))

            elif cmd == "Ns":
                out.write("    specularExponent=%s,\n" % parts[1] )

            elif cmd == "Ke": #??
                out.write("    emissive={%s,%s,%s},\n" % (parts[1],parts[2],parts[3]))

            elif cmd == "Ni": # IOR
                out.write("    ior=%s,\n" % parts[1])

            elif cmd == "d":
                out.write("    opacity=%s,\n" % parts[1] )

            elif cmd == "Tr":
                out.write("    transparency=%f,\n" % 1-float(parts[1]) )

            elif cmd == "illum":
                out.write("    mode=%s,\n" % parts[1])

            elif cmd == "Pr":
                out.write("    roughness=%s,\n" % parts[1])

            elif cmd == "Pm":
                out.write("    metalic=%s,\n" % parts[1])

            elif cmd == "Ps":
                out.write("    sheen=%s,\n" % parts[1])

            elif cmd == "Pc":
                out.write("    clearcoatThickness=%s,\n" % parts[1])

            elif cmd == "Pcr":
                out.write("    clearcoatRoughness=%s,\n" % parts[1])

            elif cmd == "aniso":
                out.write("    anisotropy=%s,\n" % parts[1])

            elif cmd == "anisor":
                out.write("    anisotropyRotation=%s,\n" % parts[1])
                

            elif cmd == "norm":
                out.write("    norm=")
                parseMap( out,parts )
                out.write(",\n")


            elif cmd.startswith("map_"):
                cmd = cmd[4:]
                info = names[cmd]
                out.write("    %sMap=" % info["name"])
                parseMap( out,parts )
                out.write(",\n")
                


        out.write("  }\n")
        out.write("}")

        out.close()
        f.close()
    
def simplifyTexture( file ):
    img = Image.open("textures/"+file)
    img = img.convert("RGBA")
    out = open("textures/%s.btx" % file[:-4], "wb")
    out.write( pack(">II", img.width, img.height) )

    for y in range( 0, img.height ):
        for x in range( 0, img.width ):
            r,g,b,a = img.getpixel((0,0))
            out.write( pack("BBBB", a,r,g,b) )

    out.close()



all = {}
for file in os.listdir("objects/source"):
    name = file[:-4]
    ext = file[-3:]
    print(name," ",ext)

    if ext == "obj" and not os.path.exists("objects/lua/%s_obj.lua" % name):
        processObj( name )

    if ext == "mtl" and not os.path.exists("objects/lua/%s_mtl.lua" % name):
        processMtl( name )


for file in os.listdir("textures"):
    name = file[:-4]
    ext = file[-3:]
    if ext == "btx":
        continue
    print(name," ",ext)
    simplifyTexture( file )
    