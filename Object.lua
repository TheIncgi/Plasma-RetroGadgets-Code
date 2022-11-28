local Object = {}
local Material = require("Material")
local linalg = require"linalg"
local BTX = require"BTX"

Object.colorProperties = {
  ["ambientColor"] = true,
  ["diffuseColor"] = true,
  ["specularColor"] = true,
  ["emissive"] = true,
}

function Object:new( name )
  local obj = {}

  local model = require("objects/lua/"..name.."_obj")
  local mtl = Material:new( model.mtllib ) --instanced, safe to edit
  
  obj.model = model --can be multiple objects
  obj.material = mtl

  obj.x = 0
  obj.y = 0
  obj.z = 0
  obj.yaw = 0
  obj.pitch = 0
  obj.roll = 0

  self.__index = self
  setmetatable(obj, self)
  return obj
end

function Object:getPosVec()
  return {type="vec",val={self.x, self.y, self.z},size=3}
end

function Object:modelMatrix( baseTransform )
  local mm = linalg.identity(linalg.newMatrix(4,4))
  local rotMat = linalg.rotateMatrix
  local vec = linalg.vec
  local rad = math.rad
  local translate = linalg.transform

  mm = rotMat(mm,vec(0,0,1), rad(self.roll ))
  mm = rotMat(mm,vec(1,0,0), rad(self.pitch))
  mm = rotMat(mm,vec(0,1,0), rad(self.yaw  ))
  
  mm = translate(mm, vec( linalg.scaleVec(self:getPosVec(),-1),0))
  if baseTransform then
    mm = linalg.matrixMult( baseTransform, mm )
  end
  return mm
end

function Object:vertShader( env )
  local pos = env.pos
  local transform = env.transform
  local proj = env.projection
  local view = env.view
  local m = linalg.matrixMult
  --local s = linalg.vecSwizzle
  local vec = linalg.vec
  
  local p = m(m(view,transform), vec(pos,1))
  return m(proj, p)
end

function Object:_loadTexMap( mtl, propName )
  local prop = mtl[ propName ]
  if prop.btx then return end
  local fileName = prop.file:sub(1, -5) .. ".btx"

  prop.btx = BTX:load( "textures/"..fileName, Object.colorProperties[ propName ], false )
end

function Object:_sampleNearest( mtl, propName, uv )
  local vec = linalg.vec
  local mapName = propName .. "Map"
  if mtl[ mapName ] then
    self:_loadTexMap( mtl, mapName )
    local btx = mtl[ mapName ].btx
    return vec(btx:sampleNearest( uv.val[1] * (btx.width-1)+1, uv.val[2] * (btx.height-1)+1 ))
  else
    return vec(mtl[ propName ])
  end
end

function Object:_sampleLinear( mtl, propName, uv )
  local vec = linalg.vec
  local mapName = propName .. "Map"
  if mtl[ mapName ] then
    self:_loadTexMap( mtl, mapName )
    return vec(mtl[ mapName ].btx:sampleLinear( uv.val[1], uv.val[2] ))
  else
    return vec(mtl[ propName ])
  end
end

function Object:fragShader( env )
  local subv = linalg.subVec
  local addv = linalg.addVec
  local dot = linalg.dot
  local scale = linalg.scaleVec
  local barPos = env.barPos
  local mtl = env.mtl
  local vec = linalg.vec
  local uv = env.uvPos
  local modelView = env.transform
  local multM = linalg.matrixMult
  local vertPos = env.vertPos
  local s = linalg.vecSwizzle

  local worldPos = s(vec( multM( modelView , vec(vertPos, 1.0))),"xyz")
	local worldNormal = s(linalg.normalize(
                        vec( 
                          multM( 
                            modelView,
                             vec(env.normal, 0.0))
                        )
                     ),"xyz")
	-- local cameraNormal = linalg.subVec( cameraPos, worldPos);

  local lightVec = subv( 
    env.lightPos,
    worldPos
  )

  local factor = math.abs(
    dot(worldNormal, lightVec)
  )
  factor = math.min(1,factor + .1)
  
  local clr = self:_sampleNearest( mtl, "diffuseColor", uv ) --mtl.diffuseColor and vec(mtl.diffuseColor) or barPos
  -- local clr = linalg.vec( env.uvPos, 1 )
  -- local clr = barPos
  --env.color
  return scale(clr, factor)
end

local function applyBar( bar, a,b,c )
  local out = linalg.newVector(a.size)
  for i=1,out.size do
    out.val[i] = a.val[i] * bar[1]
               + b.val[i] * bar[2]
               + c.val[i] * bar[3]
  end
  return out
end

function Object:_onPixel(args)
  local xyzs = args.v
  local norms = args.n
  local clips = args.clip
  local uvs = args.t
  local bar = args.bar
  local env = args.env
  local px,py = args.px, args.py
  local screen = args.screen
  local unpack = table.unpack

  local xyz = applyBar(bar,unpack(xyzs))
  local norm = applyBar(bar,unpack(norms))
  local clip = applyBar(bar,unpack(clips))
  local uv = applyBar(bar,unpack(uvs))
  
  env.vertPos = xyz
  env.uvPos = uv
  env.normal = norm
  env.clipPos = clip
  env.barPos = linalg._emptyVector(3)
  env.barPos.val = bar

  local c = self:fragShader( env )
  for i=1,3 do
    c.val[i] = math.max(0,math.min(255,math.floor(c.val[i]*255)))
  end
  screen.setPixel(px,py, unpack(c.val, 1, 3))
end

--face group
function Object:_renderGroup( env, raster, screen, faceGroup, part )
  for i, face in ipairs( faceGroup ) do
    local smooth = face.smooth
    local verts = part.verts
    local norms = part.norms
    local uvs = part.uvs
    local p1,p2,p3 = table.unpack( face )
    local v1,v2,v3 = verts[p1.v], verts[p2.v], verts[p3.v]
    local n1,n2,n3 = norms[p1.n], norms[p2.n], norms[p3.n]
    local t1,t2,t3 =   uvs[p1.t],   uvs[p2.t],   uvs[p3.t]

    local vec = linalg.vec
    local sw = linalg.vecSwizzle
    local sc = linalg.scaleVec
    local abs = math.abs
    v1,v2,v3 = vec(v1), vec(v2), vec(v3)
    n1,n2,n3 = vec(n1), vec(n2), vec(n3)
    t1,t2,t3 = vec(t1), vec(t2), vec(t3)

    if not n1 then
      local n = linalg.cross(
        linalg.sub(v2, v1),
        linalg.sub(v3, v2)
      ) 
      n1,n2,n3 = n,n,n
    end

    --vertex
    local tf = {}
    local clip = {}
    local pointIn = false
    for i,v in ipairs{v1,v2,v3} do
      env.pos = v
      tf[i] = self:vertShader( env )
      clip[i] = tf[i]
      local w = 1 / sw(tf[i], "w")
      if w <= 0 then break end
        if not (abs(tf[i].val[1]) > 1
        and abs(tf[i].val[2]) > 1
        and abs(tf[i].val[3]) > 1) then 
          pointIn = true
		  end
      tf[i] = sw( sc(tf[i],w),"xyz" )
    end
    if not pointIn then return false end

    --fragment
    rast.setVecs{ tf[1].val, tf[2].val, tf[3].val }
    local function onPixel(bar,x,y) 
      self:_onPixel{
        bar=bar,
        px=x,
        py=y,
        v={v1,v2,v3},
        n={n1,n2,n3},
        t={t1,t2,t3},
        tf=tf,
        clip=clip,
        env=env,
        screen=screen
      }
    end
    while rast.itterate( onPixel ) do
      --yield
    end
  end
end

function Object:render( env, raster, screen )
  local baseTransform = env.transform
  env.transform = self:modelMatrix( baseTransform )
  for partNo, part in pairs( self.model ) do
    if type(part)=="table" then
      for material, faceGroup in pairs( part.faces ) do
        env.mtl = self.material[ material ]
        self:_renderGroup( env, raster, screen, faceGroup, part )
      end
    end
  end
  env.transform = baseTransform
end

return Object