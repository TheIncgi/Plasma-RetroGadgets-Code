local Object = {}
local Material = require("Material")
local linalg = require"linalg"

function Object:new( name )
  local obj = {}

  local model = require("objects/lua/"..name)
  local mtl = Material:new( model.mtllib ) --instanced, safe to edit
  
  obj.model = model --can be multiple objects
  obj.material = mtl

  obj.pos = {0,0,0}
  obj.yaw = 0
  obj.pitch = 0
  obj.roll = 0

  setmetatable(obj, self)
  return obj
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

function Object:fragShader( env )
  local subv = linalg.subVec
  local addv = linalg.addVec
  local dot = linalg.dot
  local scale = linalg.scaleVec
  local barPos = env.barPos

  local lightVec = subv( 
    env.lightPos,
    env.vertPos
  )
  local factor = math.abs(
    dot(env.normal,lightVec)
  )
  factor = math.min(1,factor + .1)
  -- local clr = linalg.vec( env.uvPos, 1 )
  --env.color
  return scale(barPos, factor)
end

function Object:_renderFace( env, raster, face )
end

--face group
function Object:_renderPart( env, raster, part )
end

function Object:render( env, raster )
  for material, list in pairs( self.model.faces ) do
    self:_renderPart( env, raster, list )
  end
end

return Object