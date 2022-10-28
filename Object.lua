local Object = {}
local Material = require("Material")

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

function Object:render( screen, rast, env )
  env = env or {}
end

return Object