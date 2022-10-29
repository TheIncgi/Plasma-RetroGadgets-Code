local Material = {}

function Material:new( name )
  local obj = {}
  local mtl = require("objects/lua/"..name.."_mtl")

  --needs indexing like that for individual materials..
  local meta = {
    __index = function(t,k)
      
      return rawget(obj,k) or Material[k] or mtl[k]
    end
  }

  setmetatable(obj, meta)
  return obj
end

return Material