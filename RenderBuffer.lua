local BMP = require"BMP"
local RenderBuffer = {}

function RenderBuffer:new( width, height, channelNames )
  local obj = {
    width=width,
    height=height,
    data={}
  }
  self.__index = self
  setmetatable(obj, self)

  obj.channelNames = obj.channelNames or {
    "r", "g", "b", "a"
  }

  --lookup values
  for i=1,#obj.channelNames do
    obj.channelNames[ obj.channelNames[i] ] = i
  end

  return obj
end

function RenderBuffer:getValues( x, y, channels, defaults )
  if not defaults then
    defaults = {}
    for i=1,#channels do defaults[i] = 0 end
  end
  local row = self.data[y]
  if not row then return defaults end
  local cell = row[x]
  if not cell then return defaults end
  local values = {}
  for i,cName in ipairs(channels) do
    values[i] = cell[ self.channelNames[cName] ] or defaults[i]
  end
  return values
end

function RenderBuffer:setValues( x, y, values )
  self.data[y] = self.data[y] or {}
  self.data[y][x] = self.data[y][x] or {}
  for name, val in pairs( values ) do
    local c = type(name)=="number" and name or self.channelNames[name]
    self.data[y][x][c] = val
  end
end

function RenderBuffer:exportBMP( fileName, c1, c2, c3 )
  c1 = c1 or "r"
  c2 = c2 or "g"
  c3 = c3 or "b"
  local channels = {c1,c2,c3}
  local bmp = BMP:new( self.width, self.height )

  self:processChannels( {}, function(x,y)
    local v = self:getValues( x, y, channels )
    bmp:setPixel( x, y, {r=v[1], g=v[2] ,b=v[3] })
  end)
  bmp:save( fileName )
end



--effect( x, y )
--  returns altered channelValues at x, y
function RenderBuffer:processChannels( channelNames, effect )
  for y = 1, self.height do
    for x = 1, self.width do
      if #channelNames == 0 then
        effect( x, y )
      else
        local result = {effect( x, y )}
        local values = {}
        for i, c in ipairs(channelNames) do
          values[c] = result[i]
        end
        for i,v in ipairs( values ) do
          self:setValues( x, y, values )
        end
      end
    end
  end
end

--helper for processChannels
--kernel is also [y][x] unless transposed
function RenderBuffer:convolveChannels( channelNames, kernel, kCenterX, kCenterY, transpose)
  local tmp = RenderBuffer:new( self.width, self.height )

  self:processChannels( {}, function( x, y )
    local sums = {}
    for ky = 1, #kernel do
      local dy = ky - kCenterY
      for kx = 1, #kernel[ky] or 0 do
        local dx = kx - kCenterX
        local f = transpose and kernel[kx][ky] or kernel[ky][kx]
        local values = self:getValues( x+dx, y+dy, channelNames )
        for i, v in ipairs(values) do
          local cn = channelNames[i]
          sums[ cn ] = sums[ cn ] + v
        end
      end
    end
    tmp:setValues( x, y, sums )
  end)

  self:processChannels( channelNames, function( x, y ) 
    return tmp:getValues( x, y, channelNames )
  end)
end

function RenderBuffer:boxBlur( channelNames, radius )
  local kernel = {}
  local f = 1 / (1+radius*2)
  for i = 1, 1+radius*2 do
    kernel[i] = f
  end
  local center = radius + 1
  self:convolveChannels( channelNames, kernel, 1, center, false)
  self:convolveChannels( channelNames, kernel, center, 1, true)
end

RenderBuffer.mixModes = {
  screen = function( a, b )
    return 1 - ( (1-a) * (1-b) )
  end,
  mult = function( a, b )
    return a * b
  end,
  darken = function( a, b )
    return math.min( a, b )
  end,
  lighten = function( a, b )
    return math.max( a, b )
  end,
  add = function( a, b )
    return a + b
  end,
  sub = function( a, b )
    return a - b
  end,
  difference = function( a, b )
    return math.abs( a - b )
  end
}

function RenderBuffer:mixChannels( channelSet1, ChannelSet2, method )
end

return RenderBuffer