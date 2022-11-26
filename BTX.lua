--Author: TheIncgi
--Version: 1.0.0
--Date: Nov 2022

--BTX - Binary Texture
--minimal format
--no compression
--width, height, data

--isColor, when using linear sample (blending), values are squared first for correct color blending

local BTX = {}

function BTX:load( fileName, isColor, diskMode )
  local obj = {
    fileName = fileName,
    diskMode = diskMode,
    isColor = isColor
  }

  obj.file = io.open( fileName, "r" )
  obj:_loadSize()

  if not diskMode then
    obj.data = obj.file:read"*all"
    obj.file:close()
    obj.file = nil
  end

  self.__index = self
  setmetatable( obj, self )
  return obj
end

local function toInt( bytes )
  local x = 0
  for i=1,#bytes do
    x = x * 256 + bytes[i]
  end
  return x
end

function BTX:_getBytes( start, n )
  if self.data then 
    return self.data:byte( n )
  else
    self.file:seek( start + 8 )
    return self.file:read( n or 1, "*line" ):byte( 1, n or 1 )
  end
end

function BTX:_loadSize()
  self.width = toInt{ self:_getBytes(1, 4) }
  self.height = toInt{ self:_getBytes(5, 4) }
end

--argb, 255 color space
function BTX:getPixel( x, y )
  local index = (y-1) * self.width + x
  local offset = 8 + index * 4

  return BTX:_getBytes( offset, 4 )
end

function BTX:sampleNearest( x, y )
  x = math.floor( x + 0.5 )
  y = math.floor( y + 0.5 )
  x = math.max( 1, math.min( self.width, x ) )
  y = math.max( 1, math.min( self.height, y ) )
  local A,R,G,B = self:getPixel( x, y )
  return A/255, R/255, G/255, B/255
end

function BTX:_blend( f, a, b )
  if self.isColor then
    a = a * a
    b = b * b
    return math.sqrt( (1-f)*a + f*b )
  else
    return (1-f)*a + f*b
  end
end

function BTX:sampleLinear( x, y )
  local x1 = math.floor( x + 0.5 )
  local y1 = math.floor( y + 0.5 )
  local x2 = math.ceil( x + 0.5 )
  local y2 = math.ceil( y + 0.5 )
  local xf = x-x1
  local yf = y-y1
  local c11 = { BTX:getPixel( x1, y1 ) }
  local c12 = { BTX:getPixel( x1, y2 ) }
  local c21 = { BTX:getPixel( x2, y1 ) }
  local c22 = { BTX:getPixel( x2, y2 ) }
  for c in ipairs{ c11, c12, c21, c22 } do
    for i=1,4 do
      c[i] = c[i] / 255
    end
  end
  local out = {}
  for i=1,4 do
    out[i] = self:_blend( 
      yf,
      self:_blend( xf, c11[i], c21[i] ),
      self:_blend( xf, c12[i], c22[i] )
    )
  end
  return table.unpack( out )
end

return BTX