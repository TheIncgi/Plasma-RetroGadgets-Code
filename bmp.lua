local Bitmap = {}

function Bitmap:new( width, height )
  local obj = {
    width = width,
    height = height,
    channels = 3,
    pixels = {},
    fillColor = {r=0,g=0,b=0}
  }

  for y=1, obj.height do
    obj.pixels[y] = {}
  end

  self.__index = self
  setmetatable(obj,self)
  return obj
end

function Bitmap:setPixel(x,y,color)
  self.pixels[y][x] = color
end

function Bitmap:getPixel(x,y)
  return self.pixels[y][x] or self.fillColor
end

local ZERO = string.char( 0 )

-- before Lua 5.3
-- local function toBytes( value, bytes )
--   local rs = bit32.rshift
--   local band = bit32.band
--   local out = ""
--   for i = 1, n do
--     v = band( 0xFF, rs( value, (i-1)*8) )
--     out = string.char(v)..out
--   end
--   return out
-- end

local function toBytes( value, bytes )

  local out = ""
  for i = 1, bytes do
    v = 0xFF & ( value >> (i-1)*8)
    out = out..string.char(v)
  end
  if #out > bytes then error("Value out of bounds") end
  return out
end

local function i( value )
  return toBytes( value, 4 )
end
local function s( value )
  return toBytes( value, 2 )
end
local function b( value )
  return toBytes( value, 1 )
end

function Bitmap:_genHeaderStart(fileSize, pixelOffset)
  pixelOffset = 14 + pixelOffset
  local header = "BM".."%s"..ZERO:rep(4).."%s"
  return header:format( i(fileSize), i(pixelOffset) )
end

function Bitmap:_genHeader()
  local data = {
    { 40, b=4 }, --header size
    { self.width, b=4 }, --width
    { self.height, b=4 }, --height
    { 1, b=2 }, --planes, must be 1
    { self.channels == 4 and 32 or 24, b=2 }, --bit count
    { 0, b=4 }, -- not compressed
    { 0, b=4 }, -- image size, uncompressed is 0
    { 0, b=4 }, --x pixels per meter, 0, no pref
    { 0, b=4 }, --y pixels per meter
    { 0, b=4 }, --color map entries used, no color table used
    { 0, b=4 }, --number significant colors, all are important
  }
  for i,v in ipairs(data) do
    data[i] = toBytes( v[1], v.b )
  end
  local pixels = self.width * self.height
  local rowLen = self.width * self.channels
  rowLen = rowLen + (4 - rowLen % 4)
  local pixelBytes = rowLen * self.height
  local start = self:_genHeaderStart(54 + pixelBytes, 55-15) ---15, idk, but it works
  return start..table.concat( data )
end

function Bitmap:serializeImg()
  local out = {self:_genHeader()}
  local n = 2
  for Y = 1, self.height do
    local y = self.height - Y + 1
    for x = 1, self.width do
      local p = self:getPixel( x, y )
      --little endian, smallest first
      out[ n     ] = toBytes( p.b, 1 )
      out[ n + 1 ] = toBytes( p.g, 1 )
      out[ n + 2 ] = toBytes( p.r, 1 )

      n = n+3
    end
  end
  return table.concat( out )
end

function Bitmap:save( name )
  file = io.open( name, "w+b" )
  file:write( self:serializeImg() )
  file:close()
end

-- function output_image( id, data, pin )
--   local bmp = Bitmap:new( 340, 256 )
--   for y = 1, bmp.height do
--     local flipY = bmp.height - y + 1
--     for x = 1, bmp.width do
--       local color = data[ x + (y-1) * bmp.width ]
--       bmp:setPixel( x, y, color )
--     end
--   end
-- end

return Bitmap