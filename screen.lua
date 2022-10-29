local screen = {
  buffer = {},
  WIDTH = 64,
  HEIGHT = 32
}

local ESC = string.char(27)
local block = string.char(219)

function clear(r,g,b)
  r,g,b = r or 0, g or 0, b or 0
  local x = pixel(r,g,b)
  for h=1,screen.HEIGHT do
    screen.buffer[h] = {}
    for w=1,screen.WIDTH do
      screen.buffer[h][w] = x
    end
  end
end

function pixel(r,g,b)
  return (ESC.."[38;2;%d;%d;%dm"..block):format(r,g,b)
end

function setPixel(x,y, r,g,b)
  screen.buffer[y][x] = pixel(r,g,b)
end

function draw()
  print()
  for h,row in ipairs(screen.buffer) do
    local line = table.concat(row)
    print(line)
  end
end

clear()
draw()