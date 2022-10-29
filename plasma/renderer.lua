W = 340
H = 240
T = W * H

function setColor( i )
	local start = 1 + i * 3
	local r,g,b = V1:byte( start, start+3 )
	if not r then return false end
	write_var( r, "r" )
	write_var( g, "g" )
	write_var( b, "b" )
	return true
end

function getIndex( x, y )
	-- x in pixels ->	-- x in % width
	-- x in scaled pixels
	-- %
	x = math.floor( x  * datumPerRow / W ) 
	y = math.floor( y  * datumPerCol / H )
	return datumPerRow * y + x
end

function setPos( x, y )
	write_var( x, "x1" )
	write_var( y, "y1" )
	write_var( x + r, "x2" )
	write_var( y + r, "y2" )
end

local function prog( i )
	local p = i / T * 100
	output( p, 1 )
end

local function done()
	
	if r <= 4 or r <= V3 then --V3 is resolution
		output( nil, 2 )
		isDone = true
	else
		r = r / 2
		dx = 0
		dy = 0
	end
	
	return isDone
end

function setup()
	ticks = 1
	r = 32
	dx, dy = 0, 0
	isDone = false
	datumPerRow = math.ceil( W / V3 )
	datumPerCol = math.ceil( H / V3 )
end

function loop()
	if isDone then return end
	print( ("%d, %d @ %d | %d"):format( dx, dy, r, V3 ) )
	local i = getIndex( dx, dy )
	if setColor( i ) then
		setPos( dx, dy )
		prog( i )
		dx = dx + r
		if dx > W then
			dx = 0
			dy = dy + r
			if dy > H then
				done()
			end
		end
	else
		--error"Out of data"
		done()
	end
	ticks = ticks + 1
end

function continue()
	if isDone then return end
	if ticks % ( V2 or 50 ) == 0 then
		return
	end
	loop()
end