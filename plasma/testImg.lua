W, H = math.ceil( 340 / 4),  math.ceil( 240 / 4 )

if #V1 >= (W * H * 3) then
	output( V1, 2 )
else
	local data = {V1}
	local y = #V1 / 3 / W
	for i = 1, W do
		local r = math.floor( (i-1)/W * 256)
		local g = math.floor(y / H * 255)
		local b = 0 --255 - math.max(r,g)
		data[ i+1 ] = string.char( r,g,b )
	end
	output( table.concat( data ), 1 )
end

output( #V1 / W / H / 3 * 100, 3 )
output( #V1, 4 )