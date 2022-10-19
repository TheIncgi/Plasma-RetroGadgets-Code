logs = logs or {"Logs:"}

function writeScreen( start, screenNum )
	local text = {}
	for i=1,10 do
		local msg = logs[start+i-1] or ""
		table.insert( text, msg )
		table.insert( text, "\n" )
	end
	table.remove( text )
	write_var( table.concat(text), "s"..screenNum )
end

function append()
	logs[#logs+1] = tostring(V1)
	if #logs > 100 then
		local msg = table.remove( logs, 1 )
		output( msg, 2 )
	end
	
	local firstScreen = math.floor( #logs / 10 )
	
	for s = firstScreen, 10 do
		writeScreen( s * 10, s )
	end
	
	trigger( 1 )
end

append()