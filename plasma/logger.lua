local logs = {}
function append()
	logs[#logs+1] = V1
	if #logs > 20 then
		table.remove( logs, 1 )
	end
	local t={}
	for i = math.max(1, #logs-10), #logs do
		t[#t+1] = logs[i]
		t[#t+1] = "\n"
	end
	t[#t] = nil
	output( table.concat( t ), 1 )
end