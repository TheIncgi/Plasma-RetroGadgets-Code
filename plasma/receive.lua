local delim = "|||"
msg = V1
local a,b = msg:find( delim, 1, true )

if a then
	msgKey = V1:sub(1,a-1)
	c,d = V1:find( delim, b+1, true )
	msgType = V1:sub(b+1, c-1)
	msgVal = V1:sub(d+1)
	
	if msgKey == tostring(V3) then
		if msgType == "boolean" then
			msgVal = msgVal=="true"
			
		elseif msgType == "number" then
		 	msgVal = tonumber( msgVal )
			
		elseif msgType == "string" then
		
		elseif msgType == "nil" then
			msgVal = nil
		else
			error("Type "..msgType.." conversion wasn't implemented :(")
		end
		output( msgVal, 1 )
	end
end --else invalid message
