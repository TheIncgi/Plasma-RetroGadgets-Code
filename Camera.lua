local linalg = require"linalg"

local Camera = {}


function Camera:new(x,y,z,yaw,pitch,roll)
  x = x or 0
  y = y or 0
  z = z or 0
  yaw = yaw or 0
  pitch = pitch or 0
  roll = roll or 0

  local obj = {
    x=x,
    y=y,
    z=z,
    yaw=yaw,
    pitch=pitch,
    roll=roll,
    fovW=90,
    fovH=90,
    near=.1,
    far=50,
    private={
      fovW=90,
      fovH=90
    }
  }

  setmetatable(obj, self)
  return obj
end


local function cot( x )
	return 1 / math.tan( x )
end

function Camera:setFov( fov, w, h )
  local aspect = h/w
  self.private.fovH = fov
  self.private.fovW = math.deg(2*math.atan(aspect*math.tan(math.rad(self.fovH)/2)))
end

function Camera:getFov()
  return self.private.fovH
end

function Camera:getPosVec()
  return {type="vec",val={self.x, self.y, self.z},size=3}
end

function Camera:createMatrix()
  local rad = math.rad
			
		local far,near = self.far,self.near
		
		local fovW,fovH = self.private.fovW, self.private.fovH
			
		local vec,mat = linalg.vec,linalg.mat
		local rotMat = linalg.rotateMatrix
		local translate = linalg.transform
				
		-- local cam = mat(
		-- 	vec( cot(rad(fovW/2)), 0,0,0 ), 
		-- 	vec( 0, cot(rad(fovH/2)),0,0 ),
		-- 	vec( 0,0,-far/(far-near),   0 ),
		-- 	vec( 0,0,far*near/(near-far), 0)
		-- )
		local fx = cot(rad(fovH/2))
		local fy = cot(rad(fovW/2))

		local proj = mat(
			vec( fx,  0 ,0,0 ), 
			vec(  0, fy, 0,0 ),
			vec(  0,  0, (far+near)/(near-far),   (2*far*near)/(near-far) ),
			vec(  0,  0, -1, 0)
		)
		
		local view = linalg.identity(linalg.newMatrix(4,4))

		view = rotMat(view,vec(0,0,1), rad(self.roll ))
		view = rotMat(view,vec(0,0,1), rad(self.pitch))
		view = rotMat(view,vec(0,0,1), rad(self.yaw  ))
		
		view = translate(view, vec( linalg.scaleVec(self:getPosVec(),-1),0))

		return proj, view
end

function Camera:clone()
  local copy = Camera:new(self.x, self.y, self.z, self.roll, self.yaw, self.pitch)
  copy.private.fovW = self.private.fovW
  copy.private.fovH = self.private.fovH
  return copy
end

return Camera