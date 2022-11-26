# Main
abstract3D is main
Also contains rasterization code

## Render steps:
```
For each part in an `.obj` (can be multiple objects in one .obj)
-> For each Material
  -> For each face (triangles only)
      -> transform vertex points to clip space
      -> use w to transform to screen space
      -> itterate pixels in square bounds on screen
        -> if pixel is in triangle & depth is ok
          -> convert to barycentric coords
          -> interpolate untransformed xyz, uv, normals using bary coords
          -> feed into fragment shader for color
          -> put pixel in buffer
```        


# Creating object/material data
 `.obj` and `.mtl` files from `objects/source` folder 
are converted into lua using `processObject.py` and stored in
`objects/lua`

# Other important stuff
`Camera` - has projection matrix 
`Object` - handles rendering of .obj using .mtl data (from lua version of files)
`screen` - simulates a screen/buffer that lets you set individual pixels
`linalg` - Linear Algebra library, vectors and matrices (pulled from `plasma/shader` which contains a compiler...)
