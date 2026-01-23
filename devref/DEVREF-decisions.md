# Decisions

This page describes technical decisions we have made about the engine which
affect the internal structure of the engine (usually memory layout or choice of
representation of something) and how the appdev ineracts with the engine.
There are a lot of previous decisions made not written here. We'll add them as
we remember/find/need them.

## Coordinate Systems
  - Local/Object/Model Space
    - Right handed.
    - +Y is upwards.
    - +X is rightwards.
    - +Z is out of screen towards user.
    - Positive rotation is counterclockwise around axis.
  - World Space
    - Right handed.
    - +Y is upwards.
    - +X is rightwards.
    - +Z is out of screen towards user.
    - Positive rotation is counterclockwise around axis.
  - Camera/View/Eye Space
    - Right handed.
    - +Y is upwards.
    - +X is rightwards.
    - +Z is out of screen towards user. Camera at origin looking down -Z axis.
    - Positive rotation is counterclockwise around axis.
  - Clip/Frustum Space
    - Left handed.
    - Projection matrix applied, produces 4d homogeneous coordinates.
    - The Z axis flipped via sign in projection matrix. +z points behind screen.
    - All coordinates clipped to be in box from [-w, -w, -w] to [w, w, w].
  - NDC (Normalized Device Coordinates)
    - Left handed.
    - Perform perspective divide of w in clip space coordinates.
    - All coordinates in box from [-1, -1, -1] to [1, 1, 1].
    - Occurs after w divide from Clip Space's points.
    - Beneficial for Depth buffer. Small +Z near to origin, large +Z far away.
  - Viewport/Screen Space
    - Left handed.
    - 0,0 origin is lower left corner of screen/viewport.
    - +Y is upwards
    - +X is rightwards
    - +Z points behind screen. (depth values usually in depth buffer) 


  - Image Space (IN PROGRESS / KEEP GOING)
    - Note: define-texture-map DSL/ADAPI coordinates intermix in dimensionality
      up to 3D. Zero is used for the missing coordinates when not written but
      are needed. Extra dimensions are validated to be minimal in size when not
      actually needed during execution.

    - On the memory layout of texture-map data in opengl: It isn't that the
      scanlines are reversed in memory (like, the last scanline being the first
      row in the linear array of the image data) for magical reasons. It is
      because the lowest address in the linear buffer presents the first pixel
      according to the origin specification of opengl's texture uv space, and
      continuing along that row is an increasing coord in one direction, and
      down each row is increasing coord in another direction. So the entire
      concept of "flipping y" really is wrong. It is just "laying the data out
      correctly so the sampler samples the data along the correct increasing
      coordinates for each axis" that needs to be done and nothing more.

    - In texture coordinate space (as viewed in a viewer):
      - In 1D, 0 is the left side of the line.
      - In 2D, 0,0 is the lower-left hand corner of the image.
      - In 3D, 0,0,0 is the lower-left-front corner of the voxel data.
      - U or S is the horizontal x direction.
      - V or T is the vertical y direction.
      - W or P is the depth z direction.

    - 1d images, base mipmap level only, an image 1 pixel high:
      - Example (as viewed in a viewer, number is color):
        ```text
        Data:         1111111111111111 (16 pixels)
        1D UV Coords: 0 -----------> 1 (normalized)
        DSL Coords:   0 ----------> 15 (in pixels)
        ```
      - In a viewer:
        - The origin, 0, is the first texel on the left side of image.
      - In define-texture-map DSL/ADAPI:
        - The origin, 0, is the first texel on the left side of image.
      - In memory, stored as a linear array:
        - The first texel is at the lowest address.
        - The last texel is at the highest address.
      - UV coords:
        - The first texel is at 0.0 the last texel is at 1.0.

    - 1d images, mipmaps combined in a 2D image:
      - An Example (as viewed in a viewer T is transparent, number is color):
        ```text
            Data: 16 x 5 pixels, 2d image of a single 1D mipmap hierarchy:
        4 1111111111111111 mipmap level 0
        ^ 22222222TTTTTTTT mipmap level 1
        | 3333TTTTTTTTTTTT mipmap level 2
        | 44TTTTTTTTTTTTTT mipmap level 3
        | 5TTTTTTTTTTTTTTT mipmap level 4
        | 0 -----------> 1 1D UV Coords (normalized)
        0 ------------> 15 DSL Coords (in pixels)
        ```
      - In define-texture-map DSL/ADAPI:
        - The origin, 0,0, is the lower left corner of the image.
        - Engine picks out the exact 1D span of each mipmap level as it
          realizes it to the GPU.
      - In memory, stored as a linear array:
        The 5 is at the lowest address.
        ```text
        5TTTTTTTTTTTTTTT44TTTTTTTTTTTTTT3333TTTTTTTTTTTT...and so on.
        ```
      - UV coords:
        - The first texel (of each 1D span) is at 0.0 the last texel is at 1.0.
        - The first texel is on the left side of the row, last on right side.
        - For the purposes of the sampler, it will be 1D only.

    - 2d images
      - An Example (as viewed in a viewer, number is color):
        ```text
            Data: 16 x 16 pixels, 2d image
        15 1 1111222277771111
        ^  ^ 1111222277771111
        |  | 4444333333334444
        |  | 4444333333334444
        |  | 5555444444445555
        |  | 5555444444445555
        |  | 5555444444445555
        |  | 5555444444445555
        |  | 5555444444445555
        |  | 5555444444445555
        |  | 5555444444445555
        |  | 5555444444445555
        |  | 5555444444445555
        |  | 5555444444445555
        |  | 1111999911111111
        |  | 8888999988888888
        |  0 -------------> 1   2D UV Coords (normalized)
        0 ---------------> 15   DSL Coords (in pixels)
        ```
      - In define-texture-map DSL/ADAPI:
        - The origin, 0,0, is the lower left corner of the image.
      - In memory, stored as a linear array:
        - Lowest address starts here:
        ```text
        888899998888888811119999111111115555444444445555... and so on...
        ```
      - UV coords:

















