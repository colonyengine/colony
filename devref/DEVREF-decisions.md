# Decisions

This page describes technical decisions we have made about the engine, or about
defaults supplied to the engine, which affect the internal structure of the
engine (usually memory layout or choice of representation of a math concept
like transform or projection matrices) and how the appdev ineracts with the
engine. These decisions are not set in stone, per se, but there should be good
reason to change them as it will affect engine code, appdev code, and
documentation describing them.

There are a lot of previous decisions made not written here. We'll add them as
we remember/find/need them. At this time, many of these decisions are hardcoded
into the engine and not configurable (for no particular reason other than time
and effort to implement the configurability or if it would contribute to appdev
confusion).

## Support's vorigin library
  - We use column vectors to represent mathematical quantities in matrices.
    Example: Transform matrix with a rotation and translation encoded into it.
    ```text
    The Y axis is: (Yx Yy Yz)
    The X axis is: (Xx Xy Xz)
    The Z axis is: (Zx Zy Zz)
    The translation is: (Tx Ty Tz)

    | Xx Yx Zx Tx |
    | Xy Yy Zy Ty |
    | Xz Yz Zz Tz |
    |  0 0 0 0  1 |
    ```
  - We use column major memory layout of the matrices (to match opengl).
    Example: The above matrix is linearly laid out into memory like this:
    ```text
    #(Xx Xy Xz 0 Yx Yy Yz 0 Zx Zy Zz 0 Tx Ty Tz 1)
    ```
  - NOTE: the above two concepts are orthogonal to each other! There are 4
    combinations of these features and we picked column vector representation
    to match the math books our math library is derived from, and column major
    layout to match how opengl wishes to accept the matrices.

  - A meaningful side effect of using column vector matrices is that when you
    want to apply a sequence of matrix transforms to a point, it applies right
    to left.
    Example:
    ```text
    p' = T5 * T4 * T3 * T2 * T1 * T0 * p

    This will apply the transforms to p in this order:

    First, apply T0 to p,
    then T1 next,
    then T2 next,
    then T3 next,
    then T4 next,
    then finally T5,
    which produces the transformed p' point.
    ```

## Coordinate Systems
  - Local/Object/Model Space
    - Right handed.
    - +Y is upwards.
    - +X is rightwards.
    - +Z is out of screen towards user.
    - Positive rotation is counterclockwise around axis when the observer is on
      axis, away from the origin, and then looking at the origin.
  - World Space
    - Right handed.
    - +Y is upwards.
    - +X is rightwards.
    - +Z is out of screen towards user.
    - Positive rotation is counterclockwise around axis when the observer is on
      axis, away from the origin, and then looking at the origin.
  - Camera/View/Eye Space
    - Right handed.
    - +Y is upwards.
    - +X is rightwards.
    - +Z is out of screen towards user. Camera at origin looking down -Z axis.
    - Positive rotation is counterclockwise around axis when the observer is on
      axis, away from the origin, and then looking at the origin.
  - Clip/Frustum Space
    - Left handed.
    - +Y is upwards.
    - +X is rightwards.
    - +Z is out of screen away from the viewer.
    - Projection matrix applied, produces 4d homogeneous coordinates.
    - The Z axis flipped via sign in projection matrix. +z points behind
      screen.
    - All coordinates clipped to be in box from [-w, -w, -w] to [w, w, w].
  - NDC (Normalized Device Coordinates)
    - Left handed.
    - +Y is upwards.
    - +X is rightwards.
    - +Z is out of screen away from the viewer.
    - Perform perspective divide of w in clip space coordinates.
    - All coordinates in box from [-1, -1, -1] to [1, 1, 1].
    - Occurs after w divide from Clip Space's points.
    - Beneficial for Depth buffer. Small +Z near to origin, large +Z far away.
  - Viewport/Screen Space
    - Left handed.
    - Units of width and height are in pixels.
    - 0,0 origin is lower left corner of screen/viewport.
    - +Y is upwards
    - +X is rightwards
    - +Z points behind screen. (depth values usually in depth buffer)
  - FrameBuffer Space (As in, rendering to a framebuffer):
    - Left handed.
    - Units of width and height are in pixels.
    - 0,0 origin is lower left corner of framebuffer.
    - +Y is upwards
    - +X is rightwards
    - +Z points behind framebuffer.
    - Coordinates in box range from [0, 0, 0] to [width, height, 1]

  - Image Space (IN PROGRESS / KEEP GOING)
    - On the memory layout of images in the Engine. 
      Since the engine currently uses opengl, opengl requires that the first
      pixel of an image presented to opengl is marked as the origin for the
      image. Since opengl (with the engine matching it) defines the lower-left
      corner of an image as the origin for picking rectangles/subrectangle from
      the image for uploading to the gpu in addition to texture coordinates, it
      means that the engine must honor a certain implementation specific layout
      detail in order to present the pixels to opengl in that manner. USUALLY,
      this means that the last row of the image data is at the lowest address
      in memory in the linear array of rows (with the next to the last row
      second in the linear array, and so on) so the ordering of the rows
      matches opengl's conception of "the first pixel of the lower-left corner
      of the image (as seen in an inage viewer) must be presented first".

      NOTE: If the engine determines it needs to reorder the image data to
      conform to the coordinate system, it will do so via GPU APIs, or CPU
      manipulation of memory, or compute shaders--whatever is the best and
      cheapest and fastest method.

    - In the ASCII image examples below:
      - A number means that pixel has some desired color (with or without an
        alpha channel) that is intended to be used by the app. Don't read too
        deeply into this--it is just for the explanations in this page.
      - A dot means a pixel has a don't care value.

    - define-texture-map DSL/ADAPI coordinate space:
      - Can be up to 3d points as required. Dimensionality can intermix as
        needed and will be checked by the engine for meaning.
      - Origin is lower-left corner of the image as seen in an image viewer.
      - A coordinate axis starts and ends on the edge of a pixel.
      - Can be written as (W H D) or (W H) or W:
        - W is coordinate in width axis.
        - H is coordinate in height axis.
        - D is coordinate in depth axis.

    - In texture coordinate space (as viewed in a viewer):
      - A coordinate axis starts and ends on the edge of a texel.
      - In 1D, 0 is the left side of the row in the image.
      - In 2D, 0,0 is the lower-left hand corner of the image.
      - In 3D, 0,0,0 is the lower-left-front corner of the voxel data.
      - U or S is the horizontal x direction.
      - V or T is the vertical y direction.
      - W or P is the depth z direction.
      - In non-"texture rectangle" cases:
        - Domain of texture coordinates in each axis is: [0, 1]
        - 0 is the "starting texel's left edge" for an axis.
        - 1 is the "ending texel's right edge" for that axis.
      - In the "texture rectangle" case:
        - Domain of texture coordinates in each axis is: [0, maxdim]
        - 0 is the "starting texel's left edge" for an axis.
        - maxdim in pixels is the "right hand side of the last texel".

    - 1d images, (single image or base mipmap level only):
      - Example, as viewed in a viewer:
        ```text
        Data:       0000000000000000 (16 pixels)
        1D U Axis:  0------------->1 (normalized)
        DSL W Axis: 0------------->15 (in pixels)
        ```

    - 1d images, mipmaps combined in a 2D image:
      - Example, one of many layouts, as viewed in a viewer:
        ```text
            Data: 16 x 5 pixels, 2d image of a single 1D mipmap hierarchy:
        4 0000000000000000 mipmap level 0 data
        ^ 11111111........ mipmap level 1 data
        | 2222............ mipmap level 2 data
        | 33.............. mipmap level 3 data
        | 4............... mipmap level 4 data
        | 0------------->1 1D U Axis (normalized in domain [0, 1])
        0-------------->15 DSL WH Axis (in pixels)
        ```

    - 2d image, (single image or base level mipmap only)
      - Example, as viewed in a viewer:
        ```text
            Data: 16 x 16 pixels, 2d image
        15 1 0000000000000000
        ^  ^ 0000000000000000
        |  | 0000000000000000
        |  | 0000000000000000
        |  | 0000000000000000
        |  | 0000000000000000
        |  | 0000000000000000
        |  | 0000000000000000
        |  | 0000000000000000
        |  | 0000000000000000
        |  | 0000000000000000
        |  | 0000000000000000
        |  | 0000000000000000
        |  | 0000000000000000
        |  | 0000000000000000
        |  | 0000000000000000
        |  0--------------->1   2D UV Axis (normalized in domain [0, 1])
        0----------------->15   DSL WH Axis (in pixels)
        ```

    - 2d image, (mipmaps combined in a 2d image):
      - Example, one of many layouts, as viewed in a viewer:
        ```text
            Data: 24 x 16 pixels, 2d image
        15 000000000000000011111111
        ^  000000000000000011111111
        |  000000000000000011111111
        |  000000000000000011111111
        |  000000000000000011111111
        |  000000000000000011111111
        |  000000000000000011111111
        |  000000000000000011111111
        |  00000000000000002222....
        |  00000000000000002222....
        |  00000000000000002222....
        |  00000000000000002222....
        |  000000000000000033......
        |  000000000000000033......
        |  00000000000000004.......
        |  0000000000000000........
        0------------------------23 DSL WH Axis (in pixels)
        ```
      - Mipmap origins and extents in DSL coordinates in above image:
        - Level 0: origin (0 0) extent (16 16)
          - DSL W axis is [0, 16)
          - DSL H axis is [0, 16)
        - Level 1: origin (8 16) extent (8 8)
          - DSL W axis is [0, 8)
          - DSL H axis is [0, 8)
        - Level 2: origin (4 16) extent (4 4)
          - DSL W axis is [0, 4)
          - DSL H axis is [0, 4)
        - Level 3: origin (2 16) extent (2 2)
          - DSL W axis is [0, 2)
          - DSL H axis is [0, 2)
        - Level 4: origin (1 16) extent (1 1)
          - DSL W axis is [0, 1)
          - DSL H axis is [0, 1)










