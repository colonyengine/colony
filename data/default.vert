;;; Vertex Layouts

;; Vertex layouts are the building blocks of vertex specs (explained later).
;; Each vertex layout is a list of 1 or more vertex blocks. A vertex block
;; consists of 3 specifications for divisor, block id, and a list of attribute
;; specs. Both divisor and the block id are optional as illustrated below.
;; Finally, a name is attached to a layout.

;; In the following form, the layout name '2d' is given to the layout, and it
;; defines a single block named ':mesh-data' with 2 attributes representing the
;; position and texture coordinates of a 2-dimensional mesh.
(define-vertex-layout 2d
  ((:divisor 0 ; optional, default: 0
    :block-id :mesh-data ; optional, default: :mesh-data
    :attrs ((pos :float 2)
            (uv :float 2)))))

;; This layout is just like the above, except for 3-dimensional meshes. it
;; implicitly has a divisor and a block id as mentioned.
(define-vertex-layout 3d
  ;; as per comments in 2d layout, divisor/block-id is implicit.
  ((:attrs ((pos :float 3)
            (uv :float 3)))))

;; This layout defines a :mesh-data block with *only* a color attribute. This
;; will come in handy when we talk about vertex specs later on.
(define-vertex-layout color
  ((:attrs ((color :float 4)))))

;; This layout defines just a normal mesh data attribute. Again, it will be
;; explained later.
(define-vertex-layout normal
  ((:attrs ((normal :float 3)))))

;; Let's also define a layout with 2 blocks -- one with a 'tangents' mesh-data
;; attribute, and another block that will be used for instance data (divisor 1).
(define-vertex-layout instance-test
  ((:attrs ((tangent :float 3)))
   (:divisor 1
    :block-id :test-data
    :attrs ((test-a :float 2)
            (test-b :float 3)))))

;;; Vertex Specs

;; A vertex spec is a specification with which to build a Vertex Array Object
;; (VAO). Currently, only 2 fields are given. :layouts is a list of all layouts
;; that are defined above that you want included, and :primitive specifies the
;; drawing method. :primitive is optional and defaults to :triangles.

;; Define a specification for a 3d mesh, with normal vectors and colors.
;; Remember, layouts are building blocks for describing specs. In the following
;; spec, we take 3 layouts and splice them together to create a specification
;; (similar to a class in OOP) which can be instanced to create VAO's. It is
;; important to realize that a layout block corresponds to a buffer object
;; (VBO). That is, all the :mesh-data attributes are interleaved into the same
;; buffer. If you wanted multiple buffers, you would attach attributes to
;; different block id's. Also the way layouts are spliced together is now
;; explained: We traverse the list of layouts sequentially. In the following
;; mesh spec definition, first the '3d' layout is applied to the spec. It then
;; splices in the normal layout into the same vertex buffer, since both refer to
;; the same :mesh-data block id. The resulting buffer so far looks like: (pos0
;; uv0 normal0 ... posN uvN normalN) -- we have a single interleaved array of
;; the 3d and normal mesh-data attributes. Next, we splice in the color layout
;; and it looks like: (pos0 uv0 normal0 color0 ... posN uvN normalN colorN).
;; Finally, we splice in the instance-test layout. This one is special in that
;; it both has 2 blocks instead of 1, and also introduces a new block. This
;; means first we splice in the tangent attribute into our existing mesh-data
;; buffer, and we also create a new buffer for the test attributes. The final
;; spec looks like 2 arrays of vertex data: ((pos0 uv0 normal0 color0 tangent0
;; ... posN uvN normalN colorN tangentN) (test-a0 test-b0 ... test-aN test-bN)).
;; This spec finally is instructed that the numbers contained in an instance of
;; this spec should be consumed by the shader in sets of 3, corresponding to
;; triangles to be drawn.
(define-vertex-spec mesh-spec
  (:layouts (3d normal color instance-test))
  (:primitive :triangles)) ; optional, default :triangles

;;; Meshes

;; Meshes are simply instances of VAO's created from a vertex spec and filled
;; with data. The following will create a mesh instance from the 'mesh-spec'
;; vertex spec, and upload its data to the GPU (a valid OpenGL context is
;; required once we reach this point).
(define-mesh example-mesh (mesh-spec)
  ((:mesh-data
    (((0.5 0.5 0) (1 1 0) (0 0 1) (1 0 0 1) (0 0 0)) ; pos, uv, normal, color, tangent
     ((-0.5 0.5 0) (0 1 0) (0 0 1) (0 1 0 1) (0 0 0))
     ((-0.5 -0.5 0) (0 0 0) (0 0 1) (0 0 1 1) (0 0 0))
     ((0.5 0.5 0) (1 1 0) (0 0 1) (1 0 0 1) (0 0 0))
     ((-0.5 -0.5 0) (0 0 0) (0 0 1) (0 0 1 1) (0 0 0))
     ((0.5 -0.5 0) (1 0 0) (0 0 1) (0 1 0 1) (0 0 0))))
   (:test-data
    (((0.1 0.2) (0.1 0.2 0.3)) ; test-a, test-b
     ((0.2 0.4) (0.2 0.4 0.6))
     ((0 0) (0 0 0))
     ((0 1) (0 1 2))
     ((1 2) (1 2 3))
     ((0 1) (2 3 4))))))

;;; Mesh API
;;; Various methods should be implemented on meshes.

;; Fill buffer
;; TODO define API
;; You are able to fill a buffer by name, not an integer index. That
;; is, we can upload new data for the ':test-data' buffer in the above example
;; mesh.

;; TODO describe all the rest of the methods
