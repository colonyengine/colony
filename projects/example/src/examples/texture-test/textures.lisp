(in-package :first-light.example)

(define-texture texture-test/1d-gradient (:texture-1d fl.textures:clamp-all-edges)
  (:data #((:texture-test-textures "texture-gradient-1d.tiff"))))

(define-texture texture-test/2d-wood (:texture-2d fl.textures:clamp-all-edges)
  (:data #((:texture-test-textures "wood.tiff"))))

(define-texture texture-test/3d-testpat (:texture-3d fl.textures:clamp-all-edges)
  ;; TODO: Currently, these are the only valid origin and slices values. They
  ;; directly match the default of opengl.
  (:layout `((:origin :left-back-bottom)
             (:shape (:slices :back-to-front))))
  ;; TODO: Maybe I shuld implement pattern specification of mipmaps.
  (:data #(;; Mipmap Level 0
           #(;; Back slice at index 0, front slice at index 7
             (:3d-test "slice_0-mip_0.tiff")
             (:3d-test "slice_1-mip_0.tiff")
             (:3d-test "slice_2-mip_0.tiff")
             (:3d-test "slice_3-mip_0.tiff")
             (:3d-test "slice_4-mip_0.tiff")
             (:3d-test "slice_5-mip_0.tiff")
             (:3d-test "slice_6-mip_0.tiff")
             (:3d-test "slice_7-mip_0.tiff"))
           ;; Mipmap Level 1
           #((:3d-test "slice_0-mip_1.tiff")
             (:3d-test "slice_1-mip_1.tiff")
             (:3d-test "slice_2-mip_1.tiff")
             (:3d-test "slice_3-mip_1.tiff"))
           ;; Mipmap Level 2
           #((:3d-test "slice_0-mip_2.tiff")
             (:3d-test "slice_1-mip_2.tiff"))
           ;; Mipmap Level 3
           #((:3d-test "slice_0-mip_3.tiff")))))

(define-texture texture-test/1d-array-testpat (:texture-1d-array fl.textures:clamp-all-edges)
  ;; If there are multiple images in each list, they are mipmaps.
  ;; Since this is a test, each mip_0 image is 8 width x 1 height
  (:data #(;; Image 0. First image in array contains its mipmaps.
           #((:1da-test "redline-mip_0.tiff")
             (:1da-test "redline-mip_1.tiff")
             (:1da-test "redline-mip_2.tiff")
             (:1da-test "redline-mip_3.tiff"))
           ;; Image 1, etc.
           #((:1da-test "greenline-mip_0.tiff")
             (:1da-test "greenline-mip_1.tiff")
             (:1da-test "greenline-mip_2.tiff")
             (:1da-test "greenline-mip_3.tiff"))
           ;; Image 2, etc
           #((:1da-test "blueline-mip_0.tiff")
             (:1da-test "blueline-mip_1.tiff")
             (:1da-test "blueline-mip_2.tiff")
             (:1da-test "blueline-mip_3.tiff"))
           ;; Image 3, etc
           #((:1da-test "whiteline-mip_0.tiff")
             (:1da-test "whiteline-mip_1.tiff")
             (:1da-test "whiteline-mip_2.tiff")
             (:1da-test "whiteline-mip_3.tiff")))))

(define-texture texture-test/2d-array-testarray (:texture-2d-array fl.textures:clamp-all-edges)
  ;; Since this is a test, each mip_0 image is 1024x1024 and has 11 mipmaps.
  (:data #(;; Layer 0. First image in array contains its mipmaps.
           #((:2da-test "bluefur-mip_0.tiff")
             (:2da-test "bluefur-mip_1.tiff")
             (:2da-test "bluefur-mip_2.tiff")
             (:2da-test "bluefur-mip_3.tiff")
             (:2da-test "bluefur-mip_4.tiff")
             (:2da-test "bluefur-mip_5.tiff")
             (:2da-test "bluefur-mip_6.tiff")
             (:2da-test "bluefur-mip_7.tiff")
             (:2da-test "bluefur-mip_8.tiff")
             (:2da-test "bluefur-mip_9.tiff")
             (:2da-test "bluefur-mip_10.tiff"))
           ;; Layer 1, etc.
           #((:2da-test "bark-mip_0.tiff")
             (:2da-test "bark-mip_1.tiff")
             (:2da-test "bark-mip_2.tiff")
             (:2da-test "bark-mip_3.tiff")
             (:2da-test "bark-mip_4.tiff")
             (:2da-test "bark-mip_5.tiff")
             (:2da-test "bark-mip_6.tiff")
             (:2da-test "bark-mip_7.tiff")
             (:2da-test "bark-mip_8.tiff")
             (:2da-test "bark-mip_9.tiff")
             (:2da-test "bark-mip_10.tiff"))
           ;; Layer 2, etc
           #((:2da-test "rock-mip_0.tiff")
             (:2da-test "rock-mip_1.tiff")
             (:2da-test "rock-mip_2.tiff")
             (:2da-test "rock-mip_3.tiff")
             (:2da-test "rock-mip_4.tiff")
             (:2da-test "rock-mip_5.tiff")
             (:2da-test "rock-mip_6.tiff")
             (:2da-test "rock-mip_7.tiff")
             (:2da-test "rock-mip_8.tiff")
             (:2da-test "rock-mip_9.tiff")
             (:2da-test "rock-mip_10.tiff"))
           ;; Layer 3, etc
           #((:2da-test "wiggles-mip_0.tiff")
             (:2da-test "wiggles-mip_1.tiff")
             (:2da-test "wiggles-mip_2.tiff")
             (:2da-test "wiggles-mip_3.tiff")
             (:2da-test "wiggles-mip_4.tiff")
             (:2da-test "wiggles-mip_5.tiff")
             (:2da-test "wiggles-mip_6.tiff")
             (:2da-test "wiggles-mip_7.tiff")
             (:2da-test "wiggles-mip_8.tiff")
             (:2da-test "wiggles-mip_9.tiff")
             (:2da-test "wiggles-mip_10.tiff")))))

(define-texture texture-test/testcubemap (:texture-cube-map)
  (:data
   ;; First and only allowable index is entire group of faces. Each array for
   ;; each face can have mipmaps and all faces must have the same number of
   ;; mipmaps.
   ;; TODO: Only :six (individual images) is supported currently.
   #(((:layout :six) ;; :equirectangular, :skybox, etc, etc.
      ;; Next form is whatever it is I need to describe via the layout type.
      #((:+x #((:cubemap-test "right-mip_0.tiff")))
        (:-x #((:cubemap-test "left-mip_0.tiff")))
        (:+y #((:cubemap-test "top-mip_0.tiff")))
        (:-y #((:cubemap-test "bottom-mip_0.tiff")))
        (:+z #((:cubemap-test "back-mip_0.tiff")))
        (:-z #((:cubemap-test "front-mip_0.tiff"))))))))

(define-texture texture-test/testcubemaparray (:texture-cube-map-array)
  (:data
   ;; First cube map. mipmaps allowed.
   #(((:layout :six)
      #((:+x #((:cubemaparray-test "sea/right-mip_0.tiff")))
        (:-x #((:cubemaparray-test "sea/left-mip_0.tiff")))
        (:+y #((:cubemaparray-test "sea/top-mip_0.tiff")))
        (:-y #((:cubemaparray-test "sea/bottom-mip_0.tiff")))
        (:+z #((:cubemaparray-test "sea/back-mip_0.tiff")))
        (:-z #((:cubemaparray-test "sea/front-mip_0.tiff")))))

     ;; Second cube map, mipmaps allowed
     ((:layout :six)
      #((:+x #((:cubemaparray-test "sea/right-mip_0.tiff")))
        (:-x #((:cubemaparray-test "sea/left-mip_0.tiff")))
        (:+y #((:cubemaparray-test "sea/top-mip_0.tiff")))
        (:-y #((:cubemaparray-test "sea/bottom-mip_0.tiff")))
        (:+z #((:cubemaparray-test "sea/back-mip_0.tiff")))
        (:-z #((:cubemaparray-test "sea/front-mip_0.tiff")))))

     ;; and so on....
     )))

;; TODO: Convert the stuff after this line up into the above sections.

;;(define-texture test/texture-2d (:texture-2d)
;;  (:data (vector (general-data-descriptor ....))))

#++(define-texture test/texture-3d (:texture-3d)
     ;; Second: an entire volume of data in a file, expected to fill from bottom
     ;; left of the cube, then first sheet to upper right, repeat for each sheet
     ;; going up in the cube. Got any medical data? :)
     (:data (general-data-format-descriptor
             :width 256 :height 256 :depth 256
             :internal-format :rgba8
             :pixel-format :rgba :pixel-type :unsigned-byte
             ;; If :data is :empty, then make empty space on the GPU likely
             ;; to be used as a target or filled in later. If there are
             ;; multiple data, they are mipmaps and must have the correct
             ;; size when read.
             :data #("data/test/3d-volume.dat"))))

#++(define-texture test/texture-rectangle (:texture-rectangle)
     ;; the data is stored in a traditional image. no mipmaps allowed
     (:data #("data/test/test-texture-rectangle.tiff")))

#++(define-texture test/texture-rectangle-data (:texture-rectangle)
     ;; data form of the rectangle data, no mipmaps allowed.
     (:data (general-data-format-descriptor
             :width 256 :height 256
             :internal-format :rgba8
             :pixel-format :rgba :pixel-type :unsigned-byte
             :data #("data/test/planar.dat"))))

#++(define-texture test/texture-2d-array (:texture-2d-array)
     ;; If there are multiple images in each list, they are mipmaps.
     (:data #(("data/test/test-texture-2d-array-0.tiff")
              ("data/test/test-texture-2d-array-1.tiff")
              ;; ...
              ("data/test/test/texture-2d-array-N.tiff"))))

;; Implement this later.
#++(define-texture test/texture-buffer (:texture-buffer)
     nil)

;; TODO: Implement multi-sample stuff later, when I learn how it works.
