(in-package #:colony.test)

;;;; --------------------------------------------------------------------------
;;;; Tests of Logical and Physical Texture-map Cube DSL forms.
;;;; --------------------------------------------------------------------------

;; NOTE: These texture maps requires these images. All of them must be square
;; and each face of the same size as another face at the same mipmap level.
;;
;; posx images have an underlined +X in white in their center.
;; cube-posx-64x64 Red
;; cube-posx-32x32 Orange
;; cube-posx-16x16 Yellow
;; cube-posx-8x8   Green
;; cube-posx-4x4   Blue
;; cube-posx-2x2   Indigo
;; cube-posx-1x1   Violet
;;
;; negx images have an underlined -X in white in their center.
;; cube-negx-64x64 Red
;; cube-negx-32x32 Orange
;; cube-negx-16x16 Yellow
;; cube-negx-8x8   Green
;; cube-negx-4x4   Blue
;; cube-negx-2x2   Indigo
;; cube-negx-1x1   Violet
;;
;; posy images have an underlined +Y in white in their center.
;; cube-posy-64x64 Red
;; cube-posy-32x32 Orange
;; cube-posy-16x16 Yellow
;; cube-posy-8x8   Green
;; cube-posy-4x4   Blue
;; cube-posy-2x2   Indigo
;; cube-posy-1x1   Violet
;;
;; negy images have an underlined -Y in white in their center.
;; cube-negy-64x64 Red
;; cube-negy-32x32 Orange
;; cube-negy-16x16 Yellow
;; cube-negy-8x8   Green
;; cube-negy-4x4   Blue
;; cube-negy-2x2   Indigo
;; cube-negy-1x1   Violet
;;
;; posz images have an underlined +Z in white in their center.
;; cube-posz-64x64 Red
;; cube-posz-32x32 Orange
;; cube-posz-16x16 Yellow
;; cube-posz-8x8   Green
;; cube-posz-4x4   Blue
;; cube-posz-2x2   Indigo
;; cube-posz-1x1   Violet
;;
;; negz images have an underlined -Z in white in their center.
;; cube-negz-64x64 Red
;; cube-negz-32x32 Orange
;; cube-negz-16x16 Yellow
;; cube-negz-8x8   Green
;; cube-negz-4x4   Blue
;; cube-negz-2x2   Indigo
;; cube-negz-1x1   Violet

;; Combined faces into a specific format with mipmaps:

;; NOTES for later:
;; (define-cube-envmap hcross (:discrete)
;;   " Y  "
;;   "xZXz"
;;   " y  ")
;; (define-cube-envmap vcross (:discrete)
;;   " Y "
;;   "xZX"
;;   " y "
;;   " z ")
;; (define-cube-envmap column (:discrete)
;;   "X"
;;   "x"
;;   "Y"
;;   "y"
;;   "Z"
;;   "z")
;; (define-cube-envmap row (:discrete)
;;   "XxYyZz")
;; (define-cube-envmap latlong (:cylindrical)
;;   "Y"
;;   "zxZX"
;;   "y")
;; (define-cube-envmap sphere (:spherical)
;;   " Y "
;;   "xZX"
;;   " y ")

;; c:hcross format
;; cube-hcross-256x192
;; cube-hcross-128x96
;; cube-hcross-64x48
;; cube-hcross-32x24
;; cube-hcross-16x12
;; cube-hcross-8x6
;; cube-hcross-4x3

;; TODO: Find as many representations of envmaps as possible INCLUDING shaders
;; to render them if not in a 6 face form.

;;; --------------------------------------------------------
;; Make a texture for each face with mipmaps.
;;; --------------------------------------------------------

(c:define-texture-map cube-posx (:2d :unique)
  (c:mipmap (textures cube-posx-64x64))
  (c:mipmap (textures cube-posx-32x32))
  (c:mipmap (textures cube-posx-16x16))
  (c:mipmap (textures cube-posx-8x8))
  (c:mipmap (textures cube-posx-4x4))
  (c:mipmap (textures cube-posx-2x2))
  (c:mipmap (textures cube-posx-1x1)))

(c:define-texture-map cube-negx (:2d :unique)
  (c:mipmap (textures cube-negx-64x64))
  (c:mipmap (textures cube-negx-32x32))
  (c:mipmap (textures cube-negx-16x16))
  (c:mipmap (textures cube-negx-8x8))
  (c:mipmap (textures cube-negx-4x4))
  (c:mipmap (textures cube-negx-2x2))
  (c:mipmap (textures cube-negx-1x1)))

(c:define-texture-map cube-posy (:2d :unique)
  (c:mipmap (textures cube-posy-64x64))
  (c:mipmap (textures cube-posy-32x32))
  (c:mipmap (textures cube-posy-16x16))
  (c:mipmap (textures cube-posy-8x8))
  (c:mipmap (textures cube-posy-4x4))
  (c:mipmap (textures cube-posy-2x2))
  (c:mipmap (textures cube-posy-1x1)))

(c:define-texture-map cube-negy (:2d :unique)
  (c:mipmap (textures cube-negy-64x64))
  (c:mipmap (textures cube-negy-32x32))
  (c:mipmap (textures cube-negy-16x16))
  (c:mipmap (textures cube-negy-8x8))
  (c:mipmap (textures cube-negy-4x4))
  (c:mipmap (textures cube-negy-2x2))
  (c:mipmap (textures cube-negy-1x1)))

(c:define-texture-map cube-posz (:2d :unique)
  (c:mipmap (textures cube-posz-64x64))
  (c:mipmap (textures cube-posz-32x32))
  (c:mipmap (textures cube-posz-16x16))
  (c:mipmap (textures cube-posz-8x8))
  (c:mipmap (textures cube-posz-4x4))
  (c:mipmap (textures cube-posz-2x2))
  (c:mipmap (textures cube-posz-1x1)))

(c:define-texture-map cube-negz (:2d :unique)
  (c:mipmap (textures cube-negz-64x64))
  (c:mipmap (textures cube-negz-32x32))
  (c:mipmap (textures cube-negz-16x16))
  (c:mipmap (textures cube-negz-8x8))
  (c:mipmap (textures cube-negz-4x4))
  (c:mipmap (textures cube-negz-2x2))
  (c:mipmap (textures cube-negz-1x1)))


;;; --------------------------------------------------------
;; Group: 000 (One mipmap specified with 6 faces)
;; Name: cube
;; Model (:cube :unique :six)
;;; --------------------------------------------------------

;; This logical form, which is almost certainly what humans will mostly write.
(c:define-texture-map g000-cube-log-inf-one-non (:cube :unique :six)
  (c:face (c:dir :+x) cube-posx)
  (c:face (c:dir :-x) cube-negx)
  (c:face (c:dir :+y) cube-posy)
  (c:face (c:dir :-y) cube-negy)
  (c:face (c:dir :+z) cube-posz)
  (c:face (c:dir :-z) cube-negz))
;; |
;; | gets converted to this grounded physical form. It is grounded because
;; | the texture map names it needs are already supplied.
;; |
;; v
(c:define-texture-map g000-cube-phy-gnd-one-non (:cube :unique :six)
  (c:data-elements
   (0 (c:texture-map-element :logloc cube-posx))
   (1 (c:texture-map-element :logloc cube-negx))
   (2 (c:texture-map-element :logloc cube-posy))
   (3 (c:texture-map-element :logloc cube-negy))
   (4 (c:texture-map-element :logloc cube-posz))
   (5 (c:texture-map-element :logloc cube-negz)))
  (c:cube
   (c:faces
    (c:face :dir :+x :elidx 0)
    (c:face :dir :-x :elidx 1)
    (c:face :dir :+y :elidx 2)
    (c:face :dir :-y :elidx 3)
    (c:face :dir :+z :elidx 4)
    (c:face :dir :-z :elidx 5))))
;; -
;; | Programmatic API of the above.
;; v
(defun test-g000-cube-imp-gnd-one-non ()
  (let* ((model :cube)
         (style :unique)
         (store :six)
         (ds0 (texmap:make-texture-map-element :logloc cube-posx))
         (ds1 (texmap:make-texture-map-element :logloc cube-negx))
         (ds2 (texmap:make-texture-map-element :logloc cube-posy))
         (ds3 (texmap:make-texture-map-element :logloc cube-negy))
         (ds4 (texmap:make-texture-map-element :logloc cube-posz))
         (ds5 (texmap:make-texture-map-element :logloc cube-negz))
         (data-elements
           (texmap:make-data-elements :encode ds0 ds1 ds2 ds3 ds4 ds5))

         (face0 (texmap:make-face :dir :+x :elidx 0))
         (face1 (texmap:make-face :dir :-x :elidx 1))
         (face2 (texmap:make-face :dir :+y :elidx 2))
         (face3 (texmap:make-face :dir :-y :elidx 3))
         (face4 (texmap:make-face :dir :+z :elidx 4))
         (face5 (texmap:make-face :dir :-z :elidx 5))
         (faces
           (texmap:make-faces :encode face0 face1 face2 face3 face4 face5))

         (representation (texmap:make-faces-representation :faces faces))
         (cube (texmap:make-cube :style style :store store
                                 :repr representation))

         (cube-map (texmap:make-texture-map-cube
                    :name 'g000-cube-imp-gnd-one-non
                    :anonymous-p nil
                    :model model
                    :style style
                    :store store
                    :data-elements data-elements
                    :cube cube
                    ;; If there are attrs(etc), they get assigned here.
                    ))))
  ;; If the storage forms have attrs, they get inherited from cube-map and
  ;; assigned here.
  cube-map)


;;; --------------------------------------------------------
;; Group: 001 (Envmap and one mipmap of it)
;; Name: cube
;; Model (:cube :envmap :hcross)
;;; --------------------------------------------------------

(c:define-texture-map g001-cube-log-inf-one-non (:cube :envmap :hcross)
  (c:mipmap (textures cube-hcross-256x192)))
;; |
;; | Gets converted to this early physical form. Unlike the :combined form in
;; | 1d, 2d, 3d, textures, we are simply declaring a single mipmap level
;; | whose faces must be picked out of this single image. Hence, we can
;; | in the transformation to the physical form immediately indicate that
;; | a single mipmap form exists (which is to be interpreted as an :hcross).
;; |
;; v
(c:define-texture-map g001-cube-phy-inf-one-non (:cube :envmap :hcross)
  (c:data-elements
   (0 (c:image-element :logloc (textures cube-hcross-256x192))))
  (c:cube
   (c:envmap
    (c:mipmap-2d
     :extent (c:span-2d)
     (c:mapping-span-2d :to (c:data-span-2d)
                        :from (c:data-span-2d :elidx 0))))))
;; |
;; | When the image header gets read and the rest filled in. NOTE that this
;; | grounded physical form is not ready for use by the GPU until we convert
;; | it via some means (cpu code or compute shader) to a cube of 6 faces.
;; | Or write a shader to explicitly do cube mapping with this texture.
;; |
;; v
(c:define-texture-map g001-cube-phy-gnd-one-non (:cube :envmap :hcross)
  (c:data-elements
   (0 (c:image-element :logloc (textures cube-hcross-256x192))))
  (c:cube
   (c:envmap
    (c:mipmap-2d
     :extent (c:span-2d :origin (v2:vec 0f0 0f0)
                        :extent (v2:vec 256f0 192f0))
     (c:mapping-span-2d :to (c:data-span-2d :origin (v2:vec 0f0 0f0)
                                            :extent (v2:vec 256f0 192f0))
                        :from (c:data-span-2d :origin (v2:vec 0f0 0f0)
                                              :extent (v2:vec 256f0 192f0)
                                              :elidx 0))))))
;; -
;; | Programmatic API of the above.
;; v
(defun test-g001-cube-imp-gnd-one-non ()
  (let* ((model :cube)
         (style :envmap)
         (store :hcross)
         (ds0 (texmap:make-image-element
               :logloc (textures cube-hcross-256x192)))
         (data-elements
           (texmap:make-data-elements :encode ds0))

         ;; mipmap 0
         (mip0-ms0-to (texmap:make-data-span-2d :origin (v2:vec 0f0 0f0)
                                                :extent (v2:vec 256f0 192f0)))
         (mip0-ms0-from (texmap:make-data-span-2d :origin (v2:vec 0f0 0f0)
                                                  :extent (v2:vec 256f0 192f0)
                                                  :elidx 0))
         (mip0-ms0 (texmap:make-mapping-span-2d :to mip0-ms0-to
                                                :from mip0-ms0-from))
         (mip0-mspans (texmap:make-mapping-spans :encode mip0-ms0))
         (mip0-extent (texmap:make-span-2d :origin (v2:vec 0f0 0f0)
                                           :extent (v2:vec 256f0 192f0)))
         (mip0 (texmap:make-mipmap-2d :extent mip0-extent
                                      :mapping-spans mip0-mspans))

         ;; create the mipmaps array
         (mipmaps (texmap:make-mipmaps :encode mip0))

         (representation (texmap:make-envmap-representation :mipmaps mipmaps))
         (cube (texmap:make-cube :style style :store store
                                 :repr representation))

         (cube-map (texmap:make-texture-map-cube
                    :name 'g001-cube-imp-gnd-one-non
                    :anonymous-p nil
                    :model model
                    :style style
                    :store store
                    :data-elements data-elements
                    :cube cube
                    ;; If there are attrs(etc), they get assigned here.
                    ))))
  ;; If the storage forms have attrs, they get inherited from cube-map and
  ;; assigned here.
  cube-map)


;;; --------------------------------------------------------
;; Group: 002 (Envmap and all mipmap of it)
;; Name: cube
;; Model (:cube :envmap :hcross)
;;; --------------------------------------------------------

;; The logical, and most often human written, form.
(c:define-texture-map g002-cube-log-inf-all-non (:cube :envmap :hcross)
  (c:mipmap (textures cube-hcross-256x192))
  (c:mipmap (textures cube-hcross-128x96))
  (c:mipmap (textures cube-hcross-64x48))
  (c:mipmap (textures cube-hcross-32x24))
  (c:mipmap (textures cube-hcross-16x12))
  (c:mipmap (textures cube-hcross-8x6))
  (c:mipmap (textures cube-hcross-4x3)))
;; |
;; | gets converted to this early physical form.
;; |
;; v
(c:define-texture-map g002-cube-phy-inf-all-non (:cube :envmap :hcross)
  (c:data-elements
   (0 (c:image-element :logloc (textures cube-hcross-256x192)))
   (1 (c:image-element :logloc (textures cube-hcross-128x96)))
   (2 (c:image-element :logloc (textures cube-hcross-64x48)))
   (3 (c:image-element :logloc (textures cube-hcross-32x24)))
   (4 (c:image-element :logloc (textures cube-hcross-16x12)))
   (5 (c:image-element :logloc (textures cube-hcross-8x6)))
   (6 (c:image-element :logloc (textures cube-hcross-4x3))))
  (c:cube
   (c:envmap
    (c:mipmap-2d
     :extent (c:span-2d)
     (c:mapping-span-2d :to (c:data-span-2d)
                        :from (c:data-span-2d :elidx 0)))
    (c:mipmap-2d
     :extent (c:span-2d)
     (c:mapping-span-2d :to (c:data-span-2d)
                        :from (c:data-span-2d :elidx 1)))
    (c:mipmap-2d
     :extent (c:span-2d)
     (c:mapping-span-2d :to (c:data-span-2d)
                        :from (c:data-span-2d :elidx 2)))
    (c:mipmap-2d
     :extent (c:span-2d)
     (c:mapping-span-2d :to (c:data-span-2d)
                        :from (c:data-span-2d :elidx 3)))
    (c:mipmap-2d
     :extent (c:span-2d)
     (c:mapping-span-2d :to (c:data-span-2d)
                        :from (c:data-span-2d :elidx 4)))
    (c:mipmap-2d
     :extent (c:span-2d)
     (c:mapping-span-2d :to (c:data-span-2d)
                        :from (c:data-span-2d :elidx 5)))
    (c:mipmap-2d
     :extent (c:span-2d)
     (c:mapping-span-2d :to (c:data-span-2d)
                        :from (c:data-span-2d :elidx 6))))))
;; |
;; | When the image header gets read and the rest filled in. NOTE that this
;; | grounded physical form is not ready for use by the GPU until we convert
;; | it via some means (cpu code or compute shader) to a cube of 6 faces.
;; |
;; v
(c:define-texture-map g002-cube-phy-gnd-all-non (:cube :envmap :hcross)
  (c:data-elements
   (0 (c:image-element :logloc (textures cube-hcross-256x192)))
   (1 (c:image-element :logloc (textures cube-hcross-128x96)))
   (2 (c:image-element :logloc (textures cube-hcross-64x48)))
   (3 (c:image-element :logloc (textures cube-hcross-32x24)))
   (4 (c:image-element :logloc (textures cube-hcross-16x12)))
   (5 (c:image-element :logloc (textures cube-hcross-8x6)))
   (6 (c:image-element :logloc (textures cube-hcross-4x3))))
  (c:cube
   (c:envmap
    (c:mipmap-2d
     :extent (c:span-2d :origin (v2:vec 0f0 0f0)
                        :extent (v2:vec 256f0 192f0))
     (c:mapping-span-2d :to (c:data-span-2d :origin (v2:vec 0f0 0f0)
                                            :extent (v2:vec 256f0 192f0))
                        :from (c:data-span-2d :origin (v2:vec 0f0 0f0)
                                              :extent (v2:vec 256f0 192f0)
                                              :elidx 0)))
    (c:mipmap-2d
     :extent (c:span-2d :origin (v2:vec 0f0 0f0)
                        :extent (v2:vec 128f0 96f0))
     (c:mapping-span-2d :to (c:data-span-2d :origin (v2:vec 0f0 0f0)
                                            :extent (v2:vec 128f0 96f0))
                        :from (c:data-span-2d :origin (v2:vec 0f0 0f0)
                                              :extent (v2:vec 128f0 96f0)
                                              :elidx 1)))
    (c:mipmap-2d
     :extent (c:span-2d :origin (v2:vec 0f0 0f0)
                        :extent (v2:vec 64f0 48f0))
     (c:mapping-span-2d :to (c:data-span-2d :origin (v2:vec 0f0 0f0)
                                            :extent (v2:vec 64f0 48f0))
                        :from (c:data-span-2d :origin (v2:vec 0f0 0f0)
                                              :extent (v2:vec 64f0 48f0)
                                              :elidx 2)))
    (c:mipmap-2d
     :extent (c:span-2d :origin (v2:vec 0f0 0f0)
                        :extent (v2:vec 32f0 24f0))
     (c:mapping-span-2d :to (c:data-span-2d :origin (v2:vec 0f0 0f0)
                                            :extent (v2:vec 32f0 24f0))
                        :from (c:data-span-2d :origin (v2:vec 0f0 0f0)
                                              :extent (v2:vec 32f0 24f0)
                                              :elidx 3)))

    (c:mipmap-2d
     :extent (c:span-2d :origin (v2:vec 0f0 0f0)
                        :extent (v2:vec 16f0 12f0))
     (c:mapping-span-2d :to (c:data-span-2d :origin (v2:vec 0f0 0f0)
                                            :extent (v2:vec 16f0 12f0))
                        :from (c:data-span-2d :origin (v2:vec 0f0 0f0)
                                              :extent (v2:vec 16f0 12f0)
                                              :elidx 4)))
    (c:mipmap-2d
     :extent (c:span-2d :origin (v2:vec 0f0 0f0)
                        :extent (v2:vec 8f0 6f0))
     (c:mapping-span-2d :to (c:data-span-2d :origin (v2:vec 0f0 0f0)
                                            :extent (v2:vec 8f0 6f0))
                        :from (c:data-span-2d :origin (v2:vec 0f0 0f0)
                                              :extent (v2:vec 8f0 6f0)
                                              :elidx 5)))
    (c:mipmap-2d
     :extent (c:span-2d :origin (v2:vec 0f0 0f0)
                        :extent (v2:vec 4f0 3f0))
     (c:mapping-span-2d :to (c:data-span-2d :origin (v2:vec 0f0 0f0)
                                            :extent (v2:vec 4f0 3f0))
                        :from (c:data-span-2d :origin (v2:vec 0f0 0f0)
                                              :extent (v2:vec 4f0 3f0)
                                              :elidx 6)))
    )))
;; -
;; | Programmatic API of the above.
;; v
(defun test-g002-cube-imp-gnd-all-non ()
  (let* ((model :cube)
         (style :envmap)
         (store :hcross)
         (ds0 (texmap:make-image-element
               :logloc (textures cube-hcross-256x192)))
         (ds1 (texmap:make-image-element
               :logloc (textures cube-hcross-128x96)))
         (ds2 (texmap:make-image-element
               :logloc (textures cube-hcross-64x48)))
         (ds3 (texmap:make-image-element
               :logloc (textures cube-hcross-32x24)))
         (ds4 (texmap:make-image-element
               :logloc (textures cube-hcross-16x24)))
         (ds5 (texmap:make-image-element
               :logloc (textures cube-hcross-8x6)))
         (ds6 (texmap:make-image-element
               :logloc (textures cube-hcross-4x3)))
         (data-elements
           (texmap:make-data-elements :encode ds0 ds1 ds2 ds3 ds4 ds5 ds6))

         ;; mipmap 0
         (mip0-ms0-to (texmap:make-data-span-2d :origin (v2:vec 0f0 0f0)
                                                :extent (v2:vec 256f0 192f0)))
         (mip0-ms0-from (texmap:make-data-span-2d :origin (v2:vec 0f0 0f0)
                                                  :extent (v2:vec 256f0 192f0)
                                                  :elidx 0))
         (mip0-ms0 (texmap:make-mapping-span-2d :to mip0-ms0-to
                                                :from mip0-ms0-from))
         (mip0-mspans (texmap:make-mapping-spans :encode mip0-ms0))
         (mip0-extent (texmap:make-span-2d :origin (v2:vec 0f0 0f0)
                                           :extent (v2:vec 256f0 192f0)))
         (mip0 (texmap:make-mipmap-2d :extent mip0-extent
                                      :mapping-spans mip0-mspans))

         ;; mipmap 1
         (mip1-ms0-to (texmap:make-data-span-2d :origin (v2:vec 0f0 0f0)
                                                :extent (v2:vec 128f0 96f0)))
         (mip1-ms0-from (texmap:make-data-span-2d :origin (v2:vec 0f0 0f0)
                                                  :extent (v2:vec 128f0 96f0)
                                                  :elidx 1))
         (mip1-ms0 (texmap:make-mapping-span-2d :to mip1-ms0-to
                                                :from mip1-ms0-from))
         (mip1-mspans (texmap:make-mapping-spans :encode mip1-ms0))
         (mip1-extent (texmap:make-span-2d :origin (v2:vec 0f0 0f0)
                                           :extent (v2:vec 128f0 96f0)))
         (mip1 (texmap:make-mipmap-2d :extent mip1-extent
                                      :mapping-spans mip1-mspans))

         ;; mipmap 2
         (mip2-ms0-to (texmap:make-data-span-2d :origin (v2:vec 0f0 0f0)
                                                :extent (v2:vec 64f0 48f0)))
         (mip2-ms0-from (texmap:make-data-span-2d :origin (v2:vec 0f0 0f0)
                                                  :extent (v2:vec 64f0 48f0)
                                                  :elidx 2))
         (mip2-ms0 (texmap:make-mapping-span-2d :to mip2-ms0-to
                                                :from mip2-ms0-from))
         (mip2-mspans (texmap:make-mapping-spans :encode mip2-ms0))
         (mip2-extent (texmap:make-span-2d :origin (v2:vec 0f0 0f0)
                                           :extent (v2:vec 64f0 48f0)))
         (mip2 (texmap:make-mipmap-2d :extent mip2-extent
                                      :mapping-spans mip2-mspans))

         ;; mipmap 3
         (mip3-ms0-to (texmap:make-data-span-2d :origin (v2:vec 0f0 0f0)
                                                :extent (v2:vec 32f0 24f0)))
         (mip3-ms0-from (texmap:make-data-span-2d :origin (v2:vec 0f0 0f0)
                                                  :extent (v2:vec 32f0 24f0)
                                                  :elidx 3))
         (mip3-ms0 (texmap:make-mapping-span-2d :to mip3-ms0-to
                                                :from mip3-ms0-from))
         (mip3-mspans (texmap:make-mapping-spans :encode mip3-ms0))
         (mip3-extent (texmap:make-span-2d :origin (v2:vec 0f0 0f0)
                                           :extent (v2:vec 32f0 24f0)))
         (mip3 (texmap:make-mipmap-2d :extent mip3-extent
                                      :mapping-spans mip3-mspans))

         ;; mipmap 4
         (mip4-ms0-to (texmap:make-data-span-2d :origin (v2:vec 0f0 0f0)
                                                :extent (v2:vec 16f0 12f0)))
         (mip4-ms0-from (texmap:make-data-span-2d :origin (v2:vec 0f0 0f0)
                                                  :extent (v2:vec 16f0 12f0)
                                                  :elidx 4))
         (mip4-ms0 (texmap:make-mapping-span-2d :to mip4-ms0-to
                                                :from mip4-ms0-from))
         (mip4-mspans (texmap:make-mapping-spans :encode mip4-ms0))
         (mip4-extent (texmap:make-span-2d :origin (v2:vec 0f0 0f0)
                                           :extent (v2:vec 16f0 12f0)))
         (mip4 (texmap:make-mipmap-2d :extent mip4-extent
                                      :mapping-spans mip4-mspans))

         ;; mipmap 5
         (mip5-ms0-to (texmap:make-data-span-2d :origin (v2:vec 0f0 0f0)
                                                :extent (v2:vec 8f0 6f0)))
         (mip5-ms0-from (texmap:make-data-span-2d :origin (v2:vec 0f0 0f0)
                                                  :extent (v2:vec 8f0 6f0)
                                                  :elidx 5))
         (mip5-ms0 (texmap:make-mapping-span-2d :to mip5-ms0-to
                                                :from mip5-ms0-from))
         (mip5-mspans (texmap:make-mapping-spans :encode mip5-ms0))
         (mip5-extent (texmap:make-span-2d :origin (v2:vec 0f0 0f0)
                                           :extent (v2:vec 8f0 6f0)))
         (mip5 (texmap:make-mipmap-2d :extent mip5-extent
                                      :mapping-spans mip5-mspans))

         ;; mipmap 6
         (mip6-ms0-to (texmap:make-data-span-2d :origin (v2:vec 0f0 0f0)
                                                :extent (v2:vec 4f0 3f0)))
         (mip6-ms0-from (texmap:make-data-span-2d :origin (v2:vec 0f0 0f0)
                                                  :extent (v2:vec 4f0 3f0)
                                                  :elidx 6))
         (mip6-ms0 (texmap:make-mapping-span-2d :to mip6-ms0-to
                                                :from mip6-ms0-from))
         (mip6-mspans (texmap:make-mapping-spans :encode mip6-ms0))
         (mip6-extent (texmap:make-span-2d :origin (v2:vec 0f0 0f0)
                                           :extent (v2:vec 4f0 3f0)))
         (mip6 (texmap:make-mipmap-2d :extent mip6-extent
                                      :mapping-spans mip6-mspans))

         ;; create the mipmaps array
         (mipmaps
           (texmap:make-mipmaps :encode mip0 mip1 mip2 mip3 mip4 mip5 mip6))

         (representation (texmap:make-envmap-representation :mipmaps mipmaps))
         (cube (texmap:make-cube :style style :store store
                                 :repr representation))

         (cube-map (texmap:make-texture-map-cube
                    :name 'g002-cube-imp-gnd-all-non
                    :anonymous-p nil
                    :model model
                    :style style
                    :store store
                    :data-elements data-elements
                    :cube cube
                    ;; If there are attrs(etc), they get assigned here.
                    ))))
  ;; If the storage forms have attrs, they get inherited from cube-map and
  ;; assigned here.
  cube-map)

;;; TODO: Maybe remove this one for now.
;;; --------------------------------------------------------
;; Group: 003 (One mipmap specified with 6 faces as immediate forms)
;; Name: cube
;; Model (:cube :unique :six)
;;; --------------------------------------------------------

;; This logical form, which is almost certainly what humans will mostly write.
(c:define-texture-map g003-cube-log-inf-one-non (:cube :unique :six)
  (c:face (c:dir :+x)
          (c:define-texture-map nil (:2d :unique)
            (c:mipmap (textures cube-posx-64x64))
            (c:mipmap (textures cube-posx-32x32))
            (c:mipmap (textures cube-posx-16x16))
            (c:mipmap (textures cube-posx-8x8))
            (c:mipmap (textures cube-posx-4x4))
            (c:mipmap (textures cube-posx-2x2))
            (c:mipmap (textures cube-posx-1x1))))
  (c:face (c:dir :-x)
          (c:define-texture-map nil (:2d :unique)
            (c:mipmap (textures cube-negx-64x64))
            (c:mipmap (textures cube-negx-32x32))
            (c:mipmap (textures cube-negx-16x16))
            (c:mipmap (textures cube-negx-8x8))
            (c:mipmap (textures cube-negx-4x4))
            (c:mipmap (textures cube-negx-2x2))
            (c:mipmap (textures cube-negx-1x1))))
  (c:face (c:dir :+y)
          (c:define-texture-map nil (:2d :unique)
            (c:mipmap (textures cube-posy-64x64))
            (c:mipmap (textures cube-posy-32x32))
            (c:mipmap (textures cube-posy-16x16))
            (c:mipmap (textures cube-posy-8x8))
            (c:mipmap (textures cube-posy-4x4))
            (c:mipmap (textures cube-posy-2x2))
            (c:mipmap (textures cube-posy-1x1))))
  (c:face (c:dir :-y)
          (c:define-texture-map nil (:2d :unique)
            (c:mipmap (textures cube-negy-64x64))
            (c:mipmap (textures cube-negy-32x32))
            (c:mipmap (textures cube-negy-16x16))
            (c:mipmap (textures cube-negy-8x8))
            (c:mipmap (textures cube-negy-4x4))
            (c:mipmap (textures cube-negy-2x2))
            (c:mipmap (textures cube-negy-1x1))))
  (c:face (c:dir :+z)
          (c:define-texture-map nil (:2d :unique)
            (c:mipmap (textures cube-posz-64x64))
            (c:mipmap (textures cube-posz-32x32))
            (c:mipmap (textures cube-posz-16x16))
            (c:mipmap (textures cube-posz-8x8))
            (c:mipmap (textures cube-posz-4x4))
            (c:mipmap (textures cube-posz-2x2))
            (c:mipmap (textures cube-posz-1x1))))
  (c:face (c:dir :-z)
          (c:define-texture-map nil (:2d :unique)
            (c:mipmap (textures cube-negz-64x64))
            (c:mipmap (textures cube-negz-32x32))
            (c:mipmap (textures cube-negz-16x16))
            (c:mipmap (textures cube-negz-8x8))
            (c:mipmap (textures cube-negz-4x4))
            (c:mipmap (textures cube-negz-2x2))
            (c:mipmap (textures cube-negz-1x1)))))
;; |
;; | The define-texture-maps are lifted to gensymed names and actual toplevel
;; | metaspace objects. Those names are put in place of the form and the cube
;; | form is processed as normal. Basically, it turns into
;; | g000-cube-log-inf-one-non but with gensym names.
;; |
;; v
;; Expansion Explained above.
