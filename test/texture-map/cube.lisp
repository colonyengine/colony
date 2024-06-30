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

;; TODO: Find as many representations of envmaps as possible INCLUDEING shaders
;; to render them if not in a 6 face form.

;;; --------------------------------------------------------
;; Make a texture for each face with mipmaps.
;;; --------------------------------------------------------

(define-texture-map cube-posx (:2d :unique)
  (c:mipmap (textures cube-posx-64x64))
  (c:mipmap (textures cube-posx-32x32))
  (c:mipmap (textures cube-posx-16x16))
  (c:mipmap (textures cube-posx-8x8))
  (c:mipmap (textures cube-posx-4x4))
  (c:mipmap (textures cube-posx-2x2))
  (c:mipmap (textures cube-posx-1x1)))

(define-texture-map cube-negx (:2d :unique)
  (c:mipmap (textures cube-negx-64x64))
  (c:mipmap (textures cube-negx-32x32))
  (c:mipmap (textures cube-negx-16x16))
  (c:mipmap (textures cube-negx-8x8))
  (c:mipmap (textures cube-negx-4x4))
  (c:mipmap (textures cube-negx-2x2))
  (c:mipmap (textures cube-negx-1x1)))

(define-texture-map cube-posy (:2d :unique)
  (c:mipmap (textures cube-posy-64x64))
  (c:mipmap (textures cube-posy-32x32))
  (c:mipmap (textures cube-posy-16x16))
  (c:mipmap (textures cube-posy-8x8))
  (c:mipmap (textures cube-posy-4x4))
  (c:mipmap (textures cube-posy-2x2))
  (c:mipmap (textures cube-posy-1x1)))

(define-texture-map cube-negy (:2d :unique)
  (c:mipmap (textures cube-negy-64x64))
  (c:mipmap (textures cube-negy-32x32))
  (c:mipmap (textures cube-negy-16x16))
  (c:mipmap (textures cube-negy-8x8))
  (c:mipmap (textures cube-negy-4x4))
  (c:mipmap (textures cube-negy-2x2))
  (c:mipmap (textures cube-negy-1x1)))

(define-texture-map cube-posz (:2d :unique)
  (c:mipmap (textures cube-posz-64x64))
  (c:mipmap (textures cube-posz-32x32))
  (c:mipmap (textures cube-posz-16x16))
  (c:mipmap (textures cube-posz-8x8))
  (c:mipmap (textures cube-posz-4x4))
  (c:mipmap (textures cube-posz-2x2))
  (c:mipmap (textures cube-posz-1x1)))

(define-texture-map cube-negz (:2d :unique)
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
(define-texture-map g000-cube-log-inf-one-non (:cube :unique :six)
  (c:face (c:dir :+x) cube-posx)
  (c:face (c:dir :-x) cube-negx)
  (c:face (c:dir :+y) cube-posy)
  (c:face (c:dir :-y) cube-negy)
  (c:face (c:dir :+z) cube-posz)
  (c:face (c:dir :-z) cube-negz))
;; |
;; | gets converted to this physical form.
;; |
;; v
(define-texture-map g000-cube-phy-inf-one-non (:cube :unique :six)
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

;;; --------------------------------------------------------
;; Group: 001 (Envmap and one mipmap of it)
;; Name: cube
;; Model (:cube :envmap :hcross)
;;; --------------------------------------------------------

(define-texture-map g001-cube-log-inf-one-non (:cube :envmap :hcross)
  (c:mipmap (textures cube-hcross-256x192)))
;; |
;; | gets converted to this early physical form.
;; |
;; v
(define-texture-map g001-cube-phy-inf-one-non (:cube :unique :six)
  (c:data-elements
   (0 (c:image-element :logloc (textures cube-hcross-256x192))))
  (c:cube
   (c:envmap
    (c:mipmap
     :extent (c:span-2d)
     (c:mapping-span-2d :to (c:data-span-2d)
                        :from (c:data-span-2d :elidx 0))))))
;; |
;; | When the image header gets read and the rest filled in. NOTE that this
;; | grounded physical form is not ready for use by the GPU until we convert
;; | it via some means (cpu code or compute shader) to a cube of 6 faces.
;; |
;; v
(define-texture-map g001-cube-phy-gnd-one-non (:cube :unique :six)
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

;;; --------------------------------------------------------
;; Group: 002 (Envmap and all mipmap of it)
;; Name: cube
;; Model (:cube :envmap :hcross)
;;; --------------------------------------------------------

;; The logical, and most often human written, form.
(define-texture-map g002-cube-log-inf-all-non (:cube :envmap :hcross)
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
(define-texture-map g002-cube-phy-inf-all-non (:cube :unique :six)
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
    (c:mipmap
     :extent (c:span-2d)
     (c:mapping-span-2d :to (c:data-span-2d)
                        :from (c:data-span-2d :elidx 0)))
    (c:mipmap
     :extent (c:span-2d)
     (c:mapping-span-2d :to (c:data-span-2d)
                        :from (c:data-span-2d :elidx 1)))
    (c:mipmap
     :extent (c:span-2d)
     (c:mapping-span-2d :to (c:data-span-2d)
                        :from (c:data-span-2d :elidx 2)))
    (c:mipmap
     :extent (c:span-2d)
     (c:mapping-span-2d :to (c:data-span-2d)
                        :from (c:data-span-2d :elidx 3)))
    (c:mipmap
     :extent (c:span-2d)
     (c:mapping-span-2d :to (c:data-span-2d)
                        :from (c:data-span-2d :elidx 4)))
    (c:mipmap
     :extent (c:span-2d)
     (c:mapping-span-2d :to (c:data-span-2d)
                        :from (c:data-span-2d :elidx 5)))
    (c:mipmap
     :extent (c:span-2d)
     (c:mapping-span-2d :to (c:data-span-2d)
                        :from (c:data-span-2d :elidx 6))))))
;; |
;; | When the image header gets read and the rest filled in. NOTE that this
;; | grounded physical form is not ready for use by the GPU until we convert
;; | it via some means (cpu code or compute shader) to a cube of 6 faces.
;; |
;; v
(define-texture-map g002-cube-phy-gnd-all-non (:cube :unique :six)
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
    (c:mipmap
     :extent (c:span-2d :origin (v2:vec 0f0 0f0)
                        :extent (v2:vec 256f0 192f0))
     (c:mapping-span-2d :to (c:data-span-2d :origin (v2:vec 0f0 0f0)
                                            :extent (v2:vec 256f0 192f0))
                        :from (c:data-span-2d :origin (v2:vec 0f0 0f0)
                                              :extent (v2:vec 256f0 192f0)
                                              :elidx 0)))
    (c:mipmap
     :extent (c:span-2d :origin (v2:vec 0f0 0f0)
                        :extent (v2:vec 128f0 96f0))
     (c:mapping-span-2d :to (c:data-span-2d :origin (v2:vec 0f0 0f0)
                                            :extent (v2:vec 128f0 96f0))
                        :from (c:data-span-2d :origin (v2:vec 0f0 0f0)
                                              :extent (v2:vec 128f0 96f0)
                                              :elidx 1)))
    (c:mipmap
     :extent (c:span-2d :origin (v2:vec 0f0 0f0)
                        :extent (v2:vec 64f0 48f0))
     (c:mapping-span-2d :to (c:data-span-2d :origin (v2:vec 0f0 0f0)
                                            :extent (v2:vec 64f0 48f0))
                        :from (c:data-span-2d :origin (v2:vec 0f0 0f0)
                                              :extent (v2:vec 64f0 48f0)
                                              :elidx 2)))
    (c:mipmap
     :extent (c:span-2d :origin (v2:vec 0f0 0f0)
                        :extent (v2:vec 32f0 24f0))
     (c:mapping-span-2d :to (c:data-span-2d :origin (v2:vec 0f0 0f0)
                                            :extent (v2:vec 32f0 24f0))
                        :from (c:data-span-2d :origin (v2:vec 0f0 0f0)
                                              :extent (v2:vec 32f0 24f0)
                                              :elidx 3)))

    (c:mipmap
     :extent (c:span-2d :origin (v2:vec 0f0 0f0)
                        :extent (v2:vec 16f0 12f0))
     (c:mapping-span-2d :to (c:data-span-2d :origin (v2:vec 0f0 0f0)
                                            :extent (v2:vec 16f0 12f0))
                        :from (c:data-span-2d :origin (v2:vec 0f0 0f0)
                                              :extent (v2:vec 16f0 12f0)
                                              :elidx 4)))
    (c:mipmap
     :extent (c:span-2d :origin (v2:vec 0f0 0f0)
                        :extent (v2:vec 8f0 6f0))
     (c:mapping-span-2d :to (c:data-span-2d :origin (v2:vec 0f0 0f0)
                                            :extent (v2:vec 8f0 6f0))
                        :from (c:data-span-2d :origin (v2:vec 0f0 0f0)
                                              :extent (v2:vec 8f0 6f0)
                                              :elidx 5)))
    (c:mipmap
     :extent (c:span-2d :origin (v2:vec 0f0 0f0)
                        :extent (v2:vec 4f0 3f0))
     (c:mapping-span-2d :to (c:data-span-2d :origin (v2:vec 0f0 0f0)
                                            :extent (v2:vec 4f0 3f0))
                        :from (c:data-span-2d :origin (v2:vec 0f0 0f0)
                                              :extent (v2:vec 4f0 3f0)
                                              :elidx 6)))
    )))
