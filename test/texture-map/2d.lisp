(in-package #:colony.test)

;;;; --------------------------------------------------------------------------
;;;; Tests of Logical and Physical Texture-map 2D DSL forms.
;;;; --------------------------------------------------------------------------

;; NOTE: These texture maps requires these images:
;; Single mipmaps:
;; 2d-64x64 Red
;; 2d-32x32 Orange
;; 2d-16x16 Yellow
;; 2d-8x8   Green
;; 2d-4x4   Blue
;; 2d-2x2   Indigo
;; 2d-1x1   Violet
;;
;; Combined mipmaps:
;; 2d-all-96x64 :common format

;;; --------------------------------------------------------
;; Group: 000 (One mipmap specified in unique file)
;; Name: 2d
;; Model (:2d :unique)
;;; --------------------------------------------------------

;; logical form, 99% of texture-maps prolly look like this (or with all
;; mipmaps specified).
(c:define-texture-map g000-2d-log-inf-one-non (:2d :unique)
  (c:mipmap (textures 2d-64x64)))
;; |
;; | converted upon macro expansion immediately to the below physical form.
;; v
;; physical form of the above logical form with many things yet to be inferred
;; For each element: :physloc will be filled in too, but :data may or may not
;; be fully realized (meaning it could exist but hold header information
;; only).
(c:define-texture-map g000-2d-phy-inf-one-non (:2d :unique)
  (c:data-elements
   ;; The first value can be a symbol or an integer, but in memory are mapped
   ;; to integers starting from 0 and increasing by one each time in the order
   ;; of presentation in c:data-elements. The :elidx entry in the data-span-*
   ;; entries are altered to be the actual index of the data-element in the
   ;; data-elements array and the symbolic names are lost forever (at this
   ;; time) in the actual in memory data structure.
   (0 (c:image-element :logloc (textures 2d-64x64))))
  (c:mipmap-2d
   :extent (c:span-2d)
   (c:mapping-span-2d :to (c:data-span-2d)
                      :from (c:data-span-2d :elidx 0))))
;; |
;; | One or more, up to all, storage-forms will be filled in my reading the
;; ! headers of the images in order to get the right dimensions. The actual
;; | image data may or may not have been loaded (and unlikely to be loaded)
;; | because the style is :unique. Due to the needs of how this texture-map
;; | might be used, it is not necessarily required that all storage-forms
;; | have their image data actually loaded into main memory all at the same
;; | time.
;; v
;; Grounded (as much as possible) physical form of the above logical form.
(c:define-texture-map g000-2d-phy-gnd-one-non (:2d :unique)
  (c:data-elements
   (0 (c:image-element :logloc (textures 2d-64x64))))
  (c:mipmap-2d
   :extent (c:span-2d :origin (v2:zero)
                      :extent (v2:vec 64f0 64f0))
   (c:mapping-span-2d :to (c:data-span-2d :origin (v2:zero)
                                          :extent (v2:vec 64f0 64f0))
                      :from (c:data-span-2d :origin (v2:zero)
                                            :extent (v2:vec 64f0 64f0)
                                            :elidx 0))))

;;; --------------------------------------------------------
;; Group: 001 (All mipmaps specified in unique files)
;; Name: 2d
;; Model (:2d :unique)
;;; --------------------------------------------------------

(c:define-texture-map g001-2d-log-inf-all-non (:2d :unique)
  (c:mipmap (textures 2d-64x64))
  (c:mipmap (textures 2d-32x32))
  (c:mipmap (textures 2d-16x16))
  (c:mipmap (textures 2d-8x8))
  (c:mipmap (textures 2d-4x4))
  (c:mipmap (textures 2d-2x2))
  (c:mipmap (textures 2d-1x1)))
;; |
;; | Converted at macro expansion time to the below. The expander simply
;; | honors the mipmap images found.
;; |
;; v
(c:define-texture-map g001-2d-phy-inf-all-non (:2d :unique)
  (c:data-elements
   (0 (c:image-element :logloc (textures 2d-64x64)))
   (1 (c:image-element :logloc (textures 2d-32x32)))
   (2 (c:image-element :logloc (textures 2d-16x16)))
   (3 (c:image-element :logloc (textures 2d-8x8)))
   (4 (c:image-element :logloc (textures 2d-4x4)))
   (5 (c:image-element :logloc (textures 2d-2x2)))
   (6 (c:image-element :logloc (textures 2d-1x1))))
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
                      :from (c:data-span-2d :elidx 6))))
;; |
;; | One or more mipmaps image headers are read and ultimately produce
;; | the following. However, in order to mark the texture-map as "complete"
;; | we need to eventually see the entire set of image headers.
;; |
;; v
(c:define-texture-map g001-2d-phy-gnd-all-non (:2d :unique)
  (c:data-elements
   (0 (c:image-element :logloc (textures 2d-64x64)))
   (1 (c:image-element :logloc (textures 2d-32x32)))
   (2 (c:image-element :logloc (textures 2d-16x16)))
   (3 (c:image-element :logloc (textures 2d-8x8)))
   (4 (c:image-element :logloc (textures 2d-4x4)))
   (5 (c:image-element :logloc (textures 2d-2x2)))
   (6 (c:image-element :logloc (textures 2d-1x1))))
  (c:mipmap-2d
   :extent (c:span-2d :origin (v2:zero)
                      :extent (v2:vec 64f0 64f0))
   (c:mapping-span-2d :to (c:data-span-2d :origin (v2:zero)
                                          :extent (v2:vec 64f0 64f0))
                      :from (c:data-span-2d :origin (v2:zero)
                                            :extent (v2:vec 64f0 64f0)
                                            :elidx 0)))
  (c:mipmap-2d
   :extent (c:span-2d :origin (v2:zero)
                      :extent (v2:vec 32f0 32f0))
   (c:mapping-span-2d :to (c:data-span-2d :origin (v2:zero)
                                          :extent (v2:vec 32f0 32f0))
                      :from (c:data-span-2d :origin (v2:zero)
                                            :extent (v2:vec 32f0 32f0)
                                            :elidx 1)))
  (c:mipmap-2d
   :extent (c:span-2d :origin (v2:zero)
                      :extent (v2:vec 16f0 16f0))
   (c:mapping-span-2d :to (c:data-span-2d :origin (v2:zero)
                                          :extent (v2:vec 16f0 16f0))
                      :from (c:data-span-2d :origin (v2:zero)
                                            :extent (v2:vec 16f0 16f0)
                                            :elidx 2)))
  (c:mipmap-2d
   :extent (c:span-2d :origin (v2:zero)
                      :extent (v2:vec 8f0 8f0))
   (c:mapping-span-2d :to (c:data-span-2d :origin (v2:zero)
                                          :extent (v2:vec 8f0 8f0))
                      :from (c:data-span-2d :origin (v2:zero)
                                            :extent (v2:vec 8f0 8f0)
                                            :elidx 3)))
  (c:mipmap-2d
   :extent (c:span-2d :origin (v2:zero)
                      :extent (v2:vec 4f0 4f0))
   (c:mapping-span-2d :to (c:data-span-2d :origin (v2:zero)
                                          :extent (v2:vec 4f0 4f0))
                      :from (c:data-span-2d :origin (v2:zero)
                                            :extent (v2:vec 4f0 4f0)
                                            :elidx 4)))
  (c:mipmap-2d
   :extent (c:span-2d :origin (v2:zero)
                      :extent (v2:vec 2f0 2f0))
   (c:mapping-span-2d :to (c:data-span-2d :origin (v2:zero)
                                          :extent (v2:vec 2f0 2f0))
                      :from (c:data-span-2d :origin (v2:zero)
                                            :extent (v2:vec 2f0 2f0)
                                            :elidx 5)))
  (c:mipmap-2d
   :extent (c:span-2d :origin (v2:zero)
                      :extent (v2:vec 1f0 1f0))
   (c:mapping-span-2d :to (c:data-span-2d :origin (v2:zero)
                                          :extent (v2:vec 1f0 1f0))
                      :from (c:data-span-2d :origin (v2:zero)
                                            :extent (v2:vec 1f0 1f0)
                                            :elidx 6))))

;;; --------------------------------------------------------
;; Group: 003 (unique images but special mapping spans pack into one mipmap)
;; Name: 2d
;; Model (:2d :unique)
;; NOTE: There is no logical form for this, only a physical form can describe
;; this kind of thing. If one had left too much to be inferred here, the engine
;; would signal a condition and complain that it doesn't know what to do.
;; Hence the final form of this form is written by the human.
;;
;; In this, we pack each unique image into the place where it would have been
;; in a :combined :common image. There is only one mipmap.
;;; --------------------------------------------------------

(c:define-texture-map g003-2d-phy-gnd-one-non (:2d :unique)
  (c:data-elements
   (0 (c:image-element :logloc (textures 2d-64x64)))
   (1 (c:image-element :logloc (textures 2d-32x32)))
   (2 (c:image-element :logloc (textures 2d-16x16)))
   (3 (c:image-element :logloc (textures 2d-8x8)))
   (4 (c:image-element :logloc (textures 2d-4x4)))
   (5 (c:image-element :logloc (textures 2d-2x2)))
   (6 (c:image-element :logloc (textures 2d-1x1))))
  (c:mipmap-2d
   ;; TODO: This extent is not fully covered and not intended to be fully
   ;; covered. What do I do about this? What value does the untouched data
   ;; have? How do I initialize it when I allocate the storage?
   :extent (c:span-2d :origin (v2:zero)
                      :extent (v2:vec 96f0 64f0))
   (c:mapping-span-2d :to (c:data-span-2d :origin (v2:zero)
                                          :extent (v2:vec 64f0 64f0))
                      :from (c:data-span-2d :origin (v2:zero)
                                            :extent (v2:vec 64f0 64f0)
                                            :elidx 0))
   (c:mapping-span-2d :to (c:data-span-2d :origin (v2:vec 64f0 0f0)
                                          :extent (v2:vec 32f0 32f0))
                      :from (c:data-span-2d :origin (v2:zero)
                                            :extent (v2:vec 32f0 32f0)
                                            :elidx 1))
   (c:mapping-span-2d :to (c:data-span-2d :origin (v2:vec 64f0 32f0)
                                          :extent (v2:vec 16f0 16f0))
                      :from (c:data-span-2d :origin (v2:zero)
                                            :extent (v2:vec 16f0 16f0)
                                            :elidx 2))
   (c:mapping-span-2d :to (c:data-span-2d :origin (v2:vec 64f0 48f0)
                                          :extent (v2:vec 8f0 8f0))
                      :from (c:data-span-2d :origin (v2:zero)
                                            :extent (v2:vec 8f0 8f0)
                                            :elidx 3))
   (c:mapping-span-2d :to (c:data-span-2d :origin (v2:vec 64f0 56f0)
                                          :extent (v2:vec 4f0 4f0))
                      :from (c:data-span-2d :origin (v2:zero)
                                            :extent (v2:vec 4f0 4f0)
                                            :elidx 4))
   (c:mapping-span-2d :to (c:data-span-2d :origin (v2:vec 64f0 60f0)
                                          :extent (v2:vec 2f0 2f0))
                      :from (c:data-span-2d :origin (v2:zero)
                                            :extent (v2:vec 2f0 2f0)
                                            :elidx 5))
   (c:mapping-span-2d :to (c:data-span-2d :origin (v2:vec 64f0 62f0)
                                          :extent (v2:vec 1f0 1f0))
                      :from (c:data-span-2d :origin (v2:zero)
                                            :extent (v2:vec 1f0 1f0)
                                            :elidx 6))))

;;; --------------------------------------------------------
;; Group: 004 (All mipmaps specified in :combined :common file.)
;; Name: 2d
;; Model (:2d :combined :common)
;;; --------------------------------------------------------

(c:define-texture-map g004-2d-log-inf-all-non (:2d :combined :common)
  (c:mipmap (textures 2d-all-96x64)))
;; |
;; | Convert to physical form, but since we haven't read the image header
;; | we don't know how many mipmaps there are or how big they are. We can't
;; | specify any mipmaps yet!
;; v
(c:define-texture-map g004-2d-phy-inf-all-non (:2d :combined :common)
  (c:data-elements
   (0 (c:image-element :logloc (textures 2d-all-96x64)))))
;; |
;; | We've now read the image header and now know how to fill in ALL mipmaps.
;; | We still might not have read the actual image data though to validate
;; | that the image is _actually_ in the :common format.
;; v
(c:define-texture-map g004-2d-phy-gnd-all-non (:2d :combined :common)
  (c:data-elements
   (0 (c:image-element :logloc (textures 2d-all-96x64))))
  (c:mipmap-2d
   :extent (c:span-2d :origin (v2:zero)
                      :extent (v2:vec 64f0 64f0))
   (c:mapping-span-2d :to (c:data-span-2d :origin (v2:zero)
                                          :extent (v2:vec 64f0 64f0))
                      :from (c:data-span-2d :origin (v2:zero)
                                            :extent (v2:vec 64f0 64f0)
                                            :elidx 0)))
  (c:mipmap-2d
   :extent (c:span-2d :origin (v2:zero)
                      :extent (v2:vec 32f0 32f0))
   (c:mapping-span-2d :to (c:data-span-2d :origin (v2:zero)
                                          :extent (v2:vec 32f0 32f0))
                      :from (c:data-span-2d :origin (v2:vec 64f0 0f0)
                                            :extent (v2:vec 32f0 32f0)
                                            :elidx 0)))
  (c:mipmap-2d
   :extent (c:span-2d :origin (v2:zero)
                      :extent (v2:vec 16f0 16f0))
   (c:mapping-span-2d :to (c:data-span-2d :origin (v2:zero)
                                          :extent (v2:vec 16f0 16f0))
                      :from (c:data-span-2d :origin (v2:vec 64f0 32f0)
                                            :extent (v2:vec 16f0 16f0)
                                            :elidx 0)))
  (c:mipmap-2d
   :extent (c:span-2d :origin (v2:zero)
                      :extent (v2:vec 8f0 8f0))
   (c:mapping-span-2d :to (c:data-span-2d :origin (v2:zero)
                                          :extent (v2:vec 8f0 8f0))
                      :from (c:data-span-2d :origin (v2:vec 64f0 48f0)
                                            :extent (v2:vec 8f0 8f0)
                                            :elidx 0)))
  (c:mipmap-2d
   :extent (c:span-2d :origin (v2:zero)
                      :extent (v2:vec 4f0 4f0))
   (c:mapping-span-2d :to (c:data-span-2d :origin (v2:zero)
                                          :extent (v2:vec 4f0 4f0))
                      :from (c:data-span-2d :origin (v2:vec 64f0 56f0)
                                            :extent (v2:vec 4f0 4f0)
                                            :elidx 0)))
  (c:mipmap-2d
   :extent (c:span-2d :origin (v2:zero)
                      :extent (v2:vec 2f0 2f0))
   (c:mapping-span-2d :to (c:data-span-2d :origin (v2:zero)
                                          :extent (v2:vec 2f0 2f0))
                      :from (c:data-span-2d :origin (v2:vec 64f0 60f0)
                                            :extent (v2:vec 2f0 2f0)
                                            :elidx 0)))
  (c:mipmap-2d
   :extent (c:span-2d :origin (v2:zero)
                      :extent (v2:vec 1f0 1f0))
   (c:mapping-span-2d :to (c:data-span-2d :origin (v2:zero)
                                          :extent (v2:vec 1f0 1f0))
                      :from (c:data-span-2d :origin (v2:vec 64f0 62f0)
                                            :extent (v2:vec 1f0 1f0)
                                            :elidx 0))))
