(in-package #:colony.test)

;;;; --------------------------------------------------------------------------
;;;; Tests of Logical and Physical Texture-map 1D DSL forms.
;;;; Each group should resolve to the same in memory form.
;;;; --------------------------------------------------------------------------

;; NOTE: These texture maps requires these images:
;; Single mipmaps:
;; 1d-64x1 Red
;; 1d-32x1 Orange
;; 1d-16x1 Yellow
;; 1d-8x1  Green
;; 1d-4x1  Blue
;; 1d-2x1  Indigo
;; 1d-1x1  Violet
;;
;; Combined mipmaps:
;; 1d-all-127x1 NOTE: 64x1 is reading order first, 1x1 is last.

;;; --------------------------------------------------------
;; Group: 000 (One mipmap specified in unique file)
;; Name: 1d
;; Model (:1d :unique)
;;; --------------------------------------------------------

;; logical form, 99% of texture-maps prolly look like this (or with all
;; mipmaps specified).
(c:define-texture-map g000-1d-log-inf-one-non (:1d :unqiue)
  (c:mipmap (textures 1d-64x1)))
;; |
;; | converted upon macro expansion immediately to the below physical form.
;; v
;; physical form of the above logical form with many things yet to be inferred
;; For each element: :physloc will be filled in too, but :data may or may not
;; be fully realized (meaning it could exist but hold header information
;; only).
(c:define-texture-map g000-1d-phy-inf-one-non (:1d :unique)
  (c:data-elements
   ;; The first value can be a symbol or an integer, but in memory are mapped
   ;; to integers starting from 0 and increasing by one each time in the order
   ;; of presentation in c:data-elements. The :elidx entry in the data-span-*
   ;; entries are altered to be the actual index of the data-element in the
   ;; data-elements array and the symbolic names are lost forever (at this
   ;; time) in the actual in memory data structure.
   (0 (c:image-element :logloc (textures 1d-64x1))))
  (c:mipmap-1d
   :extent (c:span-1d)
   (c:mapping-span-1d :to (c:data-span-1d)
                      :from (c:data-span-1d :elidx 0))))
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
(c:define-texture-map g000-1d-phy-gnd-one-non (:1d :unique)
  (c:data-elements
   (0 (c:image-elements :logloc (textures 1d-64x1))))
   (c:mipmap-1d
    :extent (c:span-1d :origin 0 :extent 64)
    (c:mapping-span-1d :to (c:data-span-1d :origin 0 :extent 64)
                       :from (c:data-span-1d :origin 0 :extent 64 :elidx 0))))

;;; --------------------------------------------------------
;; Group: 001 (All mipmaps specified in unique files)
;; Name: 1d
;; Model (:1d :unique)
;;; --------------------------------------------------------

(c:define-texture-map g001-1d-log-inf-all-non (:1d :unique)
  ;; USER MUST PUT THEM IN ORDER.
  (c:mipmap (textures 1d-64x1))
  (c:mipmap (textures 1d-32x1))
  (c:mipmap (textures 1d-16x1))
  (c:mipmap (textures 1d-8x1))
  (c:mipmap (textures 1d-4x1))
  (c:mipmap (textures 1d-2x1))
  (c:mipmap (textures 1d-1x1)))
;; |
;; | Converted at macro expansion time to the below. The expander simply
;; | honors the mipmap images found.
;; |
;; v
(c:define-texture-map g001-1d-phy-inf-all-non (:1d :unique)
  (c:data-elements
   (0 (c:image-element :logloc (textures 1d-64x1)))
   (1 (c:image-element :logloc (textures 1d-32x1)))
   (2 (c:image-element :logloc (textures 1d-16x1)))
   (3 (c:image-element :logloc (textures 1d-8x1)))
   (4 (c:image-element :logloc (textures 1d-4x1)))
   (5 (c:image-element :logloc (textures 1d-2x1)))
   (6 (c:image-element :logloc (textures 1d-1x1))))
  (c:mipmap-1d
   :extent (c:span-1d)
   (c:mapping-span-1d :to (c:data-span-1d)
                      :from (c:data-span-1d :elidx 0)))
  (c:mipmap-1d
   :extent (c:span-1d)
   (c:mapping-span-1d :to (c:data-span-1d)
                      :from (c:data-span-1d :elidx 1)))
  (c:mipmap-1d
   :extent (c:span-1d)
   (c:mapping-span-1d :to (c:data-span-1d)
                      :from (c:data-span-1d :elidx 2)))
  (c:mipmap-1d
   :extent (c:span-1d)
   (c:mapping-span-1d :to (c:data-span-1d)
                      :from (c:data-span-1d :elidx 3)))
  (c:mipmap-1d
   :extent (c:span-1d)
   (c:mapping-span-1d :to (c:data-span-1d)
                      :from (c:data-span-1d :elidx 4)))
  (c:mipmap-1d
   :extent (c:span-1d)
   (c:mapping-span-1d :to (c:data-span-1d)
                      :from (c:data-span-1d :elidx 5)))
  (c:mipmap-1d
   :extent (c:span-1d)
   (c:mapping-span-1d :to (c:data-span-1d)
                      :from (c:data-span-1d :elidx 6))))
;; |
;; | One or more mipmaps image headers are read and ultimately produce
;; | the following. However, in order to mark the texture-map as "complete"
;; | we need to eventually see the entire set of image headers.
;; |
;; v
(c:define-texture-map g001-1d-phy-gnd-all-non (:1d :unique)
  (c:data-elements
   (0 (c:image-element :logloc (textures 1d-64x1)))
   (1 (c:image-element :logloc (textures 1d-32x1)))
   (2 (c:image-element :logloc (textures 1d-16x1)))
   (3 (c:image-element :logloc (textures 1d-8x1)))
   (4 (c:image-element :logloc (textures 1d-4x1)))
   (5 (c:image-element :logloc (textures 1d-2x1)))
   (6 (c:image-element :logloc (textures 1d-1x1))))
  (c:mipmap-1d
   :extent (c:span-1d :origin 0 :extent 64)
   (c:mapping-span-1d :to (c:data-span-1d :origin 0 :extent 64)
                      :from (c:data-span-1d :origin 0 :extent 64 :elidx 0)))
  (c:mipmap-1d
   :extent (c:span-1d :origin 0 :extent 32)
   (c:mapping-span-1d :to (c:data-span-1d :origin 0 :extent 32)
                      :from (c:data-span-1d :origin 0 :extent 32 :elidx 1)))
  (c:mipmap-1d
   :extent (c:span-1d :origin 0 :extent 16)
   (c:mapping-span-1d :to (c:data-span-1d :origin 0 :extent 16)
                      :from (c:data-span-1d :origin 0 :extent 16 :elidx 2)))
  (c:mipmap-1d
   :extent (c:span-1d :origin 0 :extent 8)
   (c:mapping-span-1d :to (c:data-span-1d :origin 0 :extent 8)
                      :from (c:data-span-1d :origin 0 :extent 8 :elidx 3)))
  (c:mipmap-1d
   :extent (c:span-1d :origin 0 :extent 4)
   (c:mapping-span-1d :to (c:data-span-1d :origin 0 :extent 4)
                      :from (c:data-span-1d :origin 0 :extent 4 :elidx 4)))
  (c:mipmap-1d
   :extent (c:span-1d :origin 0 :extent 2)
   (c:mapping-span-1d :to (c:data-span-1d :origin 0 :extent 2)
                      :from (c:data-span-1d :origin 0 :extent 2 :elidx 5)))
  (c:mipmap-1d
   :extent (c:span-1d :origin 0 :extent 1)
   (c:mapping-span-1d :to (c:data-span-1d :origin 0 :extent 1)
                      :from (c:data-span-1d :origin 0 :extent 1 :elidx 6))))

;;; --------------------------------------------------------
;; Group: 002 (unique images but special mapping spans pack into one mipmap)
;; Name: 1d
;; Model (:1d :unique)
;; NOTE: There is no logical form for this, only a physical form can describe
;; this kind of thing. If one had left too much to be inferred here, the engine
;; would signal a condition and complain that it doesn't know what to do.
;; Hence the final form of this form is written by the human.
;;; --------------------------------------------------------


(define-texture-map g002-1d-phy-gnd-one-non (:1d :unique)
  (c:data-elements
   (0 (c:image-element :logloc (textures 1d-64x1)))
   (1 (c:image-element :logloc (textures 1d-32x1)))
   (2 (c:image-element :logloc (textures 1d-16x1)))
   (3 (c:image-element :logloc (textures 1d-8x1)))
   (4 (c:image-element :logloc (textures 1d-4x1)))
   (5 (c:image-element :logloc (textures 1d-2x1)))
   (6 (c:image-element :logloc (textures 1d-1x1))))
  (c:mipmap-1d
   :extent (c:span-1d :origin 0 :extent 127)
   (c:mapping-span-1d :to (c:data-span-1d :origin 0 :extent 64)
                      :from (c:data-span-1d :origin 0 :extent 64 :elidx 0))
   (c:mapping-span-1d :to (c:data-span-1d :origin 64 :extent 96)
                      :from (c:data-span-1d :origin 0 :extent 32 :elidx 1))
   (c:mapping-span-1d :to (c:data-span-1d :origin 96 :extent 112)
                      :from (c:data-span-1d :origin 0 :extent 16 :elidx 2))
   (c:mapping-span-1d :to (c:data-span-1d :origin 112 :extent 120)
                      :from (c:data-span-1d :origin 0 :extent 8 :elidx 3))
   (c:mapping-span-1d :to (c:data-span-1d :origin 120 :extent 124)
                      :from (c:data-span-1d :origin 0 :extent 4 :elidx 4))
   (c:mapping-span-1d :to (c:data-span-1d :origin 124 :extent 126)
                      :from (c:data-span-1d :origin 0 :extent 2 :elidx 5))
   (c:mapping-span-1d :to (c:data-span-1d :origin 126 :extent 127)
                      :from (c:data-span-1d :origin 0 :extent 1 :elidx 6))))

;;; --------------------------------------------------------
;; Group: 003 (All mipmaps specified in :combined :common file.)
;; Name: 1d
;; Model (:1d :combined :common) ;; 64x1 on left side, 1x1 on right.
;;; --------------------------------------------------------

(c:define-texture-map g003-1d-log-inf-all-non (:1d :combined :common)
  (c:mipmap (textures 1d-all-127x1)))
;; |
;; | Convert to physical form, but since we haven't read the image header
;; | we don't know how many mipmaps there are or how big they are. We can't
;; | specify any mipmaps yet!
;; v
(c:define-texture-map g003-1d-phy-inf-all-non (:1d :combined :common)
  (c:data-elements
   (0 (c:image-element :logloc (textures 1d-all-127x1)))))
;; |
;; | We've now read the image header and now know how to fill in ALL mipmaps.
;; | We still might not have read the actual image data though to validate
;; | that the image is _actually_ in :common format.
;; v
(c:define-texture-map g003-1d-phy-gnd-all-non (:1d :combined :common)
  (c:data-elements
   (0 (c:image-elements :logloc (textures 1d-all-127x1))))
  (c:mipmap-1d
   :extent (c:span-1d :origin 0 :extent 64)
   (c:mapping-span-1d :to (c:data-span-1d :origin 0 :extent 64)
                      :from (c:data-span-1d :origin 0 :extent 64
                                            :elidx 0)))
  (c:mipmap-1d
   :extent (c:span-1d :origin 0 :extent 32)
   (c:mapping-span-1d :to (c:data-span-1d :origin 0 :extent 32)
                      :from (c:data-span-1d :origin 64 :extent 96
                                            :elidx 0)))
  (c:mipmap-1d
   :extent (c:span-1d :origin 0 :extent 16)
   (c:mapping-span-1d :to (c:data-span-1d :origin 0 :extent 16)
                      :from (c:data-span-1d :origin 96 :extent 112
                                            :elidx 0)))
  (c:mipmap-1d
   :extent (c:span-1d :origin 0 :extent 8)
   (c:mapping-span-1d :to (c:data-span-1d :origin 0 :extent 8)
                      :from (c:data-span-1d :origin 112 :extent 120
                                            :elidx 0)))
  (c:mipmap-1d
   :extent (c:span-1d :origin 0 :extent 4)
   (c:mapping-span-1d :to (c:data-span-1d :origin 0 :extent 4)
                      :from (c:data-span-1d :origin 120 :extent 124
                                            :elidx 0)))
  (c:mipmap-1d
   :extent (c:span-1d :origin 0 :extent 2)
   (c:mapping-span-1d :to (c:data-span-1d :origin 0 :extent 2)
                      :from (c:data-span-1d :origin 124 :extent 126
                                            :elidx 0)))
  (c:mipmap-1d
   :extent (c:span-1d :origin 0 :extent 1)
   (c:mapping-span-1d :to (c:data-span-1d :origin 0 :extent 1)
                      :from (c:data-span-1d :origin 126 :extent 127
                                            :elidx 0))))
