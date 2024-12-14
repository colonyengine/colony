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
(texmap:define-texture-map g000-1d-log-inf-one-non (:1d :unique)
  (texmap:mipmap (textures 1d-64x1)))
;; |
;; | converted by logical->physical to the below texture-map dsl.
;; v
;; physical form of the above logical form with many things yet to be inferred
;; For each element: :physloc will be filled in too, but :data may or may not
;; be fully realized (meaning it could exist but hold header information
;; only).
(texmap:define-texture-map g000-1d-phy-inf-one-non (:1d :unique)
  (texmap:data-elements
   ;; The first value can be a symbol or an integer, but in memory are mapped
   ;; to integers starting from 0 and increasing by one each time in the order
   ;; of presentation in texmap:data-elements. The :elidx entry in the
   ;; data-span-* entries are altered to be the actual index of the
   ;; data-element in the data-elements array and the symbolic names are lost
   ;; forever (at this time) in the actual in memory data structure.
   (0 (texmap:image-element :logloc (textures 1d-64x1))))
  (texmap:mipmap-1d
   :extent (texmap:span-1d)
   (texmap:mapping-span-1d :to (texmap:data-span-1d)
                           :from (texmap:data-span-1d :elidx 0))))
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
(texmap:define-texture-map g000-1d-phy-gnd-one-non (:1d :unique)
  (texmap:data-elements
   (0 (texmap:image-element :logloc (textures 1d-64x1))))
  (texmap:mipmap-1d
   :extent (texmap:span-1d :origin 0 :extent 64)
   (texmap:mapping-span-1d :to (texmap:data-span-1d :origin 0 :extent 64)
                           :from (texmap:data-span-1d :origin 0 :extent 64
                                                      :elidx 0))))
;; -
;; |
;; | The same grounded form as above, but built from programmatic API.
;; |
;; v
(defun test-g000-1d-imp-gnd-one-non ()
  (let* (;; define all data elements
         (de0 (texmap:make-image-element :logloc '(textures 1d-64x1)))
         ;; create the data-elements array
         (data-elements (texmap:make-data-elements :encode de0))

         ;; define, in groups, all mapping spans for each mipmap and then the
         ;; mipmap itself.

         ;; mipmap 0
         (mip0-ms0-to (texmap:make-data-span-1d :origin 0 :extent 64))
         (mip0-ms0-from (texmap:make-data-span-1d :origin 0 :extent 64
                                                  :elidx 0))
         (mip0-ms0 (texmap:make-mapping-span-1d :to mip0-ms0-to
                                                :from mip0-ms0-from))
         (mip0-mspans (texmap:make-mapping-spans :encode mip0-ms0))

         #| KEEP GOING |#
         (mip0-extent (texmap:make-span-1d :origin 0 :extent 64))
         (mip0 (texmap:make-mipmap-1d :extent mip0-extent
                                      :mapping-spans mip0-mspans))

         ;; create the mipmaps array
         (mipmaps (texmap:make-mipmaps :encode mip0))

         ;; finally wire together the texture-map
         (texture-map (texmap:make-texture-map-1d
                       :name 'g000-1d-imp-gnd-one-non
                       :anonymous-p nil
                       :model :1d
                       :style :unique
                       :store nil
                       :data-elements data-elements
                       :mipmaps mipmaps
                       ;; If any attributes, put them here as appropriate.
                       ;; :cattrs ((...) ...)
                       )))

    ;; HOWEVER: If there were any attributes, deal with them here.  Each
    ;; storage-forms absorbs the texture-map while using :cattr (or
    ;; whatever as appropriate)

    texture-map))



;;; --------------------------------------------------------
;; Group: 001 (All mipmaps specified in unique files)
;; Name: 1d
;; Model (:1d :unique)
;;; --------------------------------------------------------

;; The logical form which is written 99% of the time by a human.
(texmap:define-texture-map g001-1d-log-inf-all-non (:1d :unique)
  ;; USER MUST PUT THEM IN ORDER.
  (texmap:mipmap (textures 1d-64x1))
  (texmap:mipmap (textures 1d-32x1))
  (texmap:mipmap (textures 1d-16x1))
  (texmap:mipmap (textures 1d-8x1))
  (texmap:mipmap (textures 1d-4x1))
  (texmap:mipmap (textures 1d-2x1))
  (texmap:mipmap (textures 1d-1x1)))
;; |
;; | Converted at macro expansion time to the below. The expander simply
;; | honors the mipmap images found.
;; |
;; v
(texmap:define-texture-map g001-1d-phy-inf-all-non (:1d :unique)
  (texmap:data-elements
   (0 (texmap:image-element :logloc (textures 1d-64x1)))
   (1 (texmap:image-element :logloc (textures 1d-32x1)))
   (2 (texmap:image-element :logloc (textures 1d-16x1)))
   (3 (texmap:image-element :logloc (textures 1d-8x1)))
   (4 (texmap:image-element :logloc (textures 1d-4x1)))
   (5 (texmap:image-element :logloc (textures 1d-2x1)))
   (6 (texmap:image-element :logloc (textures 1d-1x1))))
  (texmap:mipmap-1d
   :extent (texmap:span-1d)
   (texmap:mapping-span-1d :to (texmap:data-span-1d)
                           :from (texmap:data-span-1d :elidx 0)))
  (texmap:mipmap-1d
   :extent (texmap:span-1d)
   (texmap:mapping-span-1d :to (texmap:data-span-1d)
                           :from (texmap:data-span-1d :elidx 1)))
  (texmap:mipmap-1d
   :extent (texmap:span-1d)
   (texmap:mapping-span-1d :to (texmap:data-span-1d)
                           :from (texmap:data-span-1d :elidx 2)))
  (texmap:mipmap-1d
   :extent (texmap:span-1d)
   (texmap:mapping-span-1d :to (texmap:data-span-1d)
                           :from (texmap:data-span-1d :elidx 3)))
  (texmap:mipmap-1d
   :extent (texmap:span-1d)
   (texmap:mapping-span-1d :to (texmap:data-span-1d)
                           :from (texmap:data-span-1d :elidx 4)))
  (texmap:mipmap-1d
   :extent (texmap:span-1d)
   (texmap:mapping-span-1d :to (texmap:data-span-1d)
                           :from (texmap:data-span-1d :elidx 5)))
  (texmap:mipmap-1d
   :extent (texmap:span-1d)
   (texmap:mapping-span-1d :to (texmap:data-span-1d)
                           :from (texmap:data-span-1d :elidx 6))))
;; |
;; | One or more mipmaps image headers are read and ultimately produce
;; | the following. However, in order to mark the texture-map as "complete"
;; | we need to eventually see the entire set of image headers.
;; |
;; v
(texmap:define-texture-map g001-1d-phy-gnd-all-non (:1d :unique)
  (texmap:data-elements
   (0 (texmap:image-element :logloc (textures 1d-64x1)))
   (1 (texmap:image-element :logloc (textures 1d-32x1)))
   (2 (texmap:image-element :logloc (textures 1d-16x1)))
   (3 (texmap:image-element :logloc (textures 1d-8x1)))
   (4 (texmap:image-element :logloc (textures 1d-4x1)))
   (5 (texmap:image-element :logloc (textures 1d-2x1)))
   (6 (texmap:image-element :logloc (textures 1d-1x1))))
  (texmap:mipmap-1d
   :extent (texmap:span-1d :origin 0 :extent 64)
   (texmap:mapping-span-1d :to (texmap:data-span-1d :origin 0 :extent 64)
                           :from (texmap:data-span-1d :origin 0 :extent 64
                                                      :elidx 0)))
  (texmap:mipmap-1d
   :extent (texmap:span-1d :origin 0 :extent 32)
   (texmap:mapping-span-1d :to (texmap:data-span-1d :origin 0 :extent 32)
                           :from (texmap:data-span-1d :origin 0 :extent 32
                                                      :elidx 1)))
  (texmap:mipmap-1d
   :extent (texmap:span-1d :origin 0 :extent 16)
   (texmap:mapping-span-1d :to (texmap:data-span-1d :origin 0 :extent 16)
                           :from (texmap:data-span-1d :origin 0 :extent 16
                                                      :elidx 2)))
  (texmap:mipmap-1d
   :extent (texmap:span-1d :origin 0 :extent 8)
   (texmap:mapping-span-1d :to (texmap:data-span-1d :origin 0 :extent 8)
                           :from (texmap:data-span-1d :origin 0 :extent 8
                                                      :elidx 3)))
  (texmap:mipmap-1d
   :extent (texmap:span-1d :origin 0 :extent 4)
   (texmap:mapping-span-1d :to (texmap:data-span-1d :origin 0 :extent 4)
                           :from (texmap:data-span-1d :origin 0 :extent 4
                                                      :elidx 4)))
  (texmap:mipmap-1d
   :extent (texmap:span-1d :origin 0 :extent 2)
   (texmap:mapping-span-1d :to (texmap:data-span-1d :origin 0 :extent 2)
                           :from (texmap:data-span-1d :origin 0 :extent 2
                                                      :elidx 5)))
  (texmap:mipmap-1d
   :extent (texmap:span-1d :origin 0 :extent 1)
   (texmap:mapping-span-1d :to (texmap:data-span-1d :origin 0 :extent 1)
                           :from (texmap:data-span-1d :origin 0 :extent 1
                                                      :elidx 6))))
;; -
;; |
;; | The same grounded form as above, but built from programmatic API.
;; |
;; v
(defun test-g001-1d-imp-gnd-all-non ()
  (let* (;; define all data elements
         (de0 (texmap:make-image-element :logloc '(textures 1d-64x1)))
         (de1 (texmap:make-image-element :logloc '(textures 1d-32x1)))
         (de2 (texmap:make-image-element :logloc '(textures 1d-16x1)))
         (de3 (texmap:make-image-element :logloc '(textures 1d-8x1)))
         (de4 (texmap:make-image-element :logloc '(textures 1d-4x1)))
         (de5 (texmap:make-image-element :logloc '(textures 1d-2x1)))
         (de6 (texmap:make-image-element :logloc '(textures 1d-1x1)))
         ;; create the data-elements array
         (data-elements
           (texmap:make-data-elements :encode de0 de1 de2 de3 de4 de5 de6))

         ;; define, in groups, all mapping spans for each mipmap and then the
         ;; mipmap itself.

         ;; mipmap 0
         (mip0-ms0-to (texmap:make-data-span-1d :origin 0 :extent 64))
         (mip0-ms0-from (texmap:make-data-span-1d :origin 0 :extent 64
                                                  :elidx 0))
         (mip0-ms0 (texmap:make-mapping-span-1d :to mip0-ms0-to
                                                :from mip0-ms0-from))
         (mip0-mspans (texmap:make-mapping-spans :encode mip0-ms0))
         (mip0-extent (texmap:make-span-1d :origin 0 :extent 64))
         (mip0 (texmap:make-mipmap-1d :extent mip0-extent
                                      :mapping-spans mip0-mspans))

         ;; mipmap 1
         (mip1-ms0-to (texmap:make-data-span-1d :origin 0 :extent 32))
         (mip1-ms0-from (texmap:make-data-span-1d :origin 0 :extent 32
                                                  :elidx 1))
         (mip1-ms0 (texmap:make-mapping-span-1d :to mip1-ms0-to
                                                :from mip1-ms0-from))
         (mip1-mspans (texmap:make-mapping-spans :encode mip1-ms0))
         (mip1-extent (texmap:make-span-1d :origin 0 :extent 32))
         (mip1 (texmap:make-mipmap-1d :extent mip1-extent
                                      :mapping-spans mip1-mspans))

         ;; mipmap 2
         (mip2-ms0-to (texmap:make-data-span-1d :origin 0 :extent 16))
         (mip2-ms0-from (texmap:make-data-span-1d :origin 0 :extent 16
                                                  :elidx 2))
         (mip2-ms0 (texmap:make-mapping-span-1d :to mip2-ms0-to
                                                :from mip2-ms0-from))
         (mip2-mspans (texmap:make-mapping-spans :encode mip2-ms0))
         (mip2-extent (texmap:make-span-1d :origin 0 :extent 16))
         (mip2 (texmap:make-mipmap-1d :extent mip2-extent
                                      :mapping-spans mip2-mspans))

         ;; mipmap 3
         (mip3-ms0-to (texmap:make-data-span-1d :origin 0 :extent 8))
         (mip3-ms0-from (texmap:make-data-span-1d :origin 0 :extent 8
                                                  :elidx 3))
         (mip3-ms0 (texmap:make-mapping-span-1d :to mip3-ms0-to
                                                :from mip3-ms0-from))
         (mip3-mspans (texmap:make-mapping-spans :encode mip3-ms0))
         (mip3-extent (texmap:make-span-1d :origin 0 :extent 8))
         (mip3 (texmap:make-mipmap-1d :extent mip3-extent
                                      :mapping-spans mip3-mspans))

         ;; mipmap 4
         (mip4-ms0-to (texmap:make-data-span-1d :origin 0 :extent 4))
         (mip4-ms0-from (texmap:make-data-span-1d :origin 0 :extent 4
                                                  :elidx 4))
         (mip4-ms0 (texmap:make-mapping-span-1d :to mip4-ms0-to
                                                :from mip4-ms0-from))
         (mip4-mspans (texmap:make-mapping-spans :encode mip4-ms0))
         (mip4-extent (texmap:make-span-1d :origin 0 :extent 4))
         (mip4 (texmap:make-mipmap-1d :extent mip4-extent
                                      :mapping-spans mip4-mspans))

         ;; mipmap 5
         (mip5-ms0-to (texmap:make-data-span-1d :origin 0 :extent 2))
         (mip5-ms0-from (texmap:make-data-span-1d :origin 0 :extent 2
                                                  :elidx 5))
         (mip5-ms0 (texmap:make-mapping-span-1d :to mip5-ms0-to
                                                :from mip5-ms0-from))
         (mip5-mspans (texmap:make-mapping-spans :encode mip5-ms0))
         (mip5-extent (texmap:make-span-1d :origin 0 :extent 2))
         (mip5 (texmap:make-mipmap-1d :extent mip5-extent
                                      :mapping-spans mip5-mspans))

         ;; mipmap 6
         (mip6-ms0-to (texmap:make-data-span-1d :origin 0 :extent 1))
         (mip6-ms0-from (texmap:make-data-span-1d :origin 0 :extent 1
                                                  :elidx 6))
         (mip6-ms0 (texmap:make-mapping-span-1d :to mip6-ms0-to
                                                :from mip6-ms0-from))
         (mip6-mspans (texmap:make-mapping-spans :encode mip6-ms0))
         (mip6-extent (texmap:make-span-1d :origin 0 :extent 1))
         (mip6 (texmap:make-mipmap-1d :extent mip6-extent
                                      :mapping-spans mip6-mspans))

         ;; create the mipmaps array
         (mipmaps
           (texmap:make-mipmaps :encode mip0 mip1 mip2 mip3 mip4 mip5 mip6))

         ;; finally wire together the texture-map
         (texture-map (texmap:make-texture-map-1d
                       :name 'g001-1d-imp-gnd-all-non
                       :anonymous-p nil
                       :model :1d
                       :style :unique
                       :store nil
                       :data-elements data-elements
                       :mipmaps mipmaps
                       ;; If any attributes, put them here as appropriate.
                       ;; :cattrs ((...) ...)
                       )))

    ;; HOWEVER: If there were any attributes, deal with them here.  Each
    ;; storage-forms absorbs the texture-map while using :cattr (or whatever as
    ;; appropriate)

    texture-map))

;;; --------------------------------------------------------
;; Group: 002 (unique images but special mapping spans pack into one mipmap)
;; Name: 1d
;; Model (:1d :unique)
;; NOTE: There is no logical form for this, only a physical form can describe
;; this kind of thing. If one had left too much to be inferred here, the engine
;; would signal a condition and complain that it doesn't know what to do.
;; Hence the final form of this form is written by the human.
;;; --------------------------------------------------------


(texmap:define-texture-map g002-1d-phy-gnd-one-non (:1d :unique)
  (texmap:data-elements
   (0 (texmap:image-element :logloc (textures 1d-64x1)))
   (1 (texmap:image-element :logloc (textures 1d-32x1)))
   (2 (texmap:image-element :logloc (textures 1d-16x1)))
   (3 (texmap:image-element :logloc (textures 1d-8x1)))
   (4 (texmap:image-element :logloc (textures 1d-4x1)))
   (5 (texmap:image-element :logloc (textures 1d-2x1)))
   (6 (texmap:image-element :logloc (textures 1d-1x1))))
  (texmap:mipmap-1d
   :extent (texmap:span-1d :origin 0 :extent 127)
   (texmap:mapping-span-1d :to (texmap:data-span-1d :origin 0 :extent 64)
                           :from (texmap:data-span-1d :origin 0 :extent 64
                                                      :elidx 0))
   (texmap:mapping-span-1d :to (texmap:data-span-1d :origin 64 :extent 32)
                           :from (texmap:data-span-1d :origin 0 :extent 32
                                                      :elidx 1))
   (texmap:mapping-span-1d :to (texmap:data-span-1d :origin 96 :extent 16)
                           :from (texmap:data-span-1d :origin 0 :extent 16
                                                      :elidx 2))
   (texmap:mapping-span-1d :to (texmap:data-span-1d :origin 112 :extent 8)
                           :from (texmap:data-span-1d :origin 0 :extent 8
                                                      :elidx 3))
   (texmap:mapping-span-1d :to (texmap:data-span-1d :origin 120 :extent 4)
                           :from (texmap:data-span-1d :origin 0 :extent 4
                                                      :elidx 4))
   (texmap:mapping-span-1d :to (texmap:data-span-1d :origin 124 :extent 2)
                           :from (texmap:data-span-1d :origin 0 :extent 2
                                                      :elidx 5))
   (texmap:mapping-span-1d :to (texmap:data-span-1d :origin 126 :extent 1)
                           :from (texmap:data-span-1d :origin 0 :extent 1
                                                      :elidx 6))))
;; -
;; |
;; | The same grounded form as above, but built from programmatic API.
;; |
;; v
(defun test-g002-1d-imp-gnd-one-non ()
  (let* (;; define all data elements
         (de0 (texmap:make-image-element :logloc '(textures 1d-64x1)))
         (de1 (texmap:make-image-element :logloc '(textures 1d-32x1)))
         (de2 (texmap:make-image-element :logloc '(textures 1d-16x1)))
         (de3 (texmap:make-image-element :logloc '(textures 1d-8x1)))
         (de4 (texmap:make-image-element :logloc '(textures 1d-4x1)))
         (de5 (texmap:make-image-element :logloc '(textures 1d-2x1)))
         (de6 (texmap:make-image-element :logloc '(textures 1d-1x1)))
         ;; create the data-elements array
         (data-elements
           (texmap:make-data-elements :encode de0 de1 de2 de3 de4 de5 de6))

         ;; define, in groups, all mapping spans for each mipmap and then the
         ;; mipmap itself.

         ;; mipmap 0
         (mip0-ms0-to (texmap:make-data-span-1d :origin 0 :extent 64))
         (mip0-ms0-from (texmap:make-data-span-1d :origin 0 :extent 64
                                                  :elidx 0))
         (mip0-ms0 (texmap:make-mapping-span-1d :to mip0-ms0-to
                                                :from mip0-ms0-from))
         (mip0-ms1-to (texmap:make-data-span-1d :origin 64 :extent 32))
         (mip0-ms1-from (texmap:make-data-span-1d :origin 0 :extent 32
                                                  :elidx 1))
         (mip0-ms1 (texmap:make-mapping-span-1d :to mip0-ms1-to
                                                :from mip0-ms1-from))
         (mip0-ms2-to (texmap:make-data-span-1d :origin 96 :extent 16))
         (mip0-ms2-from (texmap:make-data-span-1d :origin 0 :extent 16
                                                  :elidx 2))
         (mip0-ms2 (texmap:make-mapping-span-1d :to mip0-ms2-to
                                                :from mip0-ms2-from))
         (mip0-ms3-to (texmap:make-data-span-1d :origin 112 :extent 8))
         (mip0-ms3-from (texmap:make-data-span-1d :origin 0 :extent 8
                                                  :elidx 3))
         (mip0-ms3 (texmap:make-mapping-span-1d :to mip0-ms3-to
                                                :from mip0-ms3-from))
         (mip0-ms4-to (texmap:make-data-span-1d :origin 120 :extent 4))
         (mip0-ms4-from (texmap:make-data-span-1d :origin 0 :extent 4
                                                  :elidx 4))
         (mip0-ms4 (texmap:make-mapping-span-1d :to mip0-ms4-to
                                                :from mip0-ms4-from))
         (mip0-ms5-to (texmap:make-data-span-1d :origin 124 :extent 2))
         (mip0-ms5-from (texmap:make-data-span-1d :origin 0 :extent 2
                                                  :elidx 5))
         (mip0-ms5 (texmap:make-mapping-span-1d :to mip0-ms5-to
                                                :from mip0-ms5-from))
         (mip0-ms6-to (texmap:make-data-span-1d :origin 126 :extent 1))
         (mip0-ms6-from (texmap:make-data-span-1d :origin 0 :extent 1
                                                  :elidx 6))
         (mip0-ms6 (texmap:make-mapping-span-1d :to mip0-ms6-to
                                                :from mip0-ms6-from))

         (mip0-mspans
           (texmap:make-mapping-spans :encode mip0-ms0 mip0-ms1 mip0-ms2
                                      mip0-ms3 mip0-ms4 mip0-ms5 mip0-ms6))

         ;; And finally the single mipmap accepting all the mapping-spans
         (mip0-extent (texmap:make-span-1d :origin 0 :extent 127))
         (mip0 (texmap:make-mipmap-1d :extent mip0-extent
                                      :mapping-spans mip0-mspans))

         ;; create the mipmaps array
         (mipmaps
           (texmap:make-mipmaps :encode mip0))

         ;; finally wire together the texture-map
         (texture-map (texmap:make-texture-map-1d
                       :name 'g002-1d-imp-gnd-one-non
                       :anonymous-p nil
                       :model :1d
                       :style :unique
                       :store nil
                       :data-elements data-elements
                       :mipmaps mipmaps
                       ;; If any attributes, put them here as appropriate.
                       ;; :cattrs ((...) ...)
                       )))

    ;; HOWEVER: If there were any attributes, deal with them here.  Each
    ;; storage-forms absorbs the texture-map while using :cattr (or whatever as
    ;; appropriate)

    texture-map))

;;; --------------------------------------------------------
;; Group: 003 (All mipmaps specified in :combined :common file.)
;; Name: 1d
;; Model (:1d :combined :common) ;; 64x1 on left side, 1x1 on right.
;;; --------------------------------------------------------

(texmap:define-texture-map g003-1d-log-inf-all-non (:1d :combined :common)
  ;; NOTE: There can only be ONE mipmap here.
  (texmap:mipmap (textures 1d-all-127x1)))
;; |
;; | Convert to physical form, but since we haven't read the image header
;; | we don't know how many mipmaps there are or how big they are. We can't
;; | specify any mipmaps yet! The engine will have to figure it out at runtime
;; | or asset packing time.
;; v
(texmap:define-texture-map g003-1d-phy-inf-all-non (:1d :combined :common)
  (texmap:data-elements
   (0 (texmap:image-element :logloc (textures 1d-all-127x1))))
  ;; NOTE: Empty here, the dsl parser expects a single image in data-elements
  ;; and will know what to do with it.
  )
;; |
;; | We've now read the image header and now know how to fill in ALL mipmaps.
;; | We still might not have read the actual image data though to validate
;; | that the image is _actually_ in :common format.
;; v
(texmap:define-texture-map g003-1d-phy-gnd-all-non (:1d :combined :common)
  (texmap:data-elements
   (0 (texmap:image-element :logloc (textures 1d-all-127x1))))
  (texmap:mipmap-1d
   :extent (texmap:span-1d :origin 0 :extent 64)
   (texmap:mapping-span-1d :to (texmap:data-span-1d :origin 0 :extent 64)
                           :from (texmap:data-span-1d :origin 0 :extent 64
                                                      :elidx 0)))
  (texmap:mipmap-1d
   :extent (texmap:span-1d :origin 0 :extent 32)
   (texmap:mapping-span-1d :to (texmap:data-span-1d :origin 0 :extent 32)
                           :from (texmap:data-span-1d :origin 64 :extent 32
                                                      :elidx 0)))
  (texmap:mipmap-1d
   :extent (texmap:span-1d :origin 0 :extent 16)
   (texmap:mapping-span-1d :to (texmap:data-span-1d :origin 0 :extent 16)
                           :from (texmap:data-span-1d :origin 96 :extent 16
                                                      :elidx 0)))
  (texmap:mipmap-1d
   :extent (texmap:span-1d :origin 0 :extent 8)
   (texmap:mapping-span-1d :to (texmap:data-span-1d :origin 0 :extent 8)
                           :from (texmap:data-span-1d :origin 112 :extent 8
                                                      :elidx 0)))
  (texmap:mipmap-1d
   :extent (texmap:span-1d :origin 0 :extent 4)
   (texmap:mapping-span-1d :to (texmap:data-span-1d :origin 0 :extent 4)
                           :from (texmap:data-span-1d :origin 120 :extent 4
                                                      :elidx 0)))
  (texmap:mipmap-1d
   :extent (texmap:span-1d :origin 0 :extent 2)
   (texmap:mapping-span-1d :to (texmap:data-span-1d :origin 0 :extent 2)
                           :from (texmap:data-span-1d :origin 124 :extent 2
                                                      :elidx 0)))
  (texmap:mipmap-1d
   :extent (texmap:span-1d :origin 0 :extent 1)
   (texmap:mapping-span-1d :to (texmap:data-span-1d :origin 0 :extent 1)
                           :from (texmap:data-span-1d :origin 126 :extent 1
                                                      :elidx 0))))
;; -
;; |
;; | The same grounded form as above, but built from programmatic API.
;; |
;; v
(defun test-g003-1d-imp-gnd-all-non ()
  (let* (;; define all data elements
         (de0 (texmap:make-image-element :logloc '(textures 1d-127x1)))
         ;; create the data-elements array
         (data-elements
           (texmap:make-data-elements :encode de0))

         ;; define, in groups, all mapping spans for each mipmap and then the
         ;; mipmap itself.

         ;; mipmap 0
         (mip0-ms0-to (texmap:make-data-span-1d :origin 0 :extent 64))
         (mip0-ms0-from (texmap:make-data-span-1d :origin 0 :extent 64
                                                  :elidx 0))
         (mip0-ms0 (texmap:make-mapping-span-1d :to mip0-ms0-to
                                                :from mip0-ms0-from))
         (mip0-mspans (texmap:make-mapping-spans :encode mip0-ms0))
         (mip0-extent (texmap:make-span-1d :origin 0 :extent 64))
         (mip0 (texmap:make-mipmap-1d :extent mip0-extent
                                      :mapping-spans mip0-mspans))

         ;; mipmap 1
         (mip1-ms0-to (texmap:make-data-span-1d :origin 0 :extent 32))
         (mip1-ms0-from (texmap:make-data-span-1d :origin 64 :extent 32
                                                  :elidx 0))
         (mip1-ms0 (texmap:make-mapping-span-1d :to mip1-ms0-to
                                                :from mip1-ms0-from))
         (mip1-mspans (texmap:make-mapping-spans :encode mip1-ms0))
         (mip1-extent (texmap:make-span-1d :origin 0 :extent 32))
         (mip1 (texmap:make-mipmap-1d :extent mip1-extent
                                      :mapping-spans mip1-mspans))

         ;; mipmap 2
         (mip2-ms0-to (texmap:make-data-span-1d :origin 0 :extent 16))
         (mip2-ms0-from (texmap:make-data-span-1d :origin 96 :extent 16
                                                  :elidx 0))
         (mip2-ms0 (texmap:make-mapping-span-1d :to mip2-ms0-to
                                                :from mip2-ms0-from))
         (mip2-mspans (texmap:make-mapping-spans :encode mip2-ms0))
         (mip2-extent (texmap:make-span-1d :origin 0 :extent 16))
         (mip2 (texmap:make-mipmap-1d :extent mip2-extent
                                      :mapping-spans mip2-mspans))

         ;; mipmap 3
         (mip3-ms0-to (texmap:make-data-span-1d :origin 0 :extent 8))
         (mip3-ms0-from (texmap:make-data-span-1d :origin 112 :extent 8
                                                  :elidx 0))
         (mip3-ms0 (texmap:make-mapping-span-1d :to mip3-ms0-to
                                                :from mip3-ms0-from))
         (mip3-mspans (texmap:make-mapping-spans :encode mip3-ms0))
         (mip3-extent (texmap:make-span-1d :origin 0 :extent 8))
         (mip3 (texmap:make-mipmap-1d :extent mip3-extent
                                      :mapping-spans mip3-mspans))

         ;; mipmap 4
         (mip4-ms0-to (texmap:make-data-span-1d :origin 0 :extent 4))
         (mip4-ms0-from (texmap:make-data-span-1d :origin 120 :extent 4
                                                  :elidx 0))
         (mip4-ms0 (texmap:make-mapping-span-1d :to mip4-ms0-to
                                                :from mip4-ms0-from))
         (mip4-mspans (texmap:make-mapping-spans :encode mip4-ms0))
         (mip4-extent (texmap:make-span-1d :origin 0 :extent 4))
         (mip4 (texmap:make-mipmap-1d :extent mip4-extent
                                      :mapping-spans mip4-mspans))

         ;; mipmap 5
         (mip5-ms0-to (texmap:make-data-span-1d :origin 0 :extent 2))
         (mip5-ms0-from (texmap:make-data-span-1d :origin 124 :extent 2
                                                  :elidx 0))
         (mip5-ms0 (texmap:make-mapping-span-1d :to mip5-ms0-to
                                                :from mip5-ms0-from))
         (mip5-mspans (texmap:make-mapping-spans :encode mip5-ms0))
         (mip5-extent (texmap:make-span-1d :origin 0 :extent 2))
         (mip5 (texmap:make-mipmap-1d :extent mip5-extent
                                      :mapping-spans mip5-mspans))

         ;; mipmap 6
         (mip6-ms0-to (texmap:make-data-span-1d :origin 0 :extent 1))
         (mip6-ms0-from (texmap:make-data-span-1d :origin 126 :extent 1
                                                  :elidx 0))
         (mip6-ms0 (texmap:make-mapping-span-1d :to mip6-ms0-to
                                                :from mip6-ms0-from))
         (mip6-mspans (texmap:make-mapping-spans :encode mip6-ms0))
         (mip6-extent (texmap:make-span-1d :origin 0 :extent 1))
         (mip6 (texmap:make-mipmap-1d :extent mip6-extent
                                      :mapping-spans mip6-mspans))

         ;; create the mipmaps array
         (mipmaps
           (texmap:make-mipmaps :encode mip0 mip1 mip2 mip3 mip4 mip5 mip6))

         ;; finally wire together the texture-map
         (texture-map (texmap:make-texture-map-1d
                       :name 'g003-1d-imp-gnd-all-non
                       :anonymous-p nil
                       :model :1d
                       :style :combined
                       :store :common
                       :data-elements data-elements
                       :mipmaps mipmaps
                       ;; If any attributes, put them here as appropriate.
                       ;; :cattrs ((...) ...)
                       )))

    ;; HOWEVER: If there were any attributes, deal with them here.  Each
    ;; storage-forms absorbs the texture-map while using :cattr (or whatever as
    ;; appropriate)

    texture-map))

;;; --------------------------------------------------------
;; Group: 004 (One mipmap specified with attributes)
;; Name: 1d
;; Model (:1d :unique)
;;; --------------------------------------------------------
(texmap:define-texture-map g004-1d-log-inf-one-ovr (:1d :unique)
  ;; use (texmap:attrs (key semantic-val computed-val) ...) for full control.
  ;; use (texmap:cattrs (key computed-val) ...) for computed attributes.
  ;; use (texmap:sattrs (key semantic-val) ...) for semantic attributes.
  (texmap:cattrs ('foo "tmap attr 0")
                 ('bar "tmap attr 1"))
  (texmap:mipmap
   (texmap:cattrs ('bar "mipmap attr 0 overlays onto tmap attr 0")
                  ('qux "mipmap attr 1"))
   (textures 1d-64x1)))
;; |
;; | converted to this physical form
;; |
;; v
(texmap:define-texture-map g004-1d-phy-inf-one-ovr (:1d :unique)
  (texmap:cattrs ('foo "tmap attr 0")
                 ('bar "tmap attr 1"))
  (texmap:data-elements
   (0 (texmap:image-element :logloc (textures 1d-64x1))))
  (texmap:mipmap-1d
   :extent (texmap:span-1d)
   (texmap:cattrs ('bar "mipmap attr 0 overlays onto tmap attr 0")
                  ('qux "mipmap attr 1"))
   (texmap:mapping-span-1d :to (texmap:data-span-1d)
                           :from (texmap:data-span-1d :elidx 0))))
;; |
;; | Then once the image header read, it is grounded into this form.
;; |
;; v
(texmap:define-texture-map g004-1d-phy-gnd-one-ovr (:1d :unique)
  (texmap:cattrs ('foo "tmap attr 0")
                 ('bar "tmap attr 1"))
  (texmap:data-elements
   (0 (texmap:image-element :logloc (textures 1d-64x1))))
  (texmap:mipmap-1d
   :extent (texmap:span-1d :origin 0 :extent 64)
   (texmap:cattrs ('bar "mip attr 0 overlays onto tmap attr 0")
                  ('qux "mip attr 1"))
   (texmap:mapping-span-1d :to (texmap:data-span-1d :origin 0 :extent 64)
                           :from (texmap:data-span-1d :origin 0 :extent 64
                                                      :elidx 0))))
;; -
;; |
;; | The same grounded form as above, but built from programmatic API.
;; |
;; v
(defun test-g004-1d-imp-gnd-one-ovr ()
  (let* (;; define all data elements
         (de0 (texmap:make-image-element :logloc '(textures 1d-64x1)))
         ;; create the data-elements array
         (data-elements (texmap:make-data-elements :encode de0))

         ;; define, in groups, all mapping spans for each mipmap and then the
         ;; mipmap itself.

         ;; mipmap 0
         (mip0-ms0-to (texmap:make-data-span-1d :origin 0 :extent 64))
         (mip0-ms0-from (texmap:make-data-span-1d :origin 0 :extent 64
                                                  :elidx 0))
         (mip0-ms0 (texmap:make-mapping-span-1d :to mip0-ms0-to
                                                :from mip0-ms0-from))
         (mip0-mspans (texmap:make-mapping-spans :encode mip0-ms0))
         (mip0-extent (texmap:make-span-1d :origin 0 :extent 64))
         (mip0 (texmap:make-mipmap-1d :extent mip0-extent
                                      :mapping-spans mip0-mspans))

         ;; create the mipmaps array
         (mipmaps (texmap:make-mipmaps :encode mip0))

         ;; finally wire together the texture-map
         (texture-map (texmap:make-texture-map-1d
                       :name 'g004-1d-imp-gnd-one-ovr
                       :anonymous-p nil
                       :model :1d
                       :style :unique
                       :store nil
                       :data-elements data-elements
                       :mipmaps mipmaps
                       :cattrs (list
                                (list 'foo "tmap attr 0")
                                (list 'bar "tmap attr 1"))
                       )))

    ;; HOWEVER: If there were any mipmap attributes, deal with them here.
    (abag:absorb mip0
                 :bags texture-map
                 ;; This might have to generate strangely.. careful here.
                 :cattrs (list
                          (list 'bar "mip attr 0 overlays onto tmap attr 0")
                          (list 'qux "mip attr 1")))
    texture-map))

;; ----------------------------------------------------------------------------

;; The actual test forms looking at the stuff defined above.

;; TODO: Move to generic test suite definition file.
(define-test suite/texture-map)
(define-test suite/texture-map/1d
  :depends-on (suite/texture-map))

;; TODO: Start writing tests to check that the texture exists in the metaspace
;; if appropriate, and that constructed textures have the right in memory
;; structure.

(define-test meta-present/g000-1d-log-inf-one-non
  :depends-on (suite/texture-map/1d)
  (let* ((tmap-desc
           (u:href colony::=meta/texture-maps= 'g000-1d-log-inf-one-non))
         (gen-tmap (when tmap-desc (texmap:constructor tmap-desc))))
    (of-type 'texmap:texture-map-descriptor tmap-desc)
    (of-type 'function gen-tmap)))

(define-test constructed/g000-1d-log-inf-one-non
  :depends-on (suite/texture-map/1d)
  (let* ((tmap-desc
           (u:href colony::=meta/texture-maps= 'g000-1d-log-inf-one-non))
         (gen-tmap (when tmap-desc (texmap:constructor tmap-desc)))
         (tmap (when gen-tmap (funcall gen-tmap))))
    (of-type 'texmap:texture-map-descriptor tmap-desc)
    (of-type 'function gen-tmap)
    ;; TODO check that the constructed tmap in memory data structure and all if
    ;; its children, etc is exactly what we expect it to be.
    (of-type 'texmap:texture-map-1d tmap)))
