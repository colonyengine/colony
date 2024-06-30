(in-package #:colony.test)

;;;; --------------------------------------------------------------------------
;;;; Tests of Logical and Physical Texture-map 3D DSL forms.
;;;; --------------------------------------------------------------------------

;; NOTE: These texture maps requires these images:
;; Single mipmaps: A colored sphere is at each level with direction markers.
;; 3d-64x64x64-{0,63} Red
;; 3d-32x32x32-{0,31} Orange
;; 3d-16x16x16-{0,15} Yellow
;; 3d-8x8x8-{0,7}     Green
;; 3d-4x4x4-{0,3}     Blue
;; 3d-2x2x2-{0,1}     Indigo
;; 3d-1x1x1-0         Violet
;;
;; NOTE: Auto generate a sphere in each one for each color with dir markers.

;; NOTE: g000-3d is laboriously typed out by hand in order to ensure the DSL
;; conforms to the specification it needs. The rest of the 3d examples,
;; however, are generated with the help of some macros to save typing.
;; Even those macros need macros to save more typing---but I need to see
;; several examples to ensure I pick the right abstraction to make it easier
;; to specify all this stuff.

;;; --------------------------------------------------------
;; Group: 000 (One mipmap specified in unique file)
;; Name: 3d
;; Model (:3d (:slices :xy-z))
;;; --------------------------------------------------------

;; logical form, 99% of texture-maps prolly look like this (or with all
;; mipmaps specified).
(c:define-texture-map g000-3d-log-inf-one-non (:3d :unique (:slices :xy-z))
  ;; NOTE: could be (and likely will be) macro generated, but spelled out
  ;; for now to be explicit. These images represent xy slices and each slice
  ;; is increasing in the z dimension. Hence :xy-z.
  (c:mipmap (textures 3d-64x64x64-0)
            (textures 3d-64x64x64-1)
            (textures 3d-64x64x64-2)
            (textures 3d-64x64x64-3)
            (textures 3d-64x64x64-4)
            (textures 3d-64x64x64-5)
            (textures 3d-64x64x64-6)
            (textures 3d-64x64x64-7)
            (textures 3d-64x64x64-8)
            (textures 3d-64x64x64-9)
            (textures 3d-64x64x64-10)
            (textures 3d-64x64x64-11)
            (textures 3d-64x64x64-12)
            (textures 3d-64x64x64-13)
            (textures 3d-64x64x64-14)
            (textures 3d-64x64x64-15)
            (textures 3d-64x64x64-16)
            (textures 3d-64x64x64-17)
            (textures 3d-64x64x64-18)
            (textures 3d-64x64x64-19)
            (textures 3d-64x64x64-20)
            (textures 3d-64x64x64-21)
            (textures 3d-64x64x64-22)
            (textures 3d-64x64x64-23)
            (textures 3d-64x64x64-24)
            (textures 3d-64x64x64-25)
            (textures 3d-64x64x64-26)
            (textures 3d-64x64x64-27)
            (textures 3d-64x64x64-28)
            (textures 3d-64x64x64-29)
            (textures 3d-64x64x64-30)
            (textures 3d-64x64x64-31)
            (textures 3d-64x64x64-32)
            (textures 3d-64x64x64-33)
            (textures 3d-64x64x64-34)
            (textures 3d-64x64x64-35)
            (textures 3d-64x64x64-36)
            (textures 3d-64x64x64-37)
            (textures 3d-64x64x64-38)
            (textures 3d-64x64x64-39)
            (textures 3d-64x64x64-40)
            (textures 3d-64x64x64-41)
            (textures 3d-64x64x64-42)
            (textures 3d-64x64x64-43)
            (textures 3d-64x64x64-44)
            (textures 3d-64x64x64-45)
            (textures 3d-64x64x64-46)
            (textures 3d-64x64x64-47)
            (textures 3d-64x64x64-48)
            (textures 3d-64x64x64-49)
            (textures 3d-64x64x64-50)
            (textures 3d-64x64x64-51)
            (textures 3d-64x64x64-52)
            (textures 3d-64x64x64-53)
            (textures 3d-64x64x64-54)
            (textures 3d-64x64x64-55)
            (textures 3d-64x64x64-56)
            (textures 3d-64x64x64-57)
            (textures 3d-64x64x64-58)
            (textures 3d-64x64x64-59)
            (textures 3d-64x64x64-60)
            (textures 3d-64x64x64-61)
            (textures 3d-64x64x64-62)
            (textures 3d-64x64x64-63)))
;; |
;; | converted upon macro expansion immediately to the below physical form.
;; v
;; physical form of the above logical form with many things yet to be inferred
;; For each element: :physloc will be filled in too, but :data may or may not
;; be fully realized (meaning it could exist but hold header information
;; only).
(c:define-texture-map g000-3d-phy-inf-one-non (:3d :unique (:slices :xy-z))
  (c:data-elements
   ;; The first value can be a symbol or an integer, but in memory are mapped
   ;; to integers starting from 0 and increasing by one each time in the order
   ;; of presentation in c:data-elements. The :elidx entry in the data-span-*
   ;; entries are altered to be the actual index of the data-element in the
   ;; data-elements array and the symbolic names are lost forever (at this
   ;; time) in the actual in memory data structure.
   (0 (c:image-element :logloc (textures 3d-64x64x64-0)))
   (1 (c:image-element :logloc (textures 3d-64x64x64-1)))
   (2 (c:image-element :logloc (textures 3d-64x64x64-2)))
   (3 (c:image-element :logloc (textures 3d-64x64x64-3)))
   (4 (c:image-element :logloc (textures 3d-64x64x64-4)))
   (5 (c:image-element :logloc (textures 3d-64x64x64-5)))
   (6 (c:image-element :logloc (textures 3d-64x64x64-6)))
   (7 (c:image-element :logloc (textures 3d-64x64x64-7)))
   (8 (c:image-element :logloc (textures 3d-64x64x64-8)))
   (9 (c:image-element :logloc (textures 3d-64x64x64-9)))
   (10 (c:image-element :logloc (textures 3d-64x64x64-10)))
   (11 (c:image-element :logloc (textures 3d-64x64x64-11)))
   (12 (c:image-element :logloc (textures 3d-64x64x64-12)))
   (13 (c:image-element :logloc (textures 3d-64x64x64-13)))
   (14 (c:image-element :logloc (textures 3d-64x64x64-14)))
   (15 (c:image-element :logloc (textures 3d-64x64x64-15)))
   (16 (c:image-element :logloc (textures 3d-64x64x64-16)))
   (17 (c:image-element :logloc (textures 3d-64x64x64-17)))
   (18 (c:image-element :logloc (textures 3d-64x64x64-18)))
   (19 (c:image-element :logloc (textures 3d-64x64x64-19)))
   (20 (c:image-element :logloc (textures 3d-64x64x64-20)))
   (21 (c:image-element :logloc (textures 3d-64x64x64-21)))
   (22 (c:image-element :logloc (textures 3d-64x64x64-22)))
   (23 (c:image-element :logloc (textures 3d-64x64x64-23)))
   (24 (c:image-element :logloc (textures 3d-64x64x64-24)))
   (25 (c:image-element :logloc (textures 3d-64x64x64-25)))
   (26 (c:image-element :logloc (textures 3d-64x64x64-26)))
   (27 (c:image-element :logloc (textures 3d-64x64x64-27)))
   (28 (c:image-element :logloc (textures 3d-64x64x64-28)))
   (29 (c:image-element :logloc (textures 3d-64x64x64-29)))
   (30 (c:image-element :logloc (textures 3d-64x64x64-30)))
   (31 (c:image-element :logloc (textures 3d-64x64x64-31)))
   (32 (c:image-element :logloc (textures 3d-64x64x64-32)))
   (33 (c:image-element :logloc (textures 3d-64x64x64-33)))
   (34 (c:image-element :logloc (textures 3d-64x64x64-34)))
   (35 (c:image-element :logloc (textures 3d-64x64x64-35)))
   (36 (c:image-element :logloc (textures 3d-64x64x64-36)))
   (37 (c:image-element :logloc (textures 3d-64x64x64-37)))
   (38 (c:image-element :logloc (textures 3d-64x64x64-38)))
   (39 (c:image-element :logloc (textures 3d-64x64x64-39)))
   (40 (c:image-element :logloc (textures 3d-64x64x64-40)))
   (41 (c:image-element :logloc (textures 3d-64x64x64-41)))
   (42 (c:image-element :logloc (textures 3d-64x64x64-42)))
   (43 (c:image-element :logloc (textures 3d-64x64x64-43)))
   (44 (c:image-element :logloc (textures 3d-64x64x64-44)))
   (45 (c:image-element :logloc (textures 3d-64x64x64-45)))
   (46 (c:image-element :logloc (textures 3d-64x64x64-46)))
   (47 (c:image-element :logloc (textures 3d-64x64x64-47)))
   (48 (c:image-element :logloc (textures 3d-64x64x64-48)))
   (49 (c:image-element :logloc (textures 3d-64x64x64-49)))
   (50 (c:image-element :logloc (textures 3d-64x64x64-50)))
   (51 (c:image-element :logloc (textures 3d-64x64x64-51)))
   (52 (c:image-element :logloc (textures 3d-64x64x64-52)))
   (53 (c:image-element :logloc (textures 3d-64x64x64-53)))
   (54 (c:image-element :logloc (textures 3d-64x64x64-54)))
   (55 (c:image-element :logloc (textures 3d-64x64x64-55)))
   (56 (c:image-element :logloc (textures 3d-64x64x64-56)))
   (57 (c:image-element :logloc (textures 3d-64x64x64-57)))
   (58 (c:image-element :logloc (textures 3d-64x64x64-58)))
   (59 (c:image-element :logloc (textures 3d-64x64x64-59)))
   (60 (c:image-element :logloc (textures 3d-64x64x64-60)))
   (61 (c:image-element :logloc (textures 3d-64x64x64-61)))
   (62 (c:image-element :logloc (textures 3d-64x64x64-62)))
   (63 (c:image-element :logloc (textures 3d-64x64x64-63))))

  (c:mipmap-3d
   :extent (c:span-3d)
   (c:mapping-span-3d :to (c:data-span-3d)
                      :from (c:data-span-3d :elidx 0))
   (c:mapping-span-3d :to (c:data-span-3d)
                      :from (c:data-span-3d :elidx 1))
   (c:mapping-span-3d :to (c:data-span-3d)
                      :from (c:data-span-3d :elidx 2))
   (c:mapping-span-3d :to (c:data-span-3d)
                      :from (c:data-span-3d :elidx 3))
   (c:mapping-span-3d :to (c:data-span-3d)
                      :from (c:data-span-3d :elidx 4))
   (c:mapping-span-3d :to (c:data-span-3d)
                      :from (c:data-span-3d :elidx 5))
   (c:mapping-span-3d :to (c:data-span-3d)
                      :from (c:data-span-3d :elidx 6))
   (c:mapping-span-3d :to (c:data-span-3d)
                      :from (c:data-span-3d :elidx 7))
   (c:mapping-span-3d :to (c:data-span-3d)
                      :from (c:data-span-3d :elidx 8))
   (c:mapping-span-3d :to (c:data-span-3d)
                      :from (c:data-span-3d :elidx 9))
   (c:mapping-span-3d :to (c:data-span-3d)
                      :from (c:data-span-3d :elidx 10))
   (c:mapping-span-3d :to (c:data-span-3d)
                      :from (c:data-span-3d :elidx 11))
   (c:mapping-span-3d :to (c:data-span-3d)
                      :from (c:data-span-3d :elidx 12))
   (c:mapping-span-3d :to (c:data-span-3d)
                      :from (c:data-span-3d :elidx 13))
   (c:mapping-span-3d :to (c:data-span-3d)
                      :from (c:data-span-3d :elidx 14))
   (c:mapping-span-3d :to (c:data-span-3d)
                      :from (c:data-span-3d :elidx 15))
   (c:mapping-span-3d :to (c:data-span-3d)
                      :from (c:data-span-3d :elidx 16))
   (c:mapping-span-3d :to (c:data-span-3d)
                      :from (c:data-span-3d :elidx 17))
   (c:mapping-span-3d :to (c:data-span-3d)
                      :from (c:data-span-3d :elidx 18))
   (c:mapping-span-3d :to (c:data-span-3d)
                      :from (c:data-span-3d :elidx 19))
   (c:mapping-span-3d :to (c:data-span-3d)
                      :from (c:data-span-3d :elidx 20))
   (c:mapping-span-3d :to (c:data-span-3d)
                      :from (c:data-span-3d :elidx 21))
   (c:mapping-span-3d :to (c:data-span-3d)
                      :from (c:data-span-3d :elidx 22))
   (c:mapping-span-3d :to (c:data-span-3d)
                      :from (c:data-span-3d :elidx 23))
   (c:mapping-span-3d :to (c:data-span-3d)
                      :from (c:data-span-3d :elidx 24))
   (c:mapping-span-3d :to (c:data-span-3d)
                      :from (c:data-span-3d :elidx 25))
   (c:mapping-span-3d :to (c:data-span-3d)
                      :from (c:data-span-3d :elidx 26))
   (c:mapping-span-3d :to (c:data-span-3d)
                      :from (c:data-span-3d :elidx 27))
   (c:mapping-span-3d :to (c:data-span-3d)
                      :from (c:data-span-3d :elidx 28))
   (c:mapping-span-3d :to (c:data-span-3d)
                      :from (c:data-span-3d :elidx 29))
   (c:mapping-span-3d :to (c:data-span-3d)
                      :from (c:data-span-3d :elidx 30))
   (c:mapping-span-3d :to (c:data-span-3d)
                      :from (c:data-span-3d :elidx 31))
   (c:mapping-span-3d :to (c:data-span-3d)
                      :from (c:data-span-3d :elidx 32))
   (c:mapping-span-3d :to (c:data-span-3d)
                      :from (c:data-span-3d :elidx 33))
   (c:mapping-span-3d :to (c:data-span-3d)
                      :from (c:data-span-3d :elidx 34))
   (c:mapping-span-3d :to (c:data-span-3d)
                      :from (c:data-span-3d :elidx 35))
   (c:mapping-span-3d :to (c:data-span-3d)
                      :from (c:data-span-3d :elidx 36))
   (c:mapping-span-3d :to (c:data-span-3d)
                      :from (c:data-span-3d :elidx 37))
   (c:mapping-span-3d :to (c:data-span-3d)
                      :from (c:data-span-3d :elidx 38))
   (c:mapping-span-3d :to (c:data-span-3d)
                      :from (c:data-span-3d :elidx 39))
   (c:mapping-span-3d :to (c:data-span-3d)
                      :from (c:data-span-3d :elidx 40))
   (c:mapping-span-3d :to (c:data-span-3d)
                      :from (c:data-span-3d :elidx 41))
   (c:mapping-span-3d :to (c:data-span-3d)
                      :from (c:data-span-3d :elidx 42))
   (c:mapping-span-3d :to (c:data-span-3d)
                      :from (c:data-span-3d :elidx 43))
   (c:mapping-span-3d :to (c:data-span-3d)
                      :from (c:data-span-3d :elidx 44))
   (c:mapping-span-3d :to (c:data-span-3d)
                      :from (c:data-span-3d :elidx 45))
   (c:mapping-span-3d :to (c:data-span-3d)
                      :from (c:data-span-3d :elidx 46))
   (c:mapping-span-3d :to (c:data-span-3d)
                      :from (c:data-span-3d :elidx 47))
   (c:mapping-span-3d :to (c:data-span-3d)
                      :from (c:data-span-3d :elidx 48))
   (c:mapping-span-3d :to (c:data-span-3d)
                      :from (c:data-span-3d :elidx 49))
   (c:mapping-span-3d :to (c:data-span-3d)
                      :from (c:data-span-3d :elidx 50))
   (c:mapping-span-3d :to (c:data-span-3d)
                      :from (c:data-span-3d :elidx 51))
   (c:mapping-span-3d :to (c:data-span-3d)
                      :from (c:data-span-3d :elidx 52))
   (c:mapping-span-3d :to (c:data-span-3d)
                      :from (c:data-span-3d :elidx 53))
   (c:mapping-span-3d :to (c:data-span-3d)
                      :from (c:data-span-3d :elidx 54))
   (c:mapping-span-3d :to (c:data-span-3d)
                      :from (c:data-span-3d :elidx 55))
   (c:mapping-span-3d :to (c:data-span-3d)
                      :from (c:data-span-3d :elidx 56))
   (c:mapping-span-3d :to (c:data-span-3d)
                      :from (c:data-span-3d :elidx 57))
   (c:mapping-span-3d :to (c:data-span-3d)
                      :from (c:data-span-3d :elidx 58))
   (c:mapping-span-3d :to (c:data-span-3d)
                      :from (c:data-span-3d :elidx 59))
   (c:mapping-span-3d :to (c:data-span-3d)
                      :from (c:data-span-3d :elidx 60))
   (c:mapping-span-3d :to (c:data-span-3d)
                      :from (c:data-span-3d :elidx 61))
   (c:mapping-span-3d :to (c:data-span-3d)
                      :from (c:data-span-3d :elidx 62))
   (c:mapping-span-3d :to (c:data-span-3d)
                      :from (c:data-span-3d :elidx 63))))
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
(c:define-texture-map g000-3d-phy-gnd-one-non (:3d :unique (:slices :xy-z))
  (c:data-elements
   (0 (c:image-element :logloc (textures 3d-64x64x64-0)))
   (1 (c:image-element :logloc (textures 3d-64x64x64-1)))
   (2 (c:image-element :logloc (textures 3d-64x64x64-2)))
   (3 (c:image-element :logloc (textures 3d-64x64x64-3)))
   (4 (c:image-element :logloc (textures 3d-64x64x64-4)))
   (5 (c:image-element :logloc (textures 3d-64x64x64-5)))
   (6 (c:image-element :logloc (textures 3d-64x64x64-6)))
   (7 (c:image-element :logloc (textures 3d-64x64x64-7)))
   (8 (c:image-element :logloc (textures 3d-64x64x64-8)))
   (9 (c:image-element :logloc (textures 3d-64x64x64-9)))
   (10 (c:image-element :logloc (textures 3d-64x64x64-10)))
   (11 (c:image-element :logloc (textures 3d-64x64x64-11)))
   (12 (c:image-element :logloc (textures 3d-64x64x64-12)))
   (13 (c:image-element :logloc (textures 3d-64x64x64-13)))
   (14 (c:image-element :logloc (textures 3d-64x64x64-14)))
   (15 (c:image-element :logloc (textures 3d-64x64x64-15)))
   (16 (c:image-element :logloc (textures 3d-64x64x64-16)))
   (17 (c:image-element :logloc (textures 3d-64x64x64-17)))
   (18 (c:image-element :logloc (textures 3d-64x64x64-18)))
   (19 (c:image-element :logloc (textures 3d-64x64x64-19)))
   (20 (c:image-element :logloc (textures 3d-64x64x64-20)))
   (21 (c:image-element :logloc (textures 3d-64x64x64-21)))
   (22 (c:image-element :logloc (textures 3d-64x64x64-22)))
   (23 (c:image-element :logloc (textures 3d-64x64x64-23)))
   (24 (c:image-element :logloc (textures 3d-64x64x64-24)))
   (25 (c:image-element :logloc (textures 3d-64x64x64-25)))
   (26 (c:image-element :logloc (textures 3d-64x64x64-26)))
   (27 (c:image-element :logloc (textures 3d-64x64x64-27)))
   (28 (c:image-element :logloc (textures 3d-64x64x64-28)))
   (29 (c:image-element :logloc (textures 3d-64x64x64-29)))
   (30 (c:image-element :logloc (textures 3d-64x64x64-30)))
   (31 (c:image-element :logloc (textures 3d-64x64x64-31)))
   (32 (c:image-element :logloc (textures 3d-64x64x64-32)))
   (33 (c:image-element :logloc (textures 3d-64x64x64-33)))
   (34 (c:image-element :logloc (textures 3d-64x64x64-34)))
   (35 (c:image-element :logloc (textures 3d-64x64x64-35)))
   (36 (c:image-element :logloc (textures 3d-64x64x64-36)))
   (37 (c:image-element :logloc (textures 3d-64x64x64-37)))
   (38 (c:image-element :logloc (textures 3d-64x64x64-38)))
   (39 (c:image-element :logloc (textures 3d-64x64x64-39)))
   (40 (c:image-element :logloc (textures 3d-64x64x64-40)))
   (41 (c:image-element :logloc (textures 3d-64x64x64-41)))
   (42 (c:image-element :logloc (textures 3d-64x64x64-42)))
   (43 (c:image-element :logloc (textures 3d-64x64x64-43)))
   (44 (c:image-element :logloc (textures 3d-64x64x64-44)))
   (45 (c:image-element :logloc (textures 3d-64x64x64-45)))
   (46 (c:image-element :logloc (textures 3d-64x64x64-46)))
   (47 (c:image-element :logloc (textures 3d-64x64x64-47)))
   (48 (c:image-element :logloc (textures 3d-64x64x64-48)))
   (49 (c:image-element :logloc (textures 3d-64x64x64-49)))
   (50 (c:image-element :logloc (textures 3d-64x64x64-50)))
   (51 (c:image-element :logloc (textures 3d-64x64x64-51)))
   (52 (c:image-element :logloc (textures 3d-64x64x64-52)))
   (53 (c:image-element :logloc (textures 3d-64x64x64-53)))
   (54 (c:image-element :logloc (textures 3d-64x64x64-54)))
   (55 (c:image-element :logloc (textures 3d-64x64x64-55)))
   (56 (c:image-element :logloc (textures 3d-64x64x64-56)))
   (57 (c:image-element :logloc (textures 3d-64x64x64-57)))
   (58 (c:image-element :logloc (textures 3d-64x64x64-58)))
   (59 (c:image-element :logloc (textures 3d-64x64x64-59)))
   (60 (c:image-element :logloc (textures 3d-64x64x64-60)))
   (61 (c:image-element :logloc (textures 3d-64x64x64-61)))
   (62 (c:image-element :logloc (textures 3d-64x64x64-62)))
   (63 (c:image-element :logloc (textures 3d-64x64x64-63))))

  (c:mipmap-3d
   :extent (c:span-3d :origin (v3:vec 0f0 0f0 0f0 )
                      :extent (v3:vec 64f0 64f0 64f0))
   (c:mapping-span-3d :to (c:data-span-3d :origin (v3:vec 0f0 0f0 0f0)
                                          :extent (v3:vec 64f0 64f0 1f0))
                      :from (c:data-span-3d :origin (v3:vec 0f0 0f0 0f0)
                                            :extent (v3:vec 64f0 64f0 1f0)
                                            :elidx 0))
   (c:mapping-span-3d :to (c:data-span-3d :origin (v3:vec 0f0 0f0 1f0)
                                          :extent (v3:vec 64f0 64f0 1f0))
                      :from (c:data-span-3d :origin (v3:vec 0f0 0f0 0f0)
                                            :extent (v3:vec 64f0 64f0 1f0)
                                            :elidx 1))
   (c:mapping-span-3d :to (c:data-span-3d :origin (v3:vec 0f0 0f0 2f0)
                                          :extent (v3:vec 64f0 64f0 1f0))
                      :from (c:data-span-3d :origin (v3:vec 0f0 0f0 0f0)
                                            :extent (v3:vec 64f0 64f0 1f0)
                                            :elidx 2))
   (c:mapping-span-3d :to (c:data-span-3d :origin (v3:vec 0f0 0f0 3f0)
                                          :extent (v3:vec 64f0 64f0 1f0))
                      :from (c:data-span-3d :origin (v3:vec 0f0 0f0 0f0)
                                            :extent (v3:vec 64f0 64f0 1f0)
                                            :elidx 3))
   (c:mapping-span-3d :to (c:data-span-3d :origin (v3:vec 0f0 0f0 4f0)
                                          :extent (v3:vec 64f0 64f0 1f0))
                      :from (c:data-span-3d :origin (v3:vec 0f0 0f0 0f0)
                                            :extent (v3:vec 64f0 64f0 1f0)
                                            :elidx 4))
   (c:mapping-span-3d :to (c:data-span-3d :origin (v3:vec 0f0 0f0 5f0)
                                          :extent (v3:vec 64f0 64f0 1f0))
                      :from (c:data-span-3d :origin (v3:vec 0f0 0f0 0f0)
                                            :extent (v3:vec 64f0 64f0 1f0)
                                            :elidx 5))
   (c:mapping-span-3d :to (c:data-span-3d :origin (v3:vec 0f0 0f0 6f0)
                                          :extent (v3:vec 64f0 64f0 1f0))
                      :from (c:data-span-3d :origin (v3:vec 0f0 0f0 0f0)
                                            :extent (v3:vec 64f0 64f0 1f0)
                                            :elidx 6))
   (c:mapping-span-3d :to (c:data-span-3d :origin (v3:vec 0f0 0f0 7f0)
                                          :extent (v3:vec 64f0 64f0 1f0))
                      :from (c:data-span-3d :origin (v3:vec 0f0 0f0 0f0)
                                            :extent (v3:vec 64f0 64f0 1f0)
                                            :elidx 7))
   (c:mapping-span-3d :to (c:data-span-3d :origin (v3:vec 0f0 0f0 8f0)
                                          :extent (v3:vec 64f0 64f0 1f0))
                      :from (c:data-span-3d :origin (v3:vec 0f0 0f0 0f0)
                                            :extent (v3:vec 64f0 64f0 1f0)
                                            :elidx 8))
   (c:mapping-span-3d :to (c:data-span-3d :origin (v3:vec 0f0 0f0 9f0)
                                          :extent (v3:vec 64f0 64f0 1f0))
                      :from (c:data-span-3d :origin (v3:vec 0f0 0f0 0f0)
                                            :extent (v3:vec 64f0 64f0 1f0)
                                            :elidx 9))
   (c:mapping-span-3d :to (c:data-span-3d :origin (v3:vec 0f0 0f0 10f0)
                                          :extent (v3:vec 64f0 64f0 1f0))
                      :from (c:data-span-3d :origin (v3:vec 0f0 0f0 0f0)
                                            :extent (v3:vec 64f0 64f0 1f0)
                                            :elidx 10))
   (c:mapping-span-3d :to (c:data-span-3d :origin (v3:vec 0f0 0f0 11f0)
                                          :extent (v3:vec 64f0 64f0 1f0))
                      :from (c:data-span-3d :origin (v3:vec 0f0 0f0 0f0)
                                            :extent (v3:vec 64f0 64f0 1f0)
                                            :elidx 11))
   (c:mapping-span-3d :to (c:data-span-3d :origin (v3:vec 0f0 0f0 12f0)
                                          :extent (v3:vec 64f0 64f0 1f0))
                      :from (c:data-span-3d :origin (v3:vec 0f0 0f0 0f0)
                                            :extent (v3:vec 64f0 64f0 1f0)
                                            :elidx 12))
   (c:mapping-span-3d :to (c:data-span-3d :origin (v3:vec 0f0 0f0 13f0)
                                          :extent (v3:vec 64f0 64f0 1f0))
                      :from (c:data-span-3d :origin (v3:vec 0f0 0f0 0f0)
                                            :extent (v3:vec 64f0 64f0 1f0)
                                            :elidx 13))
   (c:mapping-span-3d :to (c:data-span-3d :origin (v3:vec 0f0 0f0 14f0)
                                          :extent (v3:vec 64f0 64f0 1f0))
                      :from (c:data-span-3d :origin (v3:vec 0f0 0f0 0f0)
                                            :extent (v3:vec 64f0 64f0 1f0)
                                            :elidx 14))
   (c:mapping-span-3d :to (c:data-span-3d :origin (v3:vec 0f0 0f0 15f0)
                                          :extent (v3:vec 64f0 64f0 1f0))
                      :from (c:data-span-3d :origin (v3:vec 0f0 0f0 0f0)
                                            :extent (v3:vec 64f0 64f0 1f0)
                                            :elidx 15))
   (c:mapping-span-3d :to (c:data-span-3d :origin (v3:vec 0f0 0f0 16f0)
                                          :extent (v3:vec 64f0 64f0 1f0))
                      :from (c:data-span-3d :origin (v3:vec 0f0 0f0 0f0)
                                            :extent (v3:vec 64f0 64f0 1f0)
                                            :elidx 16))
   (c:mapping-span-3d :to (c:data-span-3d :origin (v3:vec 0f0 0f0 17f0)
                                          :extent (v3:vec 64f0 64f0 1f0))
                      :from (c:data-span-3d :origin (v3:vec 0f0 0f0 0f0)
                                            :extent (v3:vec 64f0 64f0 1f0)
                                            :elidx 17))
   (c:mapping-span-3d :to (c:data-span-3d :origin (v3:vec 0f0 0f0 18f0)
                                          :extent (v3:vec 64f0 64f0 1f0))
                      :from (c:data-span-3d :origin (v3:vec 0f0 0f0 0f0)
                                            :extent (v3:vec 64f0 64f0 1f0)
                                            :elidx 18))
   (c:mapping-span-3d :to (c:data-span-3d :origin (v3:vec 0f0 0f0 19f0)
                                          :extent (v3:vec 64f0 64f0 1f0))
                      :from (c:data-span-3d :origin (v3:vec 0f0 0f0 0f0)
                                            :extent (v3:vec 64f0 64f0 1f0)
                                            :elidx 19))
   (c:mapping-span-3d :to (c:data-span-3d :origin (v3:vec 0f0 0f0 20f0)
                                          :extent (v3:vec 64f0 64f0 1f0))
                      :from (c:data-span-3d :origin (v3:vec 0f0 0f0 0f0)
                                            :extent (v3:vec 64f0 64f0 1f0)
                                            :elidx 20))
   (c:mapping-span-3d :to (c:data-span-3d :origin (v3:vec 0f0 0f0 21f0)
                                          :extent (v3:vec 64f0 64f0 1f0))
                      :from (c:data-span-3d :origin (v3:vec 0f0 0f0 0f0)
                                            :extent (v3:vec 64f0 64f0 1f0)
                                            :elidx 21))
   (c:mapping-span-3d :to (c:data-span-3d :origin (v3:vec 0f0 0f0 22f0)
                                          :extent (v3:vec 64f0 64f0 1f0))
                      :from (c:data-span-3d :origin (v3:vec 0f0 0f0 0f0)
                                            :extent (v3:vec 64f0 64f0 1f0)
                                            :elidx 22))
   (c:mapping-span-3d :to (c:data-span-3d :origin (v3:vec 0f0 0f0 23f0)
                                          :extent (v3:vec 64f0 64f0 1f0))
                      :from (c:data-span-3d :origin (v3:vec 0f0 0f0 0f0)
                                            :extent (v3:vec 64f0 64f0 1f0)
                                            :elidx 23))
   (c:mapping-span-3d :to (c:data-span-3d :origin (v3:vec 0f0 0f0 24f0)
                                          :extent (v3:vec 64f0 64f0 1f0))
                      :from (c:data-span-3d :origin (v3:vec 0f0 0f0 0f0)
                                            :extent (v3:vec 64f0 64f0 1f0)
                                            :elidx 24))
   (c:mapping-span-3d :to (c:data-span-3d :origin (v3:vec 0f0 0f0 25f0)
                                          :extent (v3:vec 64f0 64f0 1f0))
                      :from (c:data-span-3d :origin (v3:vec 0f0 0f0 0f0)
                                            :extent (v3:vec 64f0 64f0 1f0)
                                            :elidx 25))
   (c:mapping-span-3d :to (c:data-span-3d :origin (v3:vec 0f0 0f0 26f0)
                                          :extent (v3:vec 64f0 64f0 1f0))
                      :from (c:data-span-3d :origin (v3:vec 0f0 0f0 0f0)
                                            :extent (v3:vec 64f0 64f0 1f0)
                                            :elidx 26))
   (c:mapping-span-3d :to (c:data-span-3d :origin (v3:vec 0f0 0f0 27f0)
                                          :extent (v3:vec 64f0 64f0 1f0))
                      :from (c:data-span-3d :origin (v3:vec 0f0 0f0 0f0)
                                            :extent (v3:vec 64f0 64f0 1f0)
                                            :elidx 27))
   (c:mapping-span-3d :to (c:data-span-3d :origin (v3:vec 0f0 0f0 28f0)
                                          :extent (v3:vec 64f0 64f0 1f0))
                      :from (c:data-span-3d :origin (v3:vec 0f0 0f0 0f0)
                                            :extent (v3:vec 64f0 64f0 1f0)
                                            :elidx 28))
   (c:mapping-span-3d :to (c:data-span-3d :origin (v3:vec 0f0 0f0 29f0)
                                          :extent (v3:vec 64f0 64f0 1f0))
                      :from (c:data-span-3d :origin (v3:vec 0f0 0f0 0f0)
                                            :extent (v3:vec 64f0 64f0 1f0)
                                            :elidx 29))
   (c:mapping-span-3d :to (c:data-span-3d :origin (v3:vec 0f0 0f0 30f0)
                                          :extent (v3:vec 64f0 64f0 1f0))
                      :from (c:data-span-3d :origin (v3:vec 0f0 0f0 0f0)
                                            :extent (v3:vec 64f0 64f0 1f0)
                                            :elidx 30))
   (c:mapping-span-3d :to (c:data-span-3d :origin (v3:vec 0f0 0f0 31f0)
                                          :extent (v3:vec 64f0 64f0 1f0))
                      :from (c:data-span-3d :origin (v3:vec 0f0 0f0 0f0)
                                            :extent (v3:vec 64f0 64f0 1f0)
                                            :elidx 31))
   (c:mapping-span-3d :to (c:data-span-3d :origin (v3:vec 0f0 0f0 32f0)
                                          :extent (v3:vec 64f0 64f0 1f0))
                      :from (c:data-span-3d :origin (v3:vec 0f0 0f0 0f0)
                                            :extent (v3:vec 64f0 64f0 1f0)
                                            :elidx 32))
   (c:mapping-span-3d :to (c:data-span-3d :origin (v3:vec 0f0 0f0 33f0)
                                          :extent (v3:vec 64f0 64f0 1f0))
                      :from (c:data-span-3d :origin (v3:vec 0f0 0f0 0f0)
                                            :extent (v3:vec 64f0 64f0 1f0)
                                            :elidx 33))
   (c:mapping-span-3d :to (c:data-span-3d :origin (v3:vec 0f0 0f0 34f0)
                                          :extent (v3:vec 64f0 64f0 1f0))
                      :from (c:data-span-3d :origin (v3:vec 0f0 0f0 0f0)
                                            :extent (v3:vec 64f0 64f0 1f0)
                                            :elidx 34))
   (c:mapping-span-3d :to (c:data-span-3d :origin (v3:vec 0f0 0f0 35f0)
                                          :extent (v3:vec 64f0 64f0 1f0))
                      :from (c:data-span-3d :origin (v3:vec 0f0 0f0 0f0)
                                            :extent (v3:vec 64f0 64f0 1f0)
                                            :elidx 35))
   (c:mapping-span-3d :to (c:data-span-3d :origin (v3:vec 0f0 0f0 36f0)
                                          :extent (v3:vec 64f0 64f0 1f0))
                      :from (c:data-span-3d :origin (v3:vec 0f0 0f0 0f0)
                                            :extent (v3:vec 64f0 64f0 1f0)
                                            :elidx 36))
   (c:mapping-span-3d :to (c:data-span-3d :origin (v3:vec 0f0 0f0 37f0)
                                          :extent (v3:vec 64f0 64f0 1f0))
                      :from (c:data-span-3d :origin (v3:vec 0f0 0f0 0f0)
                                            :extent (v3:vec 64f0 64f0 1f0)
                                            :elidx 37))
   (c:mapping-span-3d :to (c:data-span-3d :origin (v3:vec 0f0 0f0 38f0)
                                          :extent (v3:vec 64f0 64f0 1f0))
                      :from (c:data-span-3d :origin (v3:vec 0f0 0f0 0f0)
                                            :extent (v3:vec 64f0 64f0 1f0)
                                            :elidx 38))
   (c:mapping-span-3d :to (c:data-span-3d :origin (v3:vec 0f0 0f0 39f0)
                                          :extent (v3:vec 64f0 64f0 1f0))
                      :from (c:data-span-3d :origin (v3:vec 0f0 0f0 0f0)
                                            :extent (v3:vec 64f0 64f0 1f0)
                                            :elidx 39))
   (c:mapping-span-3d :to (c:data-span-3d :origin (v3:vec 0f0 0f0 40f0)
                                          :extent (v3:vec 64f0 64f0 1f0))
                      :from (c:data-span-3d :origin (v3:vec 0f0 0f0 0f0)
                                            :extent (v3:vec 64f0 64f0 1f0)
                                            :elidx 40))
   (c:mapping-span-3d :to (c:data-span-3d :origin (v3:vec 0f0 0f0 41f0)
                                          :extent (v3:vec 64f0 64f0 1f0))
                      :from (c:data-span-3d :origin (v3:vec 0f0 0f0 0f0)
                                            :extent (v3:vec 64f0 64f0 1f0)
                                            :elidx 41))
   (c:mapping-span-3d :to (c:data-span-3d :origin (v3:vec 0f0 0f0 42f0)
                                          :extent (v3:vec 64f0 64f0 1f0))
                      :from (c:data-span-3d :origin (v3:vec 0f0 0f0 0f0)
                                            :extent (v3:vec 64f0 64f0 1f0)
                                            :elidx 42))
   (c:mapping-span-3d :to (c:data-span-3d :origin (v3:vec 0f0 0f0 43f0)
                                          :extent (v3:vec 64f0 64f0 1f0))
                      :from (c:data-span-3d :origin (v3:vec 0f0 0f0 0f0)
                                            :extent (v3:vec 64f0 64f0 1f0)
                                            :elidx 43))
   (c:mapping-span-3d :to (c:data-span-3d :origin (v3:vec 0f0 0f0 44f0)
                                          :extent (v3:vec 64f0 64f0 1f0))
                      :from (c:data-span-3d :origin (v3:vec 0f0 0f0 0f0)
                                            :extent (v3:vec 64f0 64f0 1f0)
                                            :elidx 44))
   (c:mapping-span-3d :to (c:data-span-3d :origin (v3:vec 0f0 0f0 45f0)
                                          :extent (v3:vec 64f0 64f0 1f0))
                      :from (c:data-span-3d :origin (v3:vec 0f0 0f0 0f0)
                                            :extent (v3:vec 64f0 64f0 1f0)
                                            :elidx 45))
   (c:mapping-span-3d :to (c:data-span-3d :origin (v3:vec 0f0 0f0 46f0)
                                          :extent (v3:vec 64f0 64f0 1f0))
                      :from (c:data-span-3d :origin (v3:vec 0f0 0f0 0f0)
                                            :extent (v3:vec 64f0 64f0 1f0)
                                            :elidx 46))
   (c:mapping-span-3d :to (c:data-span-3d :origin (v3:vec 0f0 0f0 47f0)
                                          :extent (v3:vec 64f0 64f0 1f0))
                      :from (c:data-span-3d :origin (v3:vec 0f0 0f0 0f0)
                                            :extent (v3:vec 64f0 64f0 1f0)
                                            :elidx 47))
   (c:mapping-span-3d :to (c:data-span-3d :origin (v3:vec 0f0 0f0 48f0)
                                          :extent (v3:vec 64f0 64f0 1f0))
                      :from (c:data-span-3d :origin (v3:vec 0f0 0f0 0f0)
                                            :extent (v3:vec 64f0 64f0 1f0)
                                            :elidx 48))
   (c:mapping-span-3d :to (c:data-span-3d :origin (v3:vec 0f0 0f0 49f0)
                                          :extent (v3:vec 64f0 64f0 1f0))
                      :from (c:data-span-3d :origin (v3:vec 0f0 0f0 0f0)
                                            :extent (v3:vec 64f0 64f0 1f0)
                                            :elidx 49))
   (c:mapping-span-3d :to (c:data-span-3d :origin (v3:vec 0f0 0f0 50f0)
                                          :extent (v3:vec 64f0 64f0 1f0))
                      :from (c:data-span-3d :origin (v3:vec 0f0 0f0 0f0)
                                            :extent (v3:vec 64f0 64f0 1f0)
                                            :elidx 50))
   (c:mapping-span-3d :to (c:data-span-3d :origin (v3:vec 0f0 0f0 51f0)
                                          :extent (v3:vec 64f0 64f0 1f0))
                      :from (c:data-span-3d :origin (v3:vec 0f0 0f0 0f0)
                                            :extent (v3:vec 64f0 64f0 1f0)
                                            :elidx 51))
   (c:mapping-span-3d :to (c:data-span-3d :origin (v3:vec 0f0 0f0 52f0)
                                          :extent (v3:vec 64f0 64f0 1f0))
                      :from (c:data-span-3d :origin (v3:vec 0f0 0f0 0f0)
                                            :extent (v3:vec 64f0 64f0 1f0)
                                            :elidx 52))
   (c:mapping-span-3d :to (c:data-span-3d :origin (v3:vec 0f0 0f0 53f0)
                                          :extent (v3:vec 64f0 64f0 1f0))
                      :from (c:data-span-3d :origin (v3:vec 0f0 0f0 0f0)
                                            :extent (v3:vec 64f0 64f0 1f0)
                                            :elidx 53))
   (c:mapping-span-3d :to (c:data-span-3d :origin (v3:vec 0f0 0f0 54f0)
                                          :extent (v3:vec 64f0 64f0 1f0))
                      :from (c:data-span-3d :origin (v3:vec 0f0 0f0 0f0)
                                            :extent (v3:vec 64f0 64f0 1f0)
                                            :elidx 54))
   (c:mapping-span-3d :to (c:data-span-3d :origin (v3:vec 0f0 0f0 55f0)
                                          :extent (v3:vec 64f0 64f0 1f0))
                      :from (c:data-span-3d :origin (v3:vec 0f0 0f0 0f0)
                                            :extent (v3:vec 64f0 64f0 1f0)
                                            :elidx 55))
   (c:mapping-span-3d :to (c:data-span-3d :origin (v3:vec 0f0 0f0 56f0)
                                          :extent (v3:vec 64f0 64f0 1f0))
                      :from (c:data-span-3d :origin (v3:vec 0f0 0f0 0f0)
                                            :extent (v3:vec 64f0 64f0 1f0)
                                            :elidx 56))
   (c:mapping-span-3d :to (c:data-span-3d :origin (v3:vec 0f0 0f0 57f0)
                                          :extent (v3:vec 64f0 64f0 1f0))
                      :from (c:data-span-3d :origin (v3:vec 0f0 0f0 0f0)
                                            :extent (v3:vec 64f0 64f0 1f0)
                                            :elidx 57))
   (c:mapping-span-3d :to (c:data-span-3d :origin (v3:vec 0f0 0f0 58f0)
                                          :extent (v3:vec 64f0 64f0 1f0))
                      :from (c:data-span-3d :origin (v3:vec 0f0 0f0 0f0)
                                            :extent (v3:vec 64f0 64f0 1f0)
                                            :elidx 58))
   (c:mapping-span-3d :to (c:data-span-3d :origin (v3:vec 0f0 0f0 59f0)
                                          :extent (v3:vec 64f0 64f0 1f0))
                      :from (c:data-span-3d :origin (v3:vec 0f0 0f0 0f0)
                                            :extent (v3:vec 64f0 64f0 1f0)
                                            :elidx 59))
   (c:mapping-span-3d :to (c:data-span-3d :origin (v3:vec 0f0 0f0 60f0)
                                          :extent (v3:vec 64f0 64f0 1f0))
                      :from (c:data-span-3d :origin (v3:vec 0f0 0f0 0f0)
                                            :extent (v3:vec 64f0 64f0 1f0)
                                            :elidx 60))
   (c:mapping-span-3d :to (c:data-span-3d :origin (v3:vec 0f0 0f0 61f0)
                                          :extent (v3:vec 64f0 64f0 1f0))
                      :from (c:data-span-3d :origin (v3:vec 0f0 0f0 0f0)
                                            :extent (v3:vec 64f0 64f0 1f0)
                                            :elidx 61))
   (c:mapping-span-3d :to (c:data-span-3d :origin (v3:vec 0f0 0f0 62f0)
                                          :extent (v3:vec 64f0 64f0 1f0))
                      :from (c:data-span-3d :origin (v3:vec 0f0 0f0 0f0)
                                            :extent (v3:vec 64f0 64f0 1f0)
                                            :elidx 62))
   (c:mapping-span-3d :to (c:data-span-3d :origin (v3:vec 0f0 0f0 63f0)
                                          :extent (v3:vec 64f0 64f0 1f0))
                      :from (c:data-span-3d :origin (v3:vec 0f0 0f0 0f0)
                                            :extent (v3:vec 64f0 64f0 1f0)
                                            :elidx 63))))

;;; --------------------------------------------------------
;; Group: 001 (All mipmaps specified with unique files)
;; Name: 3d
;; Model (:3d (:slices :xy-z))
;;; --------------------------------------------------------

(c:define-texture-map g001-3d-log-inf-all-non (:3d :unique (:slices :xy-z))
  ;; Macro generated mipmap forms for all mipmaps. Could be condensed more,
  ;; but this leaves it more reaable. This is a plausibly human writable
  ;; form and it could be slightly augmented with an abstraction over the
  ;; collector of the asset-form generator.
  `(c:mipmap
    ,@(loop :for i :below 64
            :collect `(textures ,(u:format-symbol t "~A-~D" '3d-64x64x64 i))))

  `(c:mipmap
    ,@(loop :for i :below 32
            :collect `(textures ,(u:format-symbol t "~A-~D" '3d-32x32x32 i))))

  `(c:mipmap
    ,@(loop :for i :below 16
            :collect `(textures ,(u:format-symbol t "~A-~D" '3d-16x16x16 i))))

  `(c:mipmap
    ,@(loop :for i :below 8
            :collect `(textures ,(u:format-symbol t "~A-~D" '3d-8x8x8 i))))

  `(c:mipmap
    ,@(loop :for i :below 4
            :collect `(textures ,(u:format-symbol t "~A-~D" '3d-4x4x4 i))))

  `(c:mipmap
    ,@(loop :for i :below 2
            :collect `(textures ,(u:format-symbol t "~A-~D" '3d-2x2x2 i))))

  `(c:mipmap
    ,@(loop :for i :below 1
            :collect `(textures ,(u:format-symbol t "~A-~D" '3d-1x1x1 i)))))
;; |
;; | Physical form
;; |
;; v
(c:define-texture-map g001-3d-phy-inf-all-non (:3d :unique (:slices :xy-z))
  ;; TODO: make this kind of abstraction better.
  `(c:data-elements
    ,@(loop :for elidx :by 1
            :for form :in `(,@(loop :for i :below 64
                                    :collect `(textures
                                               ,(u:format-symbol
                                                 t "~A-~D" '3d-64x64x64 i)))
                            ,@(loop :for i :below 32
                                    :collect `(textures
                                               ,(u:format-symbol
                                                 t "~A-~D" '3d-32x32x32 i)))
                            ,@(loop :for i :below 16
                                    :collect `(textures
                                               ,(u:format-symbol
                                                 t "~A-~D" '3d-16x16x16 i)))
                            ,@(loop :for i :below 8
                                    :collect `(textures
                                               ,(u:format-symbol
                                                 t "~A-~D" '3d-8x8x8 i)))
                            ,@(loop :for i :below 4
                                    :collect `(textures
                                               ,(u:format-symbol
                                                 t "~A-~D" '3d-4x4x4 i)))
                            ,@(loop :for i :below 2
                                    :collect `(textures
                                               ,(u:format-symbol
                                                 t "~A-~D" '3d-2x2x2 i)))
                            ,@(loop :for i :below 1
                                    :collect `(textures
                                               ,(u:format-symbol
                                                 t "~A-~D" '3d-1x1x1 i))))
            :collect `(,elidx (c:image-element :logloc ,form))))

  ;; TODO: Figure out mipmap and mapping-span generation.

  ;;64x64
  `(c:mipmap-3d
    :extent (c:span-3d)
    ,@(loop :for i :below 64
            :for elidx = (+ i 0)
            :collect `(c:mapping-span-3d
                       :to (c:data-span-3d)
                       :from (c:data-span-3d :elidx ,elidx))))

  ;; 32x32
  `(c:mipmap-3d
    :extent (c:span-3d)
    ,@(loop :for i :below 32
            :for elidx = (+ i 64)
            :collect `(c:mapping-span-3d
                       :to (c:data-span-3d)
                       :from (c:data-span-3d :elidx ,elidx))))
  ;; 16x16
  `(c:mipmap-3d
    :extent (c:span-3d)
    ,@(loop :for i :below 16
            :for elidx = (+ i 96)
            :collect `(c:mapping-span-3d
                       :to (c:data-span-3d)
                       :from (c:data-span-3d :elidx ,elidx))))

  ;; 8x8
  `(c:mipmap-3d
    :extent (c:span-3d)
    ,@(loop :for i :below 8
            :for elidx = (+ i 112)
            :collect `(c:mapping-span-3d
                       :to (c:data-span-3d)
                       :from (c:data-span-3d :elidx ,elidx))))

  ;; 4x4
  `(c:mipmap-3d
    :extent (c:span-3d)
    ,@(loop :for i :below 4
            :for elidx = (+ i 120)
            :collect `(c:mapping-span-3d
                       :to (c:data-span-3d)
                       :from (c:data-span-3d :elidx ,elidx))))
  ;; 2x2
  `(c:mipmap-3d
    :extent (c:span-3d)
    ,@(loop :for i :below 2
            :for elidx = (+ i 124)
            :collect `(c:mapping-span-3d
                       :to (c:data-span-3d)
                       :from (c:data-span-3d :elidx ,elidx))))
  ;; 1x1
  `(c:mipmap-3d
    :extent (c:span-3d)
    ,@(loop :for i :below 1
            :for elidx = (+ i 126)
            :collect `(c:mapping-span-3d
                       :to (c:data-span-3d)
                       :from (c:data-span-3d :elidx ,elidx)))))
;; |
;; | Grounded form after reading image headers.
;; |
;; V
(c:define-texture-map g001-3d-phy-inf-all-non (:3d :unique (:slices :xy-z))
  ;; TODO: make this kind of abstraction better.
  `(c:data-elements
    ,@(loop :for elidx :by 1
            :for form :in `(,@(loop :for i :below 64
                                    :collect `(textures
                                               ,(u:format-symbol
                                                 t "~A-~D" '3d-64x64x64 i)))
                            ,@(loop :for i :below 32
                                    :collect `(textures
                                               ,(u:format-symbol
                                                 t "~A-~D" '3d-32x32x32 i)))
                            ,@(loop :for i :below 16
                                    :collect `(textures
                                               ,(u:format-symbol
                                                 t "~A-~D" '3d-16x16x16 i)))
                            ,@(loop :for i :below 8
                                    :collect `(textures
                                               ,(u:format-symbol
                                                 t "~A-~D" '3d-8x8x8 i)))
                            ,@(loop :for i :below 4
                                    :collect `(textures
                                               ,(u:format-symbol
                                                 t "~A-~D" '3d-4x4x4 i)))
                            ,@(loop :for i :below 2
                                    :collect `(textures
                                               ,(u:format-symbol
                                                 t "~A-~D" '3d-2x2x2 i)))
                            ,@(loop :for i :below 1
                                    :collect `(textures
                                               ,(u:format-symbol
                                                 t "~A-~D" '3d-1x1x1 i))))
            :collect `(,elidx (c:image-element :logloc ,form))))


  ;; 64x64x64 mipmap
  `(c:mipmap-3d
    :extent (c:span-3d :origin (v3:vec 0f0 0f0 0f0)
                       :extent (v3:vec 64f0 64f0 64f0))
    ,@(loop ::for i :below 64
            :for elidx = (+ i 0)
            :for z-layer = (float i 1f0)
            :collect `(c:mapping-span-3d
                       :to (c:data-span-3d
                            :origin (v3:vec 0f0 0f0 ,z-layer)
                            :extent (v3:vec 64f0 64f0 1f0))
                       :from (c:data-span-3d
                              :origin (v3:vec 0f0 0f0 0f0)
                              :extent (v3:vec 64f0 64f0 1f0)
                              :elidx ,elidx))))

  ;; 32x32x32 mipmap
  `(c:mipmap-3d
    :extent (c:span-3d :origin (v3:vec 0f0 0f0 0f0)
                       :extent (v3:vec 32f0 32f0 32f0))
    ,@(loop :for i :below 32
            :for elidx = (+ i 64)
            :for z-layer = (float i 1f0)
            :collect `(c:mapping-span-3d
                       :to (c:data-span-3d
                            :origin (v3:vec 0f0 0f0 ,z-layer)
                            :extent (v3:vec 32f0 32f0 1f0))
                       :from (c:data-span-3d
                              :origin (v3:vec 0f0 0f0 0f0)
                              :extent (v3:vec 32f0 32f0 1f0)
                              :elidx ,elidx))))

  ;; 16x16x16 mipmap
  `(c:mipmap-3d
    :extent (c:span-3d :origin (v3:vec 0f0 0f0 0f0)
                       :extent (v3:vec 16f0 16f0 16f0))
    ,@(loop :for i :below 16
            :for elidx = (+ i 96)
            :for z-layer = (float i 1f0)
            :collect `(c:mapping-span-3d
                       :to (c:data-span-3d
                            :origin (v3:vec 0f0 0f0 ,z-layer)
                            :extent (v3:vec 16f0 16f0 1f0))
                       :from (c:data-span-3d
                              :origin (v3:vec 0f0 0f0 0f0)
                              :extent (v3:vec 16f0 16f0 1f0)
                              :elidx ,elidx))))

  ;; 8x8x8 mipmap
  `(c:mipmap-3d
    :extent (c:span-3d :origin (v3:vec 0f0 0f0 0f0)
                       :extent (v3:vec 8f0 8f0 8f0))
    ,@(loop :for i :below 8
            :for elidx = (+ i 112)
            :for z-layer = (float i 1f0)
            :collect `(c:mapping-span-3d
                       :to (c:data-span-3d
                            :origin (v3:vec 0f0 0f0 ,z-layer)
                            :extent (v3:vec 8f0 8f0 1f0))
                       :from (c:data-span-3d
                              :origin (v3:vec 0f0 0f0 0f0)
                              :extent (v3:vec 8f0 8f0 1f0)
                              :elidx ,elidx))))

  ;; 4x4x4 mipmap
  `(c:mipmap-3d
    :extent (c:span-3d :origin (v3:vec 0f0 0f0 0f0)
                       :extent (v3:vec 4f0 4f0 4f0))
    ,@(loop :for i :below 4
            :for elidx = (+ i 120)
            :for z-layer = (float i 1f0)
            :collect `(c:mapping-span-3d
                       :to (c:data-span-3d
                            :origin (v3:vec 0f0 0f0 ,z-layer)
                            :extent (v3:vec 4f0 4f0 1f0))
                       :from (c:data-span-3d
                              :origin (v3:vec 0f0 0f0 0f0)
                              :extent (v3:vec 4f0 4f0 1f0)
                              :elidx ,elidx))))

  ;; 2x2x2 mipmap
  `(c:mipmap-3d
    :extent (c:span-3d :origin (v3:vec 0f0 0f0 0f0)
                       :extent (v3:vec 2f0 2f0 2f0))
    ,@(loop :for i :below 2
            :for elidx = (+ i 124)
            :for z-layer = (float i 1f0)
            :collect `(c:mapping-span-3d
                       :to (c:data-span-3d
                            :origin (v3:vec 0f0 0f0 ,z-layer)
                            :extent (v3:vec 2f0 2f0 1f0))
                       :from (c:data-span-3d
                              :origin (v3:vec 0f0 0f0 0f0)
                              :extent (v3:vec 2f0 2f0 1f0)
                              :elidx ,elidx))))

  ;; 1x1x1 mipmap
  `(c:mipmap-3d
    :extent (c:span-3d :origin (v3:vec 0f0 0f0 0f0)
                       :extent (v3:vec 1f0 1f0 1f0))
    ,@(loop :for i :below 1
            :for elidx = (+ i 126)
            :for z-layer = (float i 1f0)
            :collect `(c:mapping-span-3d
                       :to (c:data-span-3d
                            :origin (v3:vec 0f0 0f0 ,z-layer)
                            :extent (v3:vec 1f0 1f0 1f0))
                       :from (c:data-span-3d
                              :origin (v3:vec 0f0 0f0 0f0)
                              :extent (v3:vec 1f0 1f0 1f0)
                              :elidx ,elidx)))))
