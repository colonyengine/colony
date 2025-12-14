(in-package #:colony.texture-map)

(defgeneric deduce-mipmap-hierarchy (texmap-inst style store
                                     &key core &allow-other-keys)
  (:documentation
   "TODO"))


;;; -----------------
;;; Utility Functions
;;; -----------------
(defun group-data-elements-by-resolution/unique (texmap-inst)
  "We get the data-elements from the materialized TEXMAP-INST for models
:1d, :2d, :3d/(:slices <any>), :cube/:envmap--but only for styles which
are :UNIQUE. WE ASSUME the data-elements are in canonical form. Return
two values. The first value is a ((X Y Z) (elidxA ... elidxZ)) form that
describes the mipmap extents and participating DATA-ELEMENTS in each
extent or NIL if something went wrong. The second value is :OK if
nothing went wrong or a keyword symbol indicating the reason.

This function will put NIL for the (elidxA ... elidxZ) form if there
were no DATA-ELEMENTS to satisfy that resolution. This function will
return a complete set of mipmap resolutions meaning starting from the
biggest resolution, the final one will be (1 1 1)."

  ;; TODO: This code probably doesn't handle the case when data-elements
  ;; actually contains many (1 1 1) mipmaps!

  ;; TODO: This is clunky. Clean up later.
  (unless (or (member (texmap:model texmap-inst) '(:1d :2d))
              (and (eq (texmap:model texmap-inst) :3d)
                   (listp (texmap:store texmap-inst))
                   (eq (car (texmap:store texmap-inst)) :slices))
              (and (eq (texmap:model texmap-inst) :cube)
                   (eq (car texmap-inst) :envmap)))
    (return-from group-data-elements-by-resolution/unique
      (values NIL :unsupported-model-form)))

  ;; We're going to temporarily take advantage of the fact that all
  ;; image formats are actually 2 dimensions.
  (let ((delems (texmap:data-elements texmap-inst))
        ;; Key: (WIDTH HEIGHT)
        ;; Value: (idxA ... idxZ) [Need to reverse these when done.]
        (res-table (u:dict #'equal)))

    ;; NOTE: This loop makes an assumption that the data-elements are in
    ;; canonical form (sorted largest to smallest and in the case of :3d
    ;; (:slices <any>) the slices are in a contiguous group per mipmap
    ;; and also in the right slice order).
    (dotimes (idx (length delems))
      (let* ((delem (aref delems idx))
             (image (rc:value (texmap:element delem)))
             (width (img:width image))
             (height (img:height image)))
        (push idx (u:href res-table (list width height)))))

    ;; Get the available resolutions and sort ascendingly.
    (let* ((ascending-keys
             (sort (u:hash-table-keys res-table)
                   ;; Sort by area.
                   (lambda (left right)
                     (destructuring-bind (left-width left-height) left
                       (destructuring-bind (right-width right-height) right
                         (> (* left-width left-height)
                            (* right-width right-height)))))))

           ;; Build partial results given only what is in the data-elements.
           (delem-extents
             (mapcar
              (lambda (key)
                (let* ((participating-elems
                         (reverse (u:href res-table key)))
                       ;; TODO: This next line is a lie. For :3d models
                       ;; and :slices store forms we need to treat what
                       ;; is actually width/height/depth more
                       ;; accurately.
                       (depth (length participating-elems)))
                  (destructuring-bind (width height) key
                    `((,width ,height ,depth) ,participating-elems))))
              ascending-keys))
           (largest-delem-extent
             (caar delem-extents)))

      ;; Now, u:compute-mipmap-extents from the largest one and resolve
      ;; them together comparing the real resolutions found in the
      ;; delem-extents with the one from compute-mipmap-extents and the
      ;; adding in any addition extents down to (1 1 1) with NIL as the
      ;; elidxs as appropriate.
      (multiple-value-bind (num-levels computed-extents)
          (destructuring-bind (width height depth) largest-delem-extent
            (u:compute-mipmap-levels width height depth))

        ;; TODO: The relationship between num-levels and length
        ;; delem-extents needs meaning assigned.

        (flet ((resolve (dext cext)
                 ;; KEEP GOING: Resolve two possibly unequal in length
                 ;; lists together into one list that will represent all
                 ;; the mipmaps from largest to smallest.
                 nil))
          (u:ragged-mapcar :dne #'resolve delem-extents computed-extents))))))





;; KEEP GOING: Implement below what I wrote above.

;; NOTE: 1d and 2d texmap-inst.
(defmethod old-deduce-mipmap-hierarchy ((texmap-inst texture-map) &key core)
  (declare (ignore core))

  (let ((state (texmap:state texmap-inst)))
    (unless (eq (texmap:state state) :synthesize)
      (error "~A ~A ~S"
             "This GF may not be used during the validation phase of "
             "rectification for the texture-map: "
             texmap-inst)))

  (unless (texmap:materialized-p texmap-inst)
    (error "deduce-mipmap-hierarchy: Unmaterialized 1d or 2d texmap!"))

  (let* ((delems (texmap:data-elements texmap-inst))
         ;; NOTE: We assume data-element 0 is the base level mipmap layer.
         (base-elem (aref delems 0))
         (base-image (rc:value (texmap:element base-elem)))
         (base-image-height (img:height base-image))
         (base-image-width (img:width base-image))
         (base-image-depth 1))

    (ecase (texmap:style texmap-inst)
      (:unique
       (u:compute-mipmap-levels base-image-width base-image-height
                                base-image-depth))
      (:combined
       (ecase (texmap:store texmap-inst)
         (:common
          ;; TODO: We don't actually CHECK the image, we just assume the
          ;; user told us the right thing.
          (u:compute-mipmap-levels
           (- base-image-width (floor (/ base-image-width 3)))
           base-image-height
           base-image-depth)))))))



;; The stuff below this line is the new code. Finish it.
;; -------------------------------------------------------------------------

;; TODO: Implement me:

(defun check-deducible-mipmap-hierarchy (texmap-inst)
  "Signal an ERROR condition if the TEXMAP-INST is not materialized, or
not in a :synthesize state."
  (unless (texmap:materialized-p texmap-inst)
    (error
     "Unmaterialized texmap ~A cannot have the mipmap hierarchy deduced."
     (texmap:name texmap-inst)))
  (let ((state (texmap:state texmap-inst)))
    (unless (eq (texmap:state state) :synthesize)
      (error
       "Texture-map ~A cannot have the mipmap hierarchy deduced with it isn't
in the :synthesize state."
       (texmap:name texmap-inst))))
  t)

(defmethod deduce-mipmap-hierarchy ((texmap-inst texture-map-1d)
                                    (style (eql :unique)) store
                                    &key core)
  (declare (ignore core))
  (check-deducible-mipmap-hierarchy texmap-inst)

  ;; TODO: This entire next code block is broken and needs remaking.

  ;; We're going to temporarily take advantage of the fact that all
  ;; image formats are actually 2 dimensions.
  (let ((delems (texmap:data-elements texmap-inst))
        ;; Key: (WIDTH HEIGHT)
        ;; Value: (idxA ... idxZ) [Need to reverse these when done.]
        (res-table (u:dict #'equal)))

    ;; NOTE: This loop makes an assumption that the data-elements are in
    ;; canonical form (sorted largest to smallest to largest).
    (dotimes (idx (length delems))
      (let* ((delem (aref delems idx))
             (image (rc:value (texmap:element delem)))
             (width (img:width image))
             (height (img:height image)))
        (push idx (u:href res-table (list width height)))))

    ;; Get the available resolutions and sort ascendingly.
    (let* ((ascending-keys
             (sort (u:hash-table-keys res-table)
                   ;; Sort by area.
                   (lambda (left right)
                     (destructuring-bind (left-width left-height) left
                       (destructuring-bind (right-width right-height) right
                         (> (* left-width left-height)
                            (* right-width right-height)))))))

           ;; Build partial results given only what is in the data-elements.
           (delem-extents
             (mapcar
              (lambda (key)
                (let* ((participating-elems
                         (reverse (u:href res-table key)))
                       ;; TODO: This next line is a lie. For :3d models
                       ;; and :slices store forms we need to treat what
                       ;; is actually width/height/depth more
                       ;; accurately.
                       (depth (length participating-elems)))
                  (destructuring-bind (width height) key
                    `((,width ,height ,depth) ,participating-elems))))
              ascending-keys))
           (largest-delem-extent
             (caar delem-extents)))

      ;; Now, u:compute-mipmap-extents from the largest one and resolve
      ;; them together comparing the real resolutions found in the
      ;; delem-extents with the one from compute-mipmap-extents and the
      ;; adding in any addition extents down to (1 1 1) with NIL as the
      ;; elidxs as appropriate.
      (multiple-value-bind (num-levels computed-extents)
          (destructuring-bind (width height depth) largest-delem-extent
            (u:compute-mipmap-levels width height depth))

        ;; TODO: The relationship between num-levels and length
        ;; delem-extents needs meaning assigned.

        (flet ((resolve (dext cext)
                 ;; KEEP GOING: Resolve two possibly unequal in length
                 ;; lists together into one list that will represent all
                 ;; the mipmaps from largest to smallest.
                 nil))
          (u:ragged-mapcar :dne #'resolve delem-extents computed-extents)))))

  nil)

(defmethod deduce-mipmap-hierarchy ((texmap-inst texture-map-1d)
                                    (style (eql :combined)) store
                                    &key core)
  (declare (ignore core))
  (check-deducible-mipmap-hierarchy texmap-inst)

  nil)

(defmethod deduce-mipmap-hierarchy ((texmap-inst texture-map-2d)
                                    (style (eql :unique)) store
                                    &key core)
  (declare (ignore core))
  (check-deducible-mipmap-hierarchy texmap-inst)

  nil)

(defmethod deduce-mipmap-hierarchy ((texmap-inst texture-map-2d)
                                    (style (eql :combined)) store
                                    &key core)
  (declare (ignore core))
  (check-deducible-mipmap-hierarchy texmap-inst)

  nil)

(defmethod deduce-mipmap-hierarchy ((texmap-inst texture-map-3d)
                                    (style (eql :unique)) store
                                    &key core)
  (declare (ignore core))
  (check-deducible-mipmap-hierarchy texmap-inst)

  nil)

(defmethod deduce-mipmap-hierarchy ((texmap-inst texture-map-3d)
                                    (style (eql :combined)) store
                                    &key core)
  (declare (ignore core))
  (check-deducible-mipmap-hierarchy texmap-inst)

  nil)

(defmethod deduce-mipmap-hierarchy ((texmap-inst texture-map-cube)
                                    (style (eql :faces)) store
                                    &key core)
  (declare (ignore core))
  (check-deducible-mipmap-hierarchy texmap-inst)

  nil)

(defmethod deduce-mipmap-hierarchy ((texmap-inst texture-map-cube)
                                    (style (eql :envmap)) store
                                    &key core)
  (declare (ignore core))
  (check-deducible-mipmap-hierarchy texmap-inst)

  nil)
