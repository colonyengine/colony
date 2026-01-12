(in-package #:colony.texture-map)

;; The stuff below this line is the new code. Finish it.
;; -------------------------------------------------------------------------

;; Used to find the physical mipmap location data from the materialized
;; data-elements.
(defgeneric deduce-physical-mipmaps (texmap-inst style store)
  (:documentation
   "During texture-map synthesis ONLY, figure out, using only the
materialized data-elements in the TEXMAP-INST and the STYLE and STORE,
the physical location and size of each mipmap. This assumes (and
requires) the data for the mipmaps is ordered from biggest to smallest
in the data-elements. For 3d texture-maps, additionally the slices must
be in order.

Returns a hash-table with an EQUAL test with this in it:

 Mipmap Extent Key: (WIDTH HEIGHT DEPTH)

 Value: ( (idxA (START-W START-H START-D) (END-W END-H END-D))
          ...
          (idxN (START-W START-H START-D) (END-W END-H END-D)) )

The value ALWAYS represents a cuboid and we treat a single image
as having a depth of 1 pixel. The idx* is the data-element from where we
need to get the data. The START-W/H/D and END-W/H/D represent the location
in the source represented by the elidx for where to find the actual data.
The START-* value is inclusive and the END-* value is exclusive."))

(defgeneric deduce-mipmap-hierarchy (texmap-inst style store
                                     &key core &allow-other-keys)
  (:documentation
   ""))

;; -------------------------------------------------------------------------



;; Used for all :1d, :2d, and :3d texture-maps whose style is :unique
;; and :3d style must be (:slices ...)
(defmethod deduce-physical-mipmaps ((texmap-inst texture-map-simple)
                                    (style (eql :unique))
                                    store)
  (declare (ignore store))

  ;; We're going to temporarily take advantage of the fact that all
  ;; image formats are actually 2 dimensions.
  (let ((delems (texmap:data-elements texmap-inst))
        ;; Key: (WIDTH HEIGHT)
        ;; Value: integer representing total number
        ;; of data-elements with this width and height.
        (total-depth-table (u:dict #'equal))
        ;; Key: (WIDTH HEIGHT)
        ;; Value: integer representing _current_ depth for a data-element with
        ;; this width and height. The current depth always starts at 0.
        (depth-table (u:dict #'equal))
        ;; Key: (WIDTH HEIGHT DEPTH) [The mipmap extent.]
        ;; Value: ( (idxA (START-W START-H START-D) (END-W END-H END-D))
        ;;          ...
        ;;          (idxN (START-W START-H START-D) (END-W END-H END-D)) )
        ;; The value ALWAYS represents a cuboid, and we treat a single image
        ;; as having a depth of 1 pixel.
        ;;
        ;; The idx* order is preserved as found in the data-elements,
        ;; and the START-* is inclusive and the END-* is exclusive in
        ;; the range of pixel indices in the images.
        (res-table (u:dict #'equal)))

    ;; NOTE: We make an assumption that the data-elements are in
    ;; canonical form (sorted largest to smallest (with correct ordering for
    ;; slices in the case of :3d/slice-* texture-maps)).
    ;;
    ;; For ease of maintenance, we do multiple passes.

    ;; Pass 1: count how many DEPTH slices there will be for each
    ;; ACTUAL WIDTH/HEIGHT image.
    (dotimes (idx (length delems))
      (let* ((delem (aref delems idx))
             (image (rc:value (texmap:element delem)))
             (width (img:width image))
             (height (img:height image)))
        (incf (gethash (list width height) total-depth-table 0))))

    ;; Pass 2: Initialize the depth-table for each mipmap. 1d and 2d
    ;; mipmaps will only have their depth start at 0, but slices of 3d
    ;; mipmaps can exist at different depth values as they are being
    ;; processed.
    (u:do-hash-keys (wxh total-depth-table)
      (setf (u:href depth-table wxh) 0))

    ;; Pass 3: Assemble the index lists preserving encounter order for
    ;; images of the same WxH.
    (dotimes (idx (length delems))
      (let* ((delem (aref delems idx))
             (image (rc:value (texmap:element delem)))
             (mipmap-width (img:width image))
             (mipmap-height (img:height image))
             (wxh (list mipmap-width mipmap-height))
             (mipmap-depth (u:href total-depth-table wxh))
             (current-depth (u:href depth-table wxh))
             (mipmap-extent (list mipmap-width mipmap-height mipmap-depth)))

        (push (list idx
                    (list 0 0 current-depth)
                    (list mipmap-width mipmap-height (1+ current-depth)))
              (u:href res-table mipmap-extent))
        (incf (u:href depth-table wxh))))

    ;; Finally, reverse all lists of indices to match observation
    ;; order in data-elements.
    (u:do-hash (mipmap-key mipmap-values res-table)
      (setf (u:href res-table mipmap-key) (nreverse mipmap-values)))

    ;; DEBUG
    (u:do-hash (mipmap-key mipmap-values res-table)
      (debug-rectification
       "texture-map-simple" :synthesize :gather-unique-mipmaps
       "mipmap-key: ~S -> mipmap-values: ~A~%" mipmap-key mipmap-values))
    (format t "sorted mipmap extents: ~A~%" (sort-mipmap-extents res-table))

    res-table))

;;; TODO: We have to reconstruct the dimensions of a set of mipmaps that can be
;;; layed out in the manner we need. The identity W = 2N - popcount(N) where N
;;; is the base mipmap size is the starting point for how to do this. A little
;;; bit of heuristic searchin must hapen because popcount() is a non-linear
;;; function. This next function is part of that guessing system.
(defun mipmap-1d-sum (base-extent-width)
  "If BASE-EXTENT is the width of a 1d mipmap, then the result of this
function is the sum of the mipmaps width as if all of them were layed
out along that dimension BASE-EXTENT does not have to be a power of two."
  (- (* base-extent-width 2) (logcount base-extent-width)))



;; KEEP GOING
(defmethod deduce-physical-mipmaps ((texmap-inst texture-map-1d)
                                    (style (eql :combined))
                                    (store list))
  (format t "deduce-physical-mipmaps :combined trampoline (?) to ~A~%"
          (first store))

  ;; Sort of a hack since STORE is a list.
  (cond
    ((= (length store) 1)
     (deduce-physical-mipmaps texmap-inst style (first store)))
    (t
     (error "Not yet implemented for texture: ~A" texmap-inst))))



(defmethod deduce-physical-mipmaps ((texmap-inst texture-map-1d)
                                    (style (eql :combined))
                                    (store (eql :horizontal-left-small)))
  nil)
(defmethod deduce-physical-mipmaps ((texmap-inst texture-map-1d)
                                    (style (eql :combined))
                                    (store (eql :horizontal-left-big)))
  nil)
(defmethod deduce-physical-mipmaps ((texmap-inst texture-map-1d)
                                    (style (eql :combined))
                                    (store (eql :vertical-top-left-small)))
  nil)
(defmethod deduce-physical-mipmaps ((texmap-inst texture-map-1d)
                                    (style (eql :combined))
                                    (store (eql :vertical-top-left-big)))
  ;; TODO: Implement me first cause much easier.

  (let ((delems (texmap:data-elements texmap-inst))
        (res-table (u:dict #'equal)))

    ;; Can only have a single combined image for all mipmaps in this context
    ;; of synthesis.
    (assert (= (length delems) 1))

    ;; KEEP GOING

    (let* ((idx 0)
           (delem (aref delems idx))
           (image (rc:value (texmap:element delem)))
           (combined-mipmap-width (img:width image))
           (combined-mipmap-height (img:height image)))

      (loop
        :for mipmap-row :from 0 :below combined-mipmap-height
        :with current-mipmap-width = combined-mipmap-width
        :do ;; We pick out a single 1D slice from the combined image
            ;; that corresponds to the correct 1d slice for this STORE
            ;; kind.
            (let ((mipmap-start (list 0 mipmap-row 0))
                  (mipmap-extent (list current-mipmap-width (1+ mipmap-row) 1)))
              (push (list idx mipmap-start mipmap-extent)
                    (u:href res-table (list current-mipmap-width 1 1)))
              (setf current-mipmap-width (floor (/ current-mipmap-width 2)))))
      res-table)))
(defmethod deduce-physical-mipmaps ((texmap-inst texture-map-1d)
                                    (style (eql :combined))
                                    (store (eql :vertical-top-center-small)))
  nil)
(defmethod deduce-physical-mipmaps ((texmap-inst texture-map-1d)
                                    (style (eql :combined))
                                    (store (eql :vertical-top-center-big)))
  nil)
(defmethod deduce-physical-mipmaps ((texmap-inst texture-map-1d)
                                    (style (eql :combined))
                                    (store (eql :vertical-top-right-small)))
  nil)
(defmethod deduce-physical-mipmaps ((texmap-inst texture-map-1d)
                                    (style (eql :combined))
                                    (store (eql :vertical-top-right-big)))
  nil)



(defun check-deducible-mipmap-hierarchy (texmap-inst)
  "Signal an ERROR condition if the TEXMAP-INST is not materialized, or
not in a :synthesize state."
  (let ((state (texmap:state texmap-inst)))
    (unless (texmap:materialized-p state)
      (error
       "Unmaterialized texmap ~A cannot have the mipmap hierarchy deduced."
       (texmap:name texmap-inst)))
    (unless (eq (texmap:rectification-classification state) :synthesize)
      (error
       "Texture-map ~A cannot have the mipmap hierarchy deduced since it isn't
in the :synthesize state."
       (texmap:name texmap-inst))))
  t)

(defun sort-mipmap-extents (res-table)
  "Return a descending sort of the extent keys in the RES-TABLE."
  (sort (u:hash-table-keys res-table)
        ;; Sort by volume.
        (lambda (left right)
          (destructuring-bind (left-width left-height left-depth) left
            (destructuring-bind (right-width right-height right-depth) right
              (> (* left-width left-height left-depth)
                 (* right-width right-height right-depth)))))))


;; TODO: If I play my cards right, deduce-physical-mipmaps will do the
;; right thing for style :unique and :combine and I can combine the
;; :unique and :combined forms of this method. It isn't guaranteed that
;; this will work out though...
;;
;;
;; COMBINE The :unique and :combined forms together. Change the making of
;; the mipmap-1d to use data-span-2d. I hacked this in, but verify and remove
;; the :combined codebase. DEDUCE-PHYSICAL-MIPMAPS is doing all the hard work.
;;

(defmethod deduce-mipmap-hierarchy ((texmap-inst texture-map-1d)
                                    ;;(style (eql :unique))
                                    style
                                    store
                                    &key core)
  (declare (ignore core))
  (check-deducible-mipmap-hierarchy texmap-inst)

  (let* (;; NOTE: The res-table keys contains the ACTUAL dimensions of
         ;; the materialized images in the data-elements gotten by
         ;; gather-unique-mipmaps via the resource cache
         (res-table (deduce-physical-mipmaps texmap-inst
                                             (texmap:style texmap-inst)
                                             (texmap:store texmap-inst)))
         (descending-mipmap-extent-keys (sort-mipmap-extents res-table))
         (largest-mipmap-extent (car descending-mipmap-extent-keys)))

    ;; NOTE: There must not be any height or depth for synthesized 1d
    ;; texture maps.
    (loop :for (width height depth) :in descending-mipmap-extent-keys
          :do (assert (= 1 height depth)))

    (format t "res-table is:~%")
    (u:do-hash (whd loc res-table)
      (format t " ~S -> ~S~%" whd loc))

    (multiple-value-bind (num-levels computed-extents)
        (destructuring-bind (width height depth) largest-mipmap-extent
          (u:compute-mipmap-levels width height depth))
      (declare (ignore num-levels))
      (let ((mipmap-obs
              (u:ragged-mapcar
               :dne
               (lambda (computed-extent mipmap-extent)
                 (multiple-value-bind (elidx-spec present-p)
                     (u:href res-table computed-extent)
                   (cond
                     (present-p
                      ;; This assertion checks that the materialized
                      ;; size of the image actually matches the
                      ;; expected mipmap extents we're expecting at
                      ;; this mipmap level. This finds bugs where
                      ;; one mipmap level might be the wrong on disk
                      ;; resolution, etc.
                      ;;
                      ;; TODO: Present this constraint error better
                      ;; to the appdev.
                      (assert (equal computed-extent mipmap-extent))

                      ;; There can only be one data-element elidx-spec
                      ;; source for this unique 1d mipmap.
                      (assert (= (length elidx-spec) 1))

                      ;; Construct the mipmap-1d for the identified section
                      ;; the elidx-spec identifies.
                      (destructuring-bind (elidx
                                           (start-w start-h start-d)
                                           (end-w end-h end-d))
                          (first elidx-spec)
                        (declare (ignore start-h start-d end-h end-d))
                        (let* ((mipmap-width (car mipmap-extent))
                               ;; Finally build the mipmap-1d instance.
                               ;; and :to in the mapping-span-1d.

                               ;; TODO: FIX DATA-SPANS FOR 2D IMAGES?
                               (mipmap-1d
                                 (make-mipmap-1d
                                  :sourced-p t
                                  :extent (make-span-1d
                                           :origin 0
                                           :extent mipmap-width)
                                  :mapping-spans
                                  (make-mapping-spans
                                   :encode
                                   (make-mapping-span-1d
                                    :from (make-data-span-1d
                                           :origin start-w
                                           :extent (- end-w start-w)
                                           :elidx elidx)
                                    :to (make-data-span-1d
                                         :origin 0
                                         :extent mipmap-width))))))
                          (list computed-extent mipmap-1d))))
                     (t
                      ;; We simply record that we don't have a known
                      ;; mipmap description for this extent.
                      (list computed-extent :dne)))))
               computed-extents
               descending-mipmap-extent-keys)))
        ;; Finally we return the answer.
        (list (cons (texmap:name texmap-inst) mipmap-obs))))))



;; TODO: See if I can just use the above copy and fix to make mipmap-1d
;; but with 2d data spans cause we're picking from 2D images. This code
;; duplication with MINOR changesis a bad antipattern and
;; unmaintainable.
#++
(defmethod deduce-mipmap-hierarchy ((texmap-inst texture-map-1d)
                                    (style (eql :combined)) store
                                    &key core)
  (declare (ignore core))
  (check-deducible-mipmap-hierarchy texmap-inst)

  (let* (;; NOTE: The res-table keys contains the ACTUAL dimensions of
         ;; the materialized images in the data-elements gotten by
         ;; gather-unique-mipmaps via the resource cache
         (res-table (deduce-physical-mipmaps texmap-inst
                                             (texmap:style texmap-inst)
                                             (texmap:store texmap-inst)))
         (descending-mipmap-extent-keys (sort-mipmap-extents res-table))
         (largest-mipmap-extent (car descending-mipmap-extent-keys)))

    ;; NOTE: There must not be any height or depth for synthesized 1d
    ;; texture maps.
    (loop :for (width height depth) :in descending-mipmap-extent-keys
          :do (assert (= 1 height depth)))

    (format t "res-table is:~%")
    (u:do-hash (whd loc res-table)
      (format t " ~S -> ~S~%" whd loc))

    (multiple-value-bind (num-levels computed-extents)
        (destructuring-bind (width height depth) largest-mipmap-extent
          (u:compute-mipmap-levels width height depth))
      (declare (ignore num-levels))
      (let ((mipmap-obs
              (u:ragged-mapcar
               :dne
               (lambda (computed-extent mipmap-extent)
                 (multiple-value-bind (elidx-spec present-p)
                     (u:href res-table computed-extent)
                   (cond
                     (present-p
                      ;; This assertion checks that the materialized
                      ;; size of the image actually matches the
                      ;; expected mipmap extents we're expecting at
                      ;; this mipmap level. This finds bugs where
                      ;; one mipmap level might be the wrong on disk
                      ;; resolution, etc.
                      ;;
                      ;; TODO: Present this constraint error better
                      ;; to the appdev.
                      (assert (equal computed-extent mipmap-extent))

                      ;; There can only be one data-element elidx-spec
                      ;; source for this unique 1d mipmap.
                      (assert (= (length elidx-spec) 1))

                      ;; Construct the mipmap-1d for the identified section
                      ;; the elidx-spec identifies.
                      (destructuring-bind (elidx
                                           (start-w start-h start-d)
                                           (end-w end-h end-d))
                          (first elidx-spec)
                        (declare (ignore start-h start-d end-h end-d))
                        (let* ((mipmap-width (car mipmap-extent))
                               ;; Finally build the mipmap-1d instance.
                               ;; and :to in the mapping-span-1d.

                               ;; TODO: FIX DATA-SPANS FOR 2D IMAGES?
                               (mipmap-1d
                                 (make-mipmap-1d
                                  :sourced-p t
                                  :extent (make-span-1d
                                           :origin 0
                                           :extent mipmap-width)
                                  :mapping-spans
                                  (make-mapping-spans
                                   :encode
                                   (make-mapping-span-1d
                                    :from (make-data-span-1d
                                           :origin start-w
                                           :extent (- end-w start-w)
                                           :elidx elidx)
                                    :to (make-data-span-1d
                                         :origin 0
                                         :extent mipmap-width))))))
                          (list computed-extent mipmap-1d))))
                     (t
                      ;; We simply record that we don't have a known
                      ;; mipmap description for this extent.
                      (list computed-extent :dne)))))
               computed-extents
               descending-mipmap-extent-keys)))
        ;; Finally we return the answer.
        (list (cons (texmap:name texmap-inst) mipmap-obs))))))































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
