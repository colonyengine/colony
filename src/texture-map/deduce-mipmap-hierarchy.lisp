(in-package #:colony.texture-map)

;; The stuff below this line is the new code. Finish it.
;; -------------------------------------------------------------------------

;; Used to find the physical mipmap location data from the materialized
;; data-elements.
(defgeneric deduce-physical-mipmaps (texmap-inst style store store-args)
  (:documentation
   "During texture-map synthesis ONLY, figure out, using only the
materialized data-elements in the TEXMAP-INST and the STYLE and STORE,
the physical location and size of each mipmap. This assumes (and
requires) the data for the mipmaps is ordered from biggest to smallest
in the data-elements. For 3d texture-maps, additionally the slices must
be in order.

Returns a hash-table with an EQUAL test with this in it:

 Mipmap Extent Key: (WIDTH HEIGHT DEPTH)

 Value: ( data-span-3d
          ...
          data-span-3d )

The value ALWAYS represents a list of data spans where each of them
specifies one or more data spans from the elidx that collectively fill
in the extent in the mipmap. We treat a single 2d image as having a
depth of 1 pixel and we treat a 1d line as having 1 height and 1 depth.
Each data-spen-3d has an elidx that indicates from where we need to get
the data."))

(defgeneric deduce-mipmap-hierarchy (texmap-inst style store store-args
                                     &key core
                                     &allow-other-keys)
  (:documentation
   "This returns one or more fully specified mipmap objects using the
results of DEDUCE-PHYSICAL-MIPMAPS."))

;; -------------------------------------------------------------------------



;; Used for all :1d, :2d, and :3d texture-maps whose style is :unique
;; and :3d style must be (:slices ...)
(defmethod deduce-physical-mipmaps ((texmap-inst texture-map-simple)
                                    (style (eql :unique))
                                    store store-args)
  (declare (ignore store store-args))

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
             ;; deduce-mipmap-extent?
             (mipmap-extent (list mipmap-width mipmap-height mipmap-depth)))

        ;; Since this method is called for any texture-map-simple, we use a
        ;; data-span-3d to represent the source mipmap region we're extracting
        ;; from the elidx object. The consumer of this result will determine
        ;; if the subspace defined by the data-span-3d is appropriate or not
        ;; for its needs.
        (push (make-data-span-3d
               :elidx idx
               :origin (iv3:vec 0 0 current-depth) ;; deduce-mipmap-origin?
               :extent (iv3:vec mipmap-width ;; deduce-mipmap-extent?
                                mipmap-height
                                1))
              (u:href res-table mipmap-extent))
        (incf (u:href depth-table wxh))))

    ;; Finally, reverse all lists of indices to match observation
    ;; order in data-elements.
    (u:do-hash (mipmap-key mipmap-values res-table)
      (setf (u:href res-table mipmap-key) (nreverse mipmap-values)))

    ;; TODO: If STORE is :slices, at this point we have to reorient the
    ;; mipmap extent (W H D) dimensions to map the slice kind in the
    ;; store args so the mipmap extent matches the actual slice data in
    ;; the appropriate dimensions.
    ;;
    ;; TODO: If I implement the deduce-* stuff above, I don't need this here.
    (when (eq store :slices)
      ;; TODO: bad place for default, put in DSL parser
      (ecase (or (car store-args) :xy-z)
        (:xy-z nil)
        (:xz-y nil)
        (:yz-x nil)))


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


;;;
;;; 1D texture map synthesis support
;;;

(defmethod deduce-physical-mipmaps ((texmap-inst texture-map-1d)
                                    (style (eql :combined))
                                    (store (eql :horizontal-left-small))
                                    store-args)
  nil)
(defmethod deduce-physical-mipmaps ((texmap-inst texture-map-1d)
                                    (style (eql :combined))
                                    (store (eql :horizontal-left-big))
                                    store-args)
  nil)
(defmethod deduce-physical-mipmaps ((texmap-inst texture-map-1d)
                                    (style (eql :combined))
                                    (store (eql :vertical-top-left-small))
                                    store-args)
  nil)
(defmethod deduce-physical-mipmaps ((texmap-inst texture-map-1d)
                                    (style (eql :combined))
                                    (store (eql :vertical-top-left-big))
                                    store-args)
  (let ((delems (texmap:data-elements texmap-inst))
        (res-table (u:dict #'equal)))
    ;; Can only have a single combined image for all mipmaps in this context
    ;; of synthesis.
    (assert (= (length delems) 1))
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
            ;; kind. We return it as a 3D data span though. It is up to
            ;; the caller to ensure it is of the correct subspan.
            (push (make-data-span-3d
                   :elidx idx
                   :origin (iv3:vec 0
                                    (- combined-mipmap-height mipmap-row 1)
                                    0)
                   :extent (iv3:vec current-mipmap-width 1 1))
                  (u:href res-table (list current-mipmap-width 1 1)))
            (setf current-mipmap-width (floor (/ current-mipmap-width 2))))
      res-table)))
(defmethod deduce-physical-mipmaps ((texmap-inst texture-map-1d)
                                    (style (eql :combined))
                                    (store (eql :vertical-top-center-small))
                                    store-args)
  nil)
(defmethod deduce-physical-mipmaps ((texmap-inst texture-map-1d)
                                    (style (eql :combined))
                                    (store (eql :vertical-top-center-big))
                                    store-args)
  nil)
(defmethod deduce-physical-mipmaps ((texmap-inst texture-map-1d)
                                    (style (eql :combined))
                                    (store (eql :vertical-top-right-small))
                                    store-args)
  nil)
(defmethod deduce-physical-mipmaps ((texmap-inst texture-map-1d)
                                    (style (eql :combined))
                                    (store (eql :vertical-top-right-big))
                                    store-args)
  nil)



;;;
;;; 2D texture map synthesis support
;;;



(defmethod deduce-physical-mipmaps ((texmap-inst texture-map-2d)
                                    (style (eql :combined))
                                    (store (eql :common))
                                    store-args)

  (let ((delems (texmap:data-elements texmap-inst))
        (res-table (u:dict #'equal)))
    ;; Can only have a single combined image for all mipmaps in this
    ;; context of synthesis.
    (assert (= (length delems) 1))
    (let* ((idx 0)
           (delem (aref delems idx))
           (image (rc:value (texmap:element delem)))
           (combined-mipmap-width (img:width image))
           (combined-mipmap-height (img:height image))
           (base-mipmap-width (max 1 (- combined-mipmap-width
                                        (floor (/ combined-mipmap-width 3)))))
           (base-mipmap-height combined-mipmap-height))
      (loop
        :with running-height = 0
        :with current-mipmap-width = base-mipmap-width
        :with current-mipmap-height = base-mipmap-height

        :for mipmap-level :from 0
          :below (1+ (floor (log (max base-mipmap-width
                                      base-mipmap-height)
                                 2)))

        :do ;; We pick out a single 2D subrectangle from the combined
            ;; image that corresponds to the correct 2d mipmap for this
            ;; STORE kind. We return it as a 3D data span though with 1
            ;; depth. It is up to the caller to ensure it is of the
            ;; correct subspan.

            (setf current-mipmap-width
                  (max 1 (floor (/ base-mipmap-width (expt 2 mipmap-level))))
                  current-mipmap-height
                  (max 1 (floor (/ base-mipmap-height (expt 2 mipmap-level)))))

            (when (plusp mipmap-level)
              (incf running-height current-mipmap-height))

            (push (make-data-span-3d
                   :elidx idx
                   :origin (if (zerop mipmap-level)
                               ;; base mipmap origin
                               (iv3:vec 0 0 0)
                               ;; other mipmap origins
                               (iv3:vec base-mipmap-width
                                        (- base-mipmap-height running-height)
                                        0))
                   :extent (iv3:vec current-mipmap-width
                                    current-mipmap-height
                                    1))
                  (u:href res-table (list current-mipmap-width
                                          current-mipmap-height
                                          1))))

      res-table)))











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


(defmethod deduce-mipmap-hierarchy ((texmap-inst texture-map-1d)
                                    style ;; Both :unique and :combined
                                    store store-args
                                    &key core)
  (declare (ignore core))
  (check-deducible-mipmap-hierarchy texmap-inst)

  (let* (;; NOTE: The res-table keys contain the ACTUAL dimensions of
         ;; the materialized images in the data-elements gotten by
         ;; gather-unique-mipmaps via the resource cache
         (res-table (deduce-physical-mipmaps texmap-inst
                                             (texmap:style texmap-inst)
                                             (texmap:store texmap-inst)
                                             (texmap:store-args texmap-inst)))
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
                      (let* ((dspan-3d (first elidx-spec))
                             (mipmap-width (car mipmap-extent))
                             ;; TODO: We assume the :from is going to
                             ;; be a 2D image. This doesn't have to be
                             ;; the case (like reading from a buffer
                             ;; instead of an image) and we will
                             ;; support it later when we actually do
                             ;; that.
                             (mipmap-1d
                               (make-mipmap-1d
                                :sourced-p t
                                :extent (make-span-1d
                                         :origin 00
                                         :extent mipmap-width)
                                :mapping-spans
                                (make-mapping-spans
                                 :encode
                                 (make-mapping-span-1d
                                  ;; We select a 1-d subspace of an N
                                  ;; pixel wide, 1 pixel high, and 1
                                  ;; pixel deep 3D span from the (2d)
                                  ;; image...
                                  :from dspan-3d
                                  ;; ...and place it here in the 1d
                                  ;; extent of this mipmap.
                                  :to (make-data-span-1d
                                       :origin 0
                                       :extent mipmap-width))))))
                        (list computed-extent mipmap-1d)))
                     (t
                      ;; We simply record that we don't have a known
                      ;; mipmap description for this extent.
                      (list computed-extent :dne)))))
               computed-extents
               descending-mipmap-extent-keys)))
        ;; Finally we return the answer.
        (list (cons (texmap:name texmap-inst) mipmap-obs))))))


(defmethod deduce-mipmap-hierarchy ((texmap-inst texture-map-2d)
                                    style ;; oth :unique and :combined
                                    store store-args
                                    &key core)
  (declare (ignore core))
  (check-deducible-mipmap-hierarchy texmap-inst)

  (let* (;; NOTE: The res-table keys contain the ACTUAL dimensions of
         ;; the materialized images in the data-elements gotten by
         ;; gather-unique-mipmaps via the resource cache
         (res-table (deduce-physical-mipmaps texmap-inst
                                             (texmap:style texmap-inst)
                                             (texmap:store texmap-inst)
                                             (texmap:store-args texmap-inst)))
         (descending-mipmap-extent-keys (sort-mipmap-extents res-table))
         (largest-mipmap-extent (car descending-mipmap-extent-keys)))

    ;; NOTE: There must not be any depth for synthesized 2d
    ;; texture maps.
    (loop :for (width height depth) :in descending-mipmap-extent-keys
          :do (assert (= 1 depth)))

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
                      (let* ((dspan-3d (first elidx-spec))
                             (mipmap-width (first mipmap-extent))
                             (mipmap-height (second mipmap-extent))
                             ;; TODO: We assume the :from is going to
                             ;; be a 2D image. This doesn't have to be
                             ;; the case (like reading from a buffer
                             ;; instead of an image) and we will
                             ;; support it later when we actually do
                             ;; that.
                             (mipmap-2d
                               (make-mipmap-2d
                                :sourced-p t
                                :extent (make-span-2d
                                         :origin (iv2:vec 0 0)
                                         :extent (iv2:vec mipmap-width
                                                          mipmap-height))
                                :mapping-spans
                                (make-mapping-spans
                                 :encode
                                 (make-mapping-span-2d
                                  ;; We select a 2-d subspace of an N
                                  ;; pixel wide, M pixel high, and 1
                                  ;; pixel deep 3D span from the (2d)
                                  ;; image...
                                  :from dspan-3d
                                  ;; ...and place it here in the 1d
                                  ;; extent of this mipmap.
                                  :to (make-data-span-2d
                                       :origin (iv2:vec 0 0)
                                       :extent (iv2:vec mipmap-width
                                                        mipmap-height)))))))
                        (list computed-extent mipmap-2d)))
                     (t
                      ;; We simply record that we don't have a known
                      ;; mipmap description for this extent.
                      (list computed-extent :dne)))))
               computed-extents
               descending-mipmap-extent-keys)))
        ;; Finally we return the answer.
        (list (cons (texmap:name texmap-inst) mipmap-obs))))))


(defmethod deduce-mipmap-hierarchy ((texmap-inst texture-map-3d)
                                    (style (eql :unique)) store store-args
                                    &key core)
  (declare (ignore core))
  (check-deducible-mipmap-hierarchy texmap-inst)

  nil)
(defmethod deduce-mipmap-hierarchy ((texmap-inst texture-map-3d)
                                    (style (eql :combined)) store store-args
                                    &key core)
  (declare (ignore core))
  (check-deducible-mipmap-hierarchy texmap-inst)

  nil)

(defmethod deduce-mipmap-hierarchy ((texmap-inst texture-map-cube)
                                    (style (eql :faces)) store store-args
                                    &key core)
  (declare (ignore core))
  (check-deducible-mipmap-hierarchy texmap-inst)

  nil)

(defmethod deduce-mipmap-hierarchy ((texmap-inst texture-map-cube)
                                    (style (eql :envmap)) store store-args
                                    &key core)
  (declare (ignore core))
  (check-deducible-mipmap-hierarchy texmap-inst)

  nil)
