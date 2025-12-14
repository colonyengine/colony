(in-package #:colony.texture-map)

;; ------------------------
;; Validate Methods
;; ------------------------

(defmethod rectify ((infer-style (eql :validate))
                    (feature (eql :mspans-cover-extent))
                    (inst mipmap)
                    (root texture-map-simple)
                    &key core)
  (declare (ignore infer-style feature inst root core))
  ;; TODO: Implement this check if the mapping-spans perfectly cover a
  ;; mipmap extent.
  ;;
  ;; 1) sum 3d vol of mapping spans, must equal to mipmap extent
  ;; 2) All mapping spans must not extend outside mipmap extent
  ;; 3) all combinations of mapping spans must not intersect
  ;; 4) ...then the mapping spans exactly cover the mipmap extent

  ;; Just a hack for testing to return T.
  t)

(defmethod rectify ((infer-style (eql :validate))
                    (feature (eql :pixel-format))
                    (inst vector)
                    (root texture-map-simple)
                    &key core)
  "Return two values: if all the image-data-elements in the INST use the
same pixel-format, then return T and :OK. Otherwise return NIL and a
keyword indicating reason for the failure."
  (declare (ignore infer-style feature core))
  (loop :with pixel-format = nil
        :for delem :across inst
        :for delem-pixel-format = (img:pixel-format
                                   (rc:value (texmap:element delem)))
        :do (if pixel-format
                (unless (eql delem-pixel-format pixel-format)
                  (return-from rectify (values nil :pixel-format-mismatch)))
                (setf pixel-format delem-pixel-format)))
  (values t :ok))

(defmethod rectify ((infer-style (eql :validate))
                    (feature (eql :texture-map-contents))
                    (inst texture-map-simple)
                    (root texture-map-simple)
                    &key core)
  (declare (ignore feature))

  (u:mvlet ((result error-domain
                    (rectify infer-style :pixel-format
                             (texmap:data-elements inst) root :core core)))
    (format
     t "rectify(texture-map-simple, validate): ~(~S~) ~(~S~): finish me!~%"
     (texmap:name inst) infer-style)

    (values result error-domain)))

(defmethod rectify ((infer-style (eql :validate))
                    (feature (eql :texture-map-contents))
                    (inst texture-map-complex)
                    (root texture-map-complex)
                    &key core)
  (declare (ignore feature root core))
  (error
   "rectify(texture-map-complex, validate): ~(~S~) ~(~S~): Implement me!~%"
   (texmap:name inst) infer-style)
  (values t :ok))
