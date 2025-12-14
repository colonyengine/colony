(in-package #:colony.texture-map)

;; ------------------------
;; Infer Methods
;; ------------------------

(defmethod rectify ((infer-style (eql :infer))
                    (feature (eql :texture-map-contents))
                    (inst texture-map-simple)
                    (root texture-map-simple)
                    &key core)
  (declare (ignore infer-style feature inst root core))
  (error "rectify(texture-map-simple, infer): ~(~S~) ~(~S~): Implement me!~%"
         (texmap:name inst) infer-style)
  nil)

(defmethod rectify ((infer-style (eql :infer))
                    (feature (eql :texture-map-contents))
                    (inst texture-map-complex)
                    (root texture-map-complex)
                    &key core)
  (declare (ignore feature root core))
  (error "rectify(texture-map-complex, infer): ~(~S~) ~(~S~): Implement me!~%"
         (texmap:name inst) infer-style)
  nil)
