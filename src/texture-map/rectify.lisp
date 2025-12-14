(in-package #:colony.texture-map)

;; TODO: Candidate for a generalized method appropriate for use in many
;; places and by the appdev. In what package would it be?
;;
;; TODO: Move to a better spot so it gets loaded into the image before
;; any uses of it show up.
(defgeneric rectify (infer-style feature inst root
                     &key core force &allow-other-keys)
  (:documentation "Rectify the INST so any required values inside of it are
concretized. INFER-STYLE can be :synthesize, :validate, or :infer and
this dictates what the RECTIFY method is going to do. FEATURE is
(usually) a symbol that indicates what particular feature of the INST
will be rectified. ROOT is usually the root object in the hierarchy of a
nested set of objects used when rectification information needs to span
more information that is just available in the INST. CORE is the usual
engine core if needed. Return two values: The first value is T or NIL
depending on if the required rectification was successful. The second
value is ignored f the first value is T, or it is a keyword indicating
a reason for the failure of the rectification."))


;; -------------------------------------
;; Texture Map Rectification Entry Point
;; -------------------------------------

(defmethod rectify (infer-style
                    (feature (eql :texture-map))
                    (inst texture-map)
                    (root texture-map)
                    &key core force)
  ;; todo: :infer requires prolog-like backtracking, we leave
  ;; unimplemented for now.
  (when (eq infer-style :infer)
    (error "Not implemented! Unable to rectify an :infer instance: ~A"
           inst))
  (let ((texmap-state (texmap:state inst)))
    ;; Allow the caller to force re-rectification. Otherwise we will
    ;; bail early because it had already been done. The keyword :force
    ;; is present for when we runtime mutate the texture map--which may
    ;; cause rectification to occur again.
    (when (and (texmap:rectified-p texmap-state) (null force))
      (return-from rectify t))
    (setf (texmap:rectified-p texmap-state) nil)
    ;; If we must :synthesize the data, we accomplish that first.
    (when (eq infer-style :synthesize)
      (unless (rectify :synthesize :texture-map-contents inst root :core core)
        (error "Rectification synthesis failed: ~A" inst))
      ;; Update the classification to the next one we need to do.
      (setf (texmap:rectification-classification texmap-state) :validate
            infer-style :validate))
    ;; Then, either after synthesis, or because we only need to validate,
    ;; process :validate.
    (when (eq infer-style :validate)
      (unless (rectify :validate :texture-map-contents inst root :core core)
        (error "Rectification validation failed: ~A" inst))
      (setf (texmap:rectification-classification texmap-state) :rectified
            (texmap:rectified-p texmap-state) t
            infer-style :rectified))
    ;; If something blew up or went awry, we signal a condition.
    (unless (eq infer-style :rectified)
      (error "Unknown infer-style ~(~S~) for this instance: ~A"
             infer-style inst))
    t))
