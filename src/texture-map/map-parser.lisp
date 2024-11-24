(in-package #:colony.texture-map)

(defgeneric parse-texture-map (name model style store body))
(defgeneric parse-texture-simple (name model style store body))
(defgeneric parse-texture-complex (name model style store body))

;; ------------------------------------------------------------------
;; Utilities for the parsing.
;; ------------------------------------------------------------------

(defun physicalp (body)
  "Return T if there is a C:DATA-ELEMENTS form in the texture-map BODY,
otherwise return NIL."
  (some (lambda (form) (and (consp form) (eql (car form) 'data-elements)))
        body))

;; ---------------------------------------------------------------------------
;; :1d, :2d, :3d  processing is the default.
;; ---------------------------------------------------------------------------

(defmethod parse-texture-map (name model style store body)
  "Return two values. The first value is the API lambda form to construct
the in memory texture-map objects. The second form is if the texture was
anonymous or not."
  (parse-texture-map-simple name model style store body))

(defmethod parse-texture-map-simple (name model style store body)
  (physical->api
   name model style store
   (if (physicalp body)
       body
       (logical->physical name model style store body))))

;; ------------------------------------------------------------------
;; :cube processing
;; ------------------------------------------------------------------

;; Handle :cube
(defmethod parse-texture-map (name (model (eql :cube)) style store body)
  (parse-texture-map-complex name model style store body))

;; Currently only :cube
(defmethod parse-texture-map-complex (name model style store body)
  (physical->api
   name model style store
   (if (physicalp body)
       body
       (logical->physical name model style store body))))
