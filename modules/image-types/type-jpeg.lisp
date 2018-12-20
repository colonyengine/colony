(in-package :first-light.image-types)

(defmethod read-image-type ((type (eql :jpeg)) path)
  (error "Not implemented"))
