(in-package :cl-user)

(defpackage #:first-light.image-types
  (:nicknames #:fl.image)
  (:use #:cl)
  (:export #:data
           #:free-storage
           #:get-pixel-size
           #:height
           #:internal-format
           #:origin
           #:pixel-format
           #:pixel-type
           #:read-image
           #:width))
