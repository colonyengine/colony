(in-package :defpackage+-user-1)

(defpackage+ #:first-light.image-types
  (:nicknames #:fl.image)
  (:local-nicknames (#:u #:fl.util))
  (:use #:cl)
  (:export #:channels
           #:data
           #:free-storage
           #:get-pixel-size
           #:height
           #:internal-format
           #:origin
           #:pixel-format
           #:pixel-type
           #:read-image
           #:width))
