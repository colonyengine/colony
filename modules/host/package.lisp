(in-package :cl-user)

(defpackage #:first-light.host
  (:nicknames #:fl.host)
  (:use #:cl)
  (:export #:check-texture-size
           #:get-gpu-vendor
           #:get-gpu-parameter))
