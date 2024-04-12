(in-package #:cl-user)

(defpackage #:xXx-SYSTEM-NAME-xXx
  (:local-nicknames
   (#:u #:vutils))
  (:use #:cl)
  (:export
   #:start))

(uiop:define-package #:xXx-SYSTEM-NAME-xXx.shader
  (:use #:colony.shader)
  (:export
   ))

(colony.nicknames:define-nicknames
  (:xXx-SYSTEM-NAME-xXx :proj)
  (:xXx-SYSTEM-NAME-xXx.shader :proj/shd))
