(in-package :defpackage+-user-1)

(defpackage+ #:first-light.util
  (:nicknames #:fl.util)
  (:use #:cl)
  (:inherit-from #:alexandria
                 #:appendf
                 #:clamp
                 #:define-constant
                 #:deletef
                 #:ensure-list
                 #:ensure-symbol
                 #:format-symbol
                 #:if-let
                 #:lerp
                 #:make-keyword
                 #:map-product
                 #:once-only
                 #:symbolicate
                 #:when-let
                 #:when-let*
                 #:with-unique-names)
  (:inherit-from #:serapeum
                 #:collecting
                 #:dict
                 #:eval-always
                 #:href
                 #:mvlet
                 #:mvlet*
                 #:octet
                 #:op
                 #:split-sequence
                 #:unique-name)
  (:export #:alist-get
           #:copy-directory
           #:define-printer
           #:defun-inline
           #:degrees->radians
           #:do-hash
           #:do-hash-keys
           #:do-hash-values
           #:flatten
           #:get-directory-contents
           #:fn->
           #:hash->alist
           #:hash-keys
           #:hash-values
           #:if-found
           #:map-domain
           #:map-files
           #:maphash-keys
           #:maphash-values
           #:noop
           #:plist-p
           #:plist-values
           #:radians->degrees
           #:resolve-system-path
           #:safe-read-file-form
           #:safe-read-file-forms
           #:string-starts-with-p
           #:when-found
           #:while
           #:with-binary-input
           #:with-binary-output
           #:with-file-input
           #:with-file-output))
