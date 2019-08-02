(in-package #:cl-user)

(defpackage #:first-light.actions
  (:nicknames #:fl.actions)
  (:local-nicknames (#:a #:alexandria)
                    (#:u #:golden-utils)
                    (#:log #:verbose)
                    (#:v3 #:origin.vec3)
                    (#:q #:origin.quat))
  (:use #:cl #:%first-light)
  ;; protocol
  (:export
   #:insert-action
   #:make-action-manager
   #:process-actions
   #:remove-action)
  ;; built-in actions
  (:export
   #:fade-in
   #:fade-out
   #:rotate
   #:rotate/reverse
   #:sprite-animate))
