(in-package :defpackage+-user-1)

(defpackage #:first-light.actions
  (:nicknames #:fl.actions)
  (:local-nicknames (#:m #:game-math))
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
