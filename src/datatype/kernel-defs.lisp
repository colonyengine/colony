(in-package #:colony)

;;; The root type for actors and components.

(defclass kernel ()
  ((%core :reader core)
   (%context :reader context
             :initarg :context)
   (%state :accessor state
           :initarg :state
           :initform :initialize)
   (%id :reader id
        :initarg :id
        :initform nil)
   (%uuid :reader uuid
          ;; TODO: Possibly move this initform to the call to make-actor /
          ;; make-component to preserve layering semantics.
          :initform (uuid:make-uuid))
   (%display-id :accessor display-id
                :initarg :display-id
                :initform "Un-named kernel")
   (%ttl :accessor ttl
         :initarg :ttl
         :initform 0)))
