(in-package :gear)

(defclass game-object ()
  ((%id :accessor id
        :initarg :id)
   ;; Below: one of :initialize, :active, :destroy
   (%state :accessor state
           :initarg :state)
   ;; Keyed by (reference to) instance, value is actual instance.
   ;; This is the conceptual storage location for components owned
   ;; by this game-object.
   (%components :accessor components
                :initarg :components
                :initform (make-hash-table))
   ;; A view into the components instances.
   ;; Keyed by (reference to) instance, value is list of (reference
   ;; to) instances (that are located in the components slot of this
   ;; object).
   (%components-by-type :accessor components-by-type
                        :initarg :components-by-type
                        :initform (make-hash-table))))

(defmethod print-object ((object game-object) stream)
  (print-unreadable-object (object stream :type t)
    (format stream "~a[~(~s~)]" (id object) (state object))))
