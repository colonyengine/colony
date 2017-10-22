(in-package :gear)

(defclass actor ()
  ((%id :accessor id
        :initarg :id)
   ;; Below: one of :initialize, :active, :destroy
   (%state :accessor state
           :initarg :state)
   ;; Keyed by (reference to) instance, value is actual instance. This is the
   ;; conceptual storage location for components owned by this actor.
   (%components :accessor components
                :initarg :components
                :initform (make-hash-table))
   ;; A view into the components instances. Keyed by (reference to) instance,
   ;; value is list of (reference to) instances (that are located in the
   ;; components slot of this object). It is ok if the value is a list since
   ;; it'll generally be the case noone searches in this list for anything on a
   ;; common basis.
   (%components-by-type :accessor components-by-type
                        :initarg :components-by-type
                        :initform (make-hash-table))))

(defmethod print-object ((object actor) stream)
  (print-unreadable-object (object stream :type t)
    (format stream "~a" (id object))))
