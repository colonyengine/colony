(in-package :gear/example)

(defclass hit-points (component)
  ((%hp :accessor hp
        :initarg :hp
        :initform 0)))

(defun make-hit-points (&rest args)
  (apply #'make-instance 'hit-points args))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass gun-manager (component)
  ((%active-gun :accessor active-gun
                :initarg :active-gun
                :initform NIL)
   (%guns :accessor guns
          :initarg :guns
          :initform (vector))))

(defun make-gun-manager (&rest args)
  (apply #'make-instance 'gun-manager args))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass gun (component)
  ((%shots :accessor shots
           :initarg :shots
           :initform 0)
   (%shot-type :accessor shot-type
               :initarg :shot-type
               :initform NIL)))

(defun make-gun (&rest args)
  (apply #'make-instance 'gun args))
