(in-package :gear/example)

(defclass hit-points (component)
  ((%hp :accessor hp
        :initarg :hp
        :initform 0)))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass gun-manager (component)
  ((%active-gun :accessor active-gun
                :initarg :active-gun
                :initform NIL)
   (%guns :accessor guns
          :initarg :guns
          :initform (vector))))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass gun (component)
  ((%shots :accessor shots
           :initarg :shots
           :initform 0)
   (%shot-type :accessor shot-type
               :initarg :shot-type
               :initform NIL)))
