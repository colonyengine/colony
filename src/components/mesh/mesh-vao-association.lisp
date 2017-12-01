(in-package :first-light)

(defclass mesh-vao-association-cache ()
  ((%cache :accessor cache
           :initarg :cache
           ;; Keyed by location (a string).
           ;; Value is a mesh-vao-association-cache-value instance.
           :initform (make-hash-table :test #'equalp))))

(defun make-mesh-vao-association-cache (&rest init-args)
  (apply #'make-instance 'mesh-vao-association-cache init-args))



(defclass mesh-vao-association-cache-value ()
  (;; location is here to make it simple to maphash over all entries if
   ;; we desire.
   (%location :accessor location
              :initarg :location)
   (%layout :accessor layout
            :initarg :layout)
   (%vao :accessor vao
         :initarg :vao)))

(defun make-mesh-vao-association-cache-value (location layout vao)
  (make-instance 'mesh-vao-association-cache-value
                 :location location
                 :layout layout
                 :vao vao))



(defun mesh-vao-association (mvac location)
  "Lookup LOCATION in the mesh-vao-association-cache MVAC and return
two values: 1) the cache entry [nil if not present], 2) if the cache value
was present."
  (gethash location (cache mvac)))

(defun (setf mesh-vao-association) (new-value mvac location)
  "SETF function for MESH-VAO-ASSOCIATION that allows the associating
of the mesh-vao-association-cache-value instance in NEW-VALUE with LOCATION
in the mesh-vao-association-cache MVAC. Return NEW-VALUE."
  (setf (gethash location (cache mvac)) new-value))
