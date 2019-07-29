(in-package #:%first-light)

(defclass dynamic-geometry-layout ()
  ((%name :reader name
          :initarg :name)
   (%groups :reader groups
            :initarg :groups
            :initform nil)
   (%group-order :reader group-order
                 :initarg :group-order)))

(defclass dynamic-geometry ()
  ((%id :reader id
        :initform (gl:gen-vertex-array))
   (%layout :reader layout
            :initarg :layout)
   (%buffers :reader buffers)
   (%buffer-names :reader buffer-names
                  :initform (u:dict))
   (%primitive :reader primitive
               :initarg :primitive)
   (%vertex-count :reader vertex-count
                  :initarg :vertex-count)))

(defun find-geometry-layout (layout-name)
  (or (u:href (meta 'dynamic-geometry-layouts) layout-name)
      (error "Geometry layout ~s not found." layout-name)))

(defun make-dynamic-geometry-thunk (layout-name
                                    &key (primitive :triangles) (vertex-count 0)
                                      buffer-data)
  (lambda ()
    (let ((geometry (make-instance 'dynamic-geometry
                                   :layout (find-geometry-layout layout-name)
                                   :primitive primitive
                                   :vertex-count vertex-count)))
      (gl:bind-vertex-array (id geometry))
      (make-geometry-buffers geometry)
      (configure-geometry-buffers geometry)
      (apply #'update-dynamic-geometry
             geometry primitive vertex-count buffer-data)
      geometry)))

(defun make-dynamic-geometry (name)
  (let ((geometry (u:href (meta 'dynamic-geometry) name)))
    (funcall geometry)))

(defun update-dynamic-geometry (geometry primitive vertex-count &rest data)
  (with-slots (%primitive %vertex-count) geometry
    (u:do-plist (k v data)
      (fill-geometry-buffer geometry k v))
    (setf %primitive primitive
          %vertex-count vertex-count)
    (u:noop)))

(defun draw-dynamic-geometry (geometry instance-count)
  (with-slots (%primitive %vertex-count) geometry
    (%gl:draw-arrays-instanced %primitive 0 %vertex-count instance-count)))

(defmacro define-geometry-layout (name &body body)
  (a:with-gensyms (groups order)
    (let ((layouts '(meta 'dynamic-geometry-layouts)))
      `(u:mvlet ((,groups ,order (make-geometry-groups ',body)))
         (unless ,layouts
           (setf (meta 'dynamic-geometry-layouts) (u:dict)))
         (setf (u:href ,layouts ',name)
               (make-instance 'dynamic-geometry-layout
                              :name ',name
                              :groups ,groups
                              :group-order ,order))))))

(defmacro define-geometry (name &body body)
  (let ((geometry-table '(meta 'dynamic-geometry)))
    (destructuring-bind (&key layout (primitive :triangles) (vertex-count 0)
                           buffers)
        body
      `(progn
         (unless ,geometry-table
           (setf (meta 'dynamic-geometry) (u:dict)))
         (setf (u:href ,geometry-table ',name)
               (make-dynamic-geometry-thunk
                ',layout
                :primitive ',primitive
                :vertex-count ,vertex-count
                :buffer-data ',buffers))))))
