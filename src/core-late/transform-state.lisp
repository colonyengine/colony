(in-package #:virality.engine)

(defclass transform-state ()
  ((%previous :accessor previous
              :initarg :previous)
   (%current :accessor current
             :initarg :current)
   (%frame :reader frame
           :initarg :frame)
   (%incremental :accessor incremental
                 :initarg :incremental)
   (%incremental-delta :reader incremental-delta
                       :initarg :incremental-delta)
   (%interpolated :reader interpolated
                  :initarg :interpolated)))

(defun make-translate-state ()
  (make-instance 'transform-state
                 :previous (v3:vec)
                 :current (v3:vec)
                 :frame (v3:vec)
                 :incremental (v3:vec)
                 :incremental-delta (v3:vec)
                 :interpolated (v3:vec)))

(defun make-rotate-state ()
  (make-instance 'transform-state
                 :previous (q:quat 1)
                 :current (q:quat 1)
                 :frame (q:quat 1)
                 :incremental (v3:vec)
                 :incremental-delta (q:quat 1)
                 :interpolated (q:quat 1)))

(defun make-scale-state ()
  (make-instance 'transform-state
                 :previous (v3:vec 1)
                 :current (v3:vec 1)
                 :frame (v3:vec)
                 :incremental (v3:vec)
                 :incremental-delta (v3:vec)
                 :interpolated (v3:vec)))

(defun initialize-translation (state &optional initial velocity)
  (when initial
    (setf (current state) initial
          (previous state) (v3:copy (current state))))
  (when velocity
    (setf (incremental state) velocity)))

(defun initialize-rotation (state &optional initial velocity)
  (when initial
    (setf (current state) initial
          (previous state) (q:copy (current state))))
  (when velocity
    (setf (incremental state) velocity)))

(defun initialize-scale (state &optional initial velocity)
  (when initial
    (setf (current state) (etypecase initial
                            (v3:vec initial)
                            (real (v3:vec initial)))
          (previous state) (v3:copy (current state))))
  (when velocity
    (setf (incremental state) velocity)))

(defun interpolate-vector (state factor)
  (v3:lerp! (interpolated state) (previous state) (current state) factor))

(defun interpolate-quaternion (state factor)
  (q:slerp! (interpolated state) (previous state) (current state) factor))

(defun transform-node/vector (state delta frame-time)
  (v3:copy! (previous state) (current state))
  (v3:scale! (frame state) (frame state) frame-time)
  (v3:+! (current state) (current state) (frame state))
  (v3:scale! (incremental-delta state) (incremental state) delta)
  (v3:+! (current state) (current state) (incremental-delta state))
  (v3:zero! (frame state)))

(defun transform-node/quaternion (state delta frame-time)
  (q:copy! (previous state) (current state))
  (q:slerp! (frame state) q:+id+ (frame state) frame-time)
  (q:rotate! (current state) (current state) (frame state))
  (o:velocity->rotation! (incremental-delta state)
                         (incremental state)
                         delta)
  (q:rotate! (current state) (current state) (incremental-delta state))
  (q:id! (frame state)))
