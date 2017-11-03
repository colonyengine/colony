(in-package :gear)

(defclass context ()
  ((%display :accessor display
             :initarg :display)
   (%title :accessor title
           :initarg :title
           :initform "Untitled Game")
   (%width :accessor width
           :initarg :width
           :initform 800)
   (%height :accessor height
            :initarg :height
            :initform 450)
   (%delta :accessor delta
           :initarg :delta
           :initform 1/30)
   (%vsyncp :accessor vsyncp
            :initarg :vsyncp
            :initform t)
   (%periodic-interval :accessor periodic-interval
                       :initarg :periodic-interval
                       :initform nil)
   (%debug-interval :accessor debug-interval
                    :initarg :debug-interval
                    :initform 5)
   (%gl-version-major :accessor gl-version-major
                      :initarg :gl-version-major
                      :initform 3)
   (%gl-version-minor :accessor gl-version-minor
                      :initarg :gl-version-minor
                      :initform 3)
   (%anti-alias-level :accessor anti-alias-level
                      :initarg :anti-alias-level
                      :initform 4)
   (%gl-capabilities :accessor gl-capabilities
                     :initarg :gl-capabilities
                     :initform '(:depth-test :blend :multisample))
   (%gl-blend-mode :accessor gl-blend-mode
                   :initarg :gl-blend-mode
                   :initform '(:src-alpha :one-minus-src-alpha))
   (%gl-depth-mode :accessor gl-depth-mode
                   :initarg :gl-depth-mode
                   :initform :lequal)))

(defun make-context (&rest initargs)
  (apply #'make-instance 'context initargs))
