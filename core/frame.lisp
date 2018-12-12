(in-package :%fl)

(defclass frame-manager ()
  ((%start :reader start
           :initform (local-time:now))
   (%now :initform (local-time:now))
   (%before :initform 0)
   (%total-time :initform 0)
   (%delta :initarg :delta
           :initform (/ 30f0))
   (%delta-buffer :initform 0)
   (%frame-time :initform 0)
   (%accumulator :initform 0)
   (%alpha :reader alpha
           :initform 0f0)
   (%vsync-p :reader vsync-p
             :initarg :vsync-p)
   (%period-elapsed :initform (local-time:now))
   (%period-interval :reader period-interval
                     :initarg :period
                     :initform nil)
   (%debug-interval :reader debug-interval
                    :initarg :debug-interval
                    :initform 5)
   (%debug-time :initform 0)
   (%debug-count :initform 0)))

(defmethod initialize-instance :after ((object frame-manager) &key)
  (with-slots (%delta) object
    (reinitialize-instance object :delta (float %delta 1f0))))

(defun make-frame-manager (core-state)
  (let ((context (context core-state)))
    (setf (frame-manager core-state)
          (make-instance 'frame-manager
                         :vsync-p (when (eq (cfg context :vsync) :on) t)
                         :delta (cfg context :delta)
                         :period (cfg context :periodic-interval)
                         :debug-interval (cfg context :debug-interval)))))

(defun smooth-delta-time (frame-manager refresh-rate)
  (with-slots (%delta-buffer %frame-time) frame-manager
    (incf %frame-time %delta-buffer)
    (let* ((frame-count (truncate (1+ (* %frame-time refresh-rate))))
           (previous %frame-time))
      (setf frame-count (if (plusp frame-count) frame-count 1)
            %frame-time (/ frame-count refresh-rate)
            %delta-buffer (- previous %frame-time)))))

(defun calculate-frame-rate (frame-manager)
  (with-slots (%debug-time %debug-count) frame-manager
    (let* ((debug-interval (debug-interval frame-manager))
           (now (get-internal-real-time))
           (elapsed-seconds (/ (- now %debug-time) internal-time-units-per-second))
           (fps (/ %debug-count debug-interval)))
      (when (and (>= elapsed-seconds debug-interval)
                 (plusp fps))
        (v:debug :fl.core.engine "Frame rate: ~,2f fps (~,3f ms/f)" fps (/ 1000 fps))
        (setf %debug-count 0
              %debug-time now))
      (incf %debug-count))))

(defun frame-update (frame-manager func)
  (with-slots (%alpha %delta %accumulator %frame-time) frame-manager
    (incf %accumulator %frame-time)
    (au:while (>= %accumulator %delta)
      (funcall func)
      (decf %accumulator %delta))
    (setf %alpha (/ %accumulator %delta))))

(defun frame-periodic-update (frame-manager func)
  (with-slots (%period-elapsed %period-interval) frame-manager
    (let ((now (local-time:now))
          (interval %period-interval))
      (when (and interval
                 (>= (local-time:timestamp-difference now %period-elapsed) interval))
        (funcall func)
        (v:trace :fl.core.engine "Periodic update performed (every ~d seconds)" interval)
        (setf %period-elapsed now)))))

(defun tick (frame-manager refresh-rate step-func &key periodic-func)
  (with-slots (%start %now %before %total-time %frame-time %vsync-p) frame-manager
    (setf %before %now
          %now (local-time:now)
          %frame-time (float (local-time:timestamp-difference %now %before) 1f0)
          %total-time (float (local-time:timestamp-difference %now %start) 1f0))
    (when %vsync-p
      (smooth-delta-time frame-manager refresh-rate))
    (frame-update frame-manager step-func)
    (when periodic-func
      (frame-periodic-update frame-manager periodic-func))
    (calculate-frame-rate frame-manager)
    (values)))
