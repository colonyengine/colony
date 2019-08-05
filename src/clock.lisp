(in-package #:virality.engine)

(defclass clock ()
  ((%start :reader start
           :initform (get-time))
   (%now :initform (get-time))
   (%pause-time :reader pause-time
                :initform 0)
   (%before :initform 0)
   (%total-time :initform 0)
   (%delta :initarg :delta
           :initform (/ 30f0))
   (%delta-buffer :initform 0)
   (%frame-time :initform 0)
   (%frame-count :initform 0)
   (%accumulator :initform 0)
   (%alpha :reader alpha
           :initform 0f0)
   (%vsync-p :reader vsync-p
             :initarg :vsync-p)
   (%period-elapsed :initform (get-time))
   (%period-interval :reader period-interval
                     :initarg :period
                     :initform nil)
   (%debug-interval :reader debug-interval
                    :initarg :debug-interval
                    :initform 5)
   (%debug-time :initform 0)
   (%debug-count :initform 0)))

(defmethod initialize-instance :after ((instance clock) &key)
  (with-slots (%delta) instance
    (reinitialize-instance instance :delta (float %delta 1f0))))

(defun make-clock (core)
  (let ((context (context core)))
    (setf (slot-value core '%clock)
          (make-instance 'clock
                         :vsync-p (when (eq (option context :vsync) :on) t)
                         :delta (option context :delta)
                         :period (option context :periodic-interval)
                         :debug-interval (option context :debug-interval)))))

(defun get-time ()
  #+sbcl
  (u:mvlet ((s ms (sb-ext:get-time-of-day)))
    (+ (- s (load-time-value (sb-ext:get-time-of-day)))
       (/ ms 1d6)))
  #-sbcl
  (float (/ (get-internal-real-time) internal-time-units-per-second) 1d0))

(defun smooth-delta-time (clock refresh-rate)
  (with-slots (%delta-buffer %frame-time) clock
    (incf %frame-time %delta-buffer)
    (let ((frame-count (max 1 (truncate (1+ (* %frame-time refresh-rate)))))
          (previous %frame-time))
      (setf %frame-time (/ frame-count refresh-rate)
            %delta-buffer (- previous %frame-time)))))

(defun calculate-frame-rate (clock)
  (with-slots (%debug-time %debug-interval %debug-count) clock
    (let* ((now (get-internal-real-time))
           (elapsed-seconds (/ (- now %debug-time)
                               internal-time-units-per-second))
           (fps (/ %debug-count %debug-interval)))
      (when (and (>= elapsed-seconds %debug-interval)
                 (plusp fps))
        (log:debug :virality.engine "Frame rate: ~,2f fps (~,3f ms/f)"
                   fps (/ 1000 fps))
        (setf %debug-count 0
              %debug-time now))
      (incf %debug-count))))

(defun initialize-frame-time (core)
  (with-slots (%start %now) (clock core)
    (let ((time (get-time)))
      (setf %start time
            %now %start))))

(defun clock-update (core)
  (with-slots (%alpha %delta %accumulator %frame-time) (clock core)
    (incf %accumulator %frame-time)
    (u:while (>= %accumulator %delta)
      (execute-flow core
                    :default
                    'active-phase
                    'protocol-physics-update
                    :come-from-state-name
                    :ef-physics-update)
      (comp.transform::map-nodes
       (lambda (x) (comp.transform::transform-node core x))
       (component-by-type (scene-tree core) 'comp.transform:transform))
      (execute-flow core
                    :default
                    'active-phase
                    'physics-collisions
                    :come-from-state-name
                    :ef-physics-collisions)
      (decf %accumulator %delta))
    (setf %alpha (/ %accumulator %delta))))

(defun clock-periodic-update (core)
  (with-slots (%now %period-elapsed %period-interval) (clock core)
    (let ((interval %period-interval))
      (when (and interval
                 (>= (- %now %period-elapsed) interval))
        (live-coding-update)
        (log:trace :virality.engine
                   "Periodic update performed (every ~d seconds)"
                   interval)
        (setf %period-elapsed %now)))))

(defun clock-tick (core)
  (let ((clock (clock core))
        (refresh-rate (refresh-rate (display core))))
    (with-slots (%start %now %before %total-time %frame-time %pause-time
                 %vsync-p)
        clock
      (setf %before (+ %now %pause-time)
            %now (- (get-time) %pause-time)
            %frame-time (float (- %now %before) 1f0)
            %total-time (float (- %now %start) 1f0)
            %pause-time 0)
      (when %vsync-p
        (smooth-delta-time clock refresh-rate))
      (clock-update core)
      (clock-periodic-update core)
      (calculate-frame-rate clock)
      (u:noop))))
