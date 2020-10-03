(in-package #:virality)

(defstruct (clock (:constructor %make-clock)
                  (:predicate nil)
                  (:copier nil))
  ;; Gaffer's "Fix Your Timestep" accumulator
  (accumulator 0d0 :type double-float)
  ;; Average frames per second (weighted average over last 10 seconds)
  (fps/average/10s 0d0 :type double-float)
  ;; Average frames per second (weighted average over last 30 seconds)
  (fps/average/30s 0d0 :type double-float)
  ;; Average frames per second (weighted average over last 60 seconds)
  (fps/average/60s 0d0 :type double-float)
  ;; Average frames per second over all frames since the game was started
  (fps/average 0d0 :type double-float)
  ;; Frames per second of the current frame
  (fps/current 0d0 :type double-float)
  ;; delta time smoothing variables
  (delta-buffer 0d0 :type double-float)
  ;; physics rate
  (delta-time (/ 30d0) :type double-float)
  ;; amount of time last frame took to render
  (frame-time 0d0 :type double-float)
  ;; number of frames since the game was started
  (frame-count 0 :type fixnum)
  ;; interpolation factor 0...1 from one physics step to the next
  (interpolation-factor 0d0 :type double-float)
  ;; amount of time spent in the last debugger invocation if any
  (pause-time 0d0 :type double-float)
  ;; the time in seconds the last periodic update has occurred
  (period-elapsed 0d0 :type double-float)
  ;; amount in seconds until the periodic update function is called, which
  ;; currently only includes unlocking the REPL
  (period-interval 0.25d0 :type double-float)
  ;; the time in seconds from the start of the game at the end of the previous
  ;; frame, excluding any time spent in any debugger invocation
  (previous-time 0d0 :type double-float)
  ;; the time in seconds from the start of the game at the end of the current
  ;; frame, excluding any time spent in any debugger invovation.
  ;; We present this as the total time of execution to the gamedev via the
  ;; total-time API call.
  (current-time 0d0 :type double-float)
  ;; a constant time recorded when the game first starts that every other time
  ;; variable is relative to
  (start-time 0 :type fixnum))

(defun pause-time (clock)
  (clock-pause-time clock))

(defun (setf pause-time) (value clock)
  (setf (clock-pause-time clock) value))

(defun make-clock (core)
  (let ((clock (%make-clock)))
    (setf (clock-start-time clock) (sb-ext:get-time-of-day)
          (clock-current-time clock) (get-time clock)
          (clock-delta-time clock) (float =delta= 1d0)
          (slot-value core '%clock) clock)))

(defun get-time (clock)
  #+sbcl
  (u:mvlet ((s ms (sb-ext:get-time-of-day)))
    (+ (- s (clock-start-time clock))
       (/ ms 1d6)))
  #-sbcl
  (float (/ (get-internal-real-time) internal-time-units-per-second) 1d0))

(defun smooth-delta-time (clock refresh-rate)
  (symbol-macrolet ((frame-time (clock-frame-time clock)))
    (incf frame-time (clock-delta-buffer clock))
    (let ((frame-count (max 1 (truncate (1+ (* frame-time refresh-rate)))))
          (previous frame-time))
      (setf frame-time (/ frame-count refresh-rate 1d0)
            (clock-delta-buffer clock) (- previous frame-time))
      nil)))

(defun calculate-frame-rate (clock)
  (let* ((time (clock-frame-time clock))
         (fps (/ 1d0 time))
         (alpha10 (- 1 (exp (- (/ time 10)))))
         (alpha30 (- 1 (exp (- (/ time 30)))))
         (alpha60 (- 1 (exp (- (/ time 60)))))
         (frame-count (clock-frame-count clock)))
    (symbol-macrolet ((average/10s (clock-fps/average/10s clock))
                      (average/30s (clock-fps/average/30s clock))
                      (average/60s (clock-fps/average/60s clock))
                      (average (clock-fps/average clock)))
      (setf (clock-fps/current clock) fps)
      (if (> (clock-current-time clock) 3)
          (setf average/10s (+ (* alpha10 fps) (* (- 1 alpha10) average/10s))
                average/30s (+ (* alpha30 fps) (* (- 1 alpha30) average/30s))
                average/60s (+ (* alpha60 fps) (* (- 1 alpha60) average/60s))
                average (/ (+ fps (* (1- frame-count) average))
                           frame-count
                           1d0))
          (setf average/10s fps
                average/30s fps
                average/60s fps
                average fps)))))

(defun clock-physics-update (core clock)
  (symbol-macrolet ((accumulator (clock-accumulator clock))
                    (delta (clock-delta-time clock)))
    (incf accumulator (clock-frame-time clock))
    (flet ((do-physics-update ()
             (execute-flow core
                           :default
                           'active-phase
                           'protocol-physics-update
                           :come-from-state-name
                           :ef-physics-update)
             (comp::process-deferred-instant-transform-updates core)
             (comp::map-nodes
              (lambda (x)
                (comp::transform-node x)
                (comp::reset-transform-replace-count x))
              (component-by-type (scene-tree core) 'comp:transform))
             (execute-flow core
                           :default
                           'active-phase
                           'physics-collisions
                           :come-from-state-name
                           :ef-physics-collisions)
             ;; TODO: Prevent an error in SBCL from producing a WARNING when
             ;; returning multiple values here. This needs more investigation.
             nil))

      ;; NOTE: On the _very first frame_, execute physics to give us something
      ;; to interpolate properly in the world BEFORE we accumulate to the first
      ;; delta amount of physics time.
      (when (zerop (clock-frame-count clock))
        (do-physics-update))

      ;; Then if enough time had passed, run physics.
      (u:while (>= accumulator delta)
        (do-physics-update)
        (decf accumulator delta))

      (setf (clock-interpolation-factor clock) (/ accumulator delta))
      nil)))

(defun clock-periodic-update (clock)
  (symbol-macrolet ((current (clock-current-time clock))
                    (elapsed (clock-period-elapsed clock)))
    (let ((period-interval (clock-period-interval clock)))
      (when (>= (- current elapsed) period-interval)
        (update-repl)
        #++(:printv "Periodic update performed (every ~d seconds)"
                    period-interval)
        (setf elapsed current)))
    nil))

(defun clock-tick (core)
  (let* ((display (display core))
         (clock (clock core))
         (pause (clock-pause-time clock))
         (previous (+ (clock-current-time clock) pause))
         (current (- (get-time clock) pause)))
    (setf (clock-previous-time clock) previous
          (clock-current-time clock) current
          (clock-frame-time clock) (- current previous))
    (when =vsync=
      (smooth-delta-time clock (refresh-rate display)))
    (clock-physics-update core clock)
    (clock-periodic-update clock)
    (when (plusp (clock-frame-count clock))
      (calculate-frame-rate clock))))
