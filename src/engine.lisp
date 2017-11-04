(in-package :gear)

(defmacro prepare-engine (core-state path)
  `(progn
     ,@(loop :with items = '(context call-flows scene)
             :for item :in items
             :for var = (alexandria:symbolicate '*temp- item '*)
             :collect `(let ((,var (make-hash-table :test #'eq)))
                         (declare (special ,var))
                         (flet ((%prepare ()
                                  (load-extensions ',item ,path)
                                  ,var))
                           (maphash
                            (lambda (k v)
                              (setf (gethash k (,(symbolicate item '-table)
                                                ,core-state))
                                    v))
                            (%prepare)))))))

(defmethod start-engine :around (system-name
                                 &optional (core-state (make-core-state)))
  (let* ((context (context-table core-state))
         (path (get-path system-name "data")))
    (prepare-engine core-state path)
    (if-let ((default-scene (cfg context :default-scene)))
      (progn
        (load-scene core-state default-scene)
        (call-next-method system-name core-state)
        core-state)
      (error "No default scene specified in settings.cfg."))))

(defmethod start-engine (system-name &optional core-state)
  (kit.sdl2:init)
  (sdl2:in-main-thread ()
    (make-display core-state))
  (kit.sdl2:start))
