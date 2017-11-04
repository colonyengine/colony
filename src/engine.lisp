(in-package :gear)

(defmacro specialp (symbol)
  (let ((f (gensym "FUNC-")))
    `(let ((,symbol 1))
       (let ((,f (lambda () ,symbol)))
         (let ((,symbol 2))
           (eql 2 (funcall ,f)))))))

(defmacro prepare-engine (core-state path)
  `(progn
     ,@(loop :with items = '(context call-flows scene)
             :for item :in items
             :for var = (alexandria:symbolicate '%temp- item)
             :collect `(let ((,var (make-hash-table :test #'eq)))
                         (declare (special ,var))
                         (macrolet ((f (var &environment env)
                                      `',(eq (sb-cltl2:variable-information var env) :special)))
                           (format t "~a is special? ~a~%" ',var (f ,var)))
                         (flet ((%prepare ()
                                  (load-extensions ',item ,path)
                                  ,var))
                           (maphash
                            (lambda (k v)
                              (setf (gethash k (,(symbolicate item '-table)
                                                ,core-state))
                                    v))
                            (%prepare)))))))

#++(defmethod start-engine :around (system-name
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

#++(defmethod start-engine (system-name &optional core-state)
  (kit.sdl2:init)
  (sdl2:in-main-thread ()
    (make-display core-state))
  (kit.sdl2:start))

(kit.sdl2:define-start-function start-engine2 ()
  (let ((*package* (find-package (package-name :gear))))
    (prepare-engine (make-core-state) (get-path :gear-example "data"))))

(defmacro test-specials-after-prepare ()
  `(loop :for var :in '(%temp-context %temp-call-flows %temp-scene)
         :do (macrolet ((f (var &environment env)
                          `',(eq (sb-cltl2:variable-information var env) :special)))
               (format t "~a is special? ~a~%" var (f var)))))
