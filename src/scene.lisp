(in-package :gear)

(defvar *scene-table*)

(defun %read-spec-forms (file)
  (let ((*package* (find-package :gear)))
    (with-open-file (in file)
      (loop :for form = (read in nil in)
            :until (eq form in)
            :collect form))))

(defun %type-check-actor (actor actors-list)
  (unless (char= (char (symbol-name actor) 0) #\@)
    (error (format nil "Actor names must begin with '@': ~a" actor)))
  (when (find actor actors-list)
    (error (format nil "Actors cannot be duplicated: ~a" actor))))

(defun %generate-actor-names (scene-spec)
  (let ((result))
    (labels ((%generate (tree)
               (loop :for (actor nil . sub-tree) :in tree
                     :do (%type-check-actor actor result)
                         (push actor result)
                         (%generate sub-tree))))
      (%generate scene-spec))
    (nreverse result)))

(defun %type-check-actor-component (actor-names component)
  (let ((component-actors
          (remove-if
           (lambda (x)
             (or (not (symbolp x))
                 (not (char= (char (symbol-name x) 0) #\@))))
           (flatten component))))
    (dolist (actor component-actors)
      (unless (find actor actor-names)
        (error (format nil "A component references the undefined actor: ~a"
                       actor))))))

(defun %generate-actor-components-table (scene-spec actor-names &optional table)
  (loop :with table = (or table (make-hash-table :test #'eq))
        :for (actor components . child) :in scene-spec
        :do (dolist (component (reverse components))
              (%type-check-actor-component actor-names component)
              (push component (gethash actor table)))
            (%generate-actor-components-table child actor-names table)
        :finally (return table)))

(defun %generate-thunk-list-symbols (actor-names)
  (mapcar
   (lambda (x)
     (symbolicate x "-COMPONENT-THUNKS" (gensym)))
   actor-names))

(defun %generate-actor-bindings (actor-names thunk-list-names table)
  (mapcan
   (lambda (actor thunk)
     `((,actor (gethash ',actor ,table))
       (,thunk)))
   actor-names
   thunk-list-names))

(defun %generate-component-initializers (actor-components)
  (flet ((generate-component-forms (components)
           (let ((component-forms))
             (dolist (c components)
               (push `(make-component ',(first c)) component-forms))
             component-forms)))
    (let ((result))
      (maphash
       (lambda (actor components)
         (push `(add-multiple-components
                 ,actor
                 (list ,@(generate-component-forms components)))
               result))
       actor-components)
      result)))

(defun %generate-component-thunks (actor-names thunk-names component-table)
  (loop :for actor :in actor-names
        :for thunk :in thunk-names
        :for components = (gethash actor component-table)
        :append
        (loop :for (component . initargs) :in components
              :collect `(push
                         (list ',component
                               (lambda ()
                                 (let ((c (get-component ',component ,actor)))
				   ;; We need the reference to the component
				   ;; after we initialize it from the scene
				   ;; dsl.
                                   (reinitialize-instance
                                    c :actor ,actor ,@initargs)
                                   c)))
                         ,thunk))))

(defun %generate-relationships (scene-spec)
  (labels ((traverse (tree &optional parent)
             (destructuring-bind (child components . sub-tree) tree
               (declare (ignore components))
               (let ((result (list (cons (or parent '@universe) child))))
                 (if sub-tree
                     (append
                      result
                      (apply #'append
                             (mapcar (lambda (x) (traverse x child)) sub-tree)))
                     result)))))
    (loop :with children = (apply #'append (mapcar #'traverse scene-spec))
          :for (parent . child) :in children
          :collect `(add-child
                     (get-component 'transform ,parent)
                     (get-component 'transform ,child)))))

(defun %generate-actor-spawn (core-state actor-names thunk-names)
  (loop :for actor :in actor-names
        :for thunk :in thunk-names
        :collect `(spawn-actor ,core-state ,actor ,thunk)))

(defun parse-scene (scene-name scene-spec)
  (with-gensyms (core-state actor-table actor-name)
    (let* ((scene-spec `((@universe ((transform)) ,@scene-spec)))
           (actor-names (%generate-actor-names scene-spec))
           (actor-components (%generate-actor-components-table
                              scene-spec actor-names))
           (thunk-list-symbols (%generate-thunk-list-symbols actor-names))
           (bindings (%generate-actor-bindings
                      actor-names thunk-list-symbols actor-table)))
      `(lambda (,core-state)
         (let ((,actor-table (make-hash-table :test #'eq)))
           (dolist (,actor-name ',actor-names)
             (setf (gethash ,actor-name ,actor-table)
                   (make-instance 'gear:actor
                                  :id ,actor-name
                                  :scene ,scene-name)))
           (let ,bindings
             ,@(%generate-component-initializers actor-components)
             ,@(%generate-component-thunks
                actor-names thunk-list-symbols actor-components)
             ,@(%generate-relationships scene-spec)
             ,@(%generate-actor-spawn
                core-state actor-names thunk-list-symbols)
             (add-scene-tree-root ,core-state @universe)
             (values ,core-state ,actor-table)))))))

(defun get-scene (core-state scene-name)
  (gethash scene-name (scene-table core-state)))

(defun load-scene (core-state scene-name)
  (funcall (get-scene core-state scene-name) core-state))

(defmethod extension-file-types ((owner (eql 'scene)))
  (list "lisp" "scene"))

(defun prepare-scenes (core-state path)
  (let ((*scene-table* (make-hash-table :test #'eq)))
    (flet ((%prepare ()
             (load-extensions 'scene path)
             *scene-table*))
      (merge-scene-table core-state (%prepare))
      core-state)))

(defmacro scene-definition (name (&key enabled) &body body)
  (when enabled
    `(setf (gethash ,name *scene-table*)
           ,(apply #'parse-scene name body))))

;;; debug stuff below

(defmacro scene-definition-code (name (&key enabled) &body body)
  (when enabled
    `(setf (gethash ,name *scene-table*)
           (apply #'parse-scene ,name ',body))))

(defun test-scene-load ()
  (let ((cs (make-core-state)))
    (prepare-scenes cs (get-path :gear-example "data"))
    (load-scene cs :demo)
    cs))

(defun test-one-frame ()
  (let ((cs (test-scene-load)))
    (prepare-call-flows cs (get-path :gear-example "data"))
    (execute-flow cs :default 'perform-one-frame 'ENTRY
                  :come-from-state-name (gensym "EXEC-FLOW-"))
    cs))

(defun test-scene-code ()
  (let ((cs (make-core-state)))
    (prepare-scenes cs (get-path :gear-example "data"))
    (get-scene cs :demo-code)))
