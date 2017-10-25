(in-package :gear)

(defvar *scene-table*)

(defun %read-spec-forms (file)
  (let ((*package* (find-package :gear)))
    (with-open-file (in file)
      (loop :for form = (read in nil in)
            :until (eq form in)
            :collect form))))

(defun %generate-actor-names (scene-spec)
  (remove-duplicates
   (remove-if
    (lambda (x) (or (not (symbolp x))
                    (not (eq (char (symbol-name x) 0) #\@))))
    (flatten scene-spec))))

(defun %generate-actor-components-table (scene-spec &optional table)
  (loop :with table = (or table (make-hash-table))
        :for (actor components . child) :in scene-spec
        :do (dolist (component (reverse components))
              (push component (gethash actor table)))
            (%generate-actor-components-table child table)
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
        :append (loop :for (component . initargs) :in components
                      :collect `(push
                                 (lambda ()
                                   (reinitialize-instance
                                    (get-component ',component ,actor)
                                    :actor ,actor
                                    ,@initargs))
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

(defun %generate-actor-realization (core-state actor-names thunk-names)
  (loop :for actor :in actor-names
        :for thunk :in thunk-names
        :collect `(spawn-actor ,core-state ,actor ,thunk)))

(defun parse-scene (scene-spec)
  (with-gensyms (core-state actor-table actor-name)
    (let* ((scene-spec `((@universe ((transform)) ,@scene-spec)))
           (actor-names (%generate-actor-names scene-spec))
           (actor-components (%generate-actor-components-table scene-spec))
           (thunk-list-symbols (%generate-thunk-list-symbols actor-names))
           (bindings (%generate-actor-bindings
                      actor-names thunk-list-symbols actor-table)))
      `(progn
         (lambda (,core-state)
           (let ((,actor-table (make-hash-table)))
             (dolist (,actor-name ',actor-names)
               (setf (gethash ,actor-name ,actor-table)
                     (make-instance 'gear:actor :id ,actor-name)))
             (let ,bindings
               ,@(%generate-component-initializers actor-components)
               ,@(%generate-component-thunks
                  actor-names thunk-list-symbols actor-components)
               ,@(%generate-relationships scene-spec)
               ,@(%generate-actor-realization
                  core-state actor-names thunk-list-symbols)
               (add-scene-tree-root ,core-state @universe)
               (values ,core-state ,actor-table))))))))

(defun get-scene (name)
  (gethash name *scene-table*))

(defmacro scene-definition (name &body body)
  `(setf (gethash ,name *scene-table*)
         ,(apply #'parse-scene body)))

(defun prepare-scenes (core-state path)
  (let ((*scene-table* (make-hash-table)))
    (declare (special *scene-table*))
    (flet ((%prepare ()
             (load-extensions (get-path :gear "extensions"))
             (load-extensions path)
             *scene-table*))
      (add-prepared-scenes core-state (%prepare))
      core-state)))

(defun add-prepared-scenes (cs ht) cs)
