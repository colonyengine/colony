(in-package :gear)

(defclass scene-definition ()
  ((%scene :accessor scene
           :initarg :scene)
   (%data :accessor data
          :initarg :data)))

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

(defun %generate-actor-bindings (actor-names table)
  (mapcan
   (lambda (actor)
     `((,actor (gethash ',actor ,table))))
   actor-names))

(defun %generate-component-symbol (component-name)
  (or (find-symbol (symbol-name component-name) :gear)
      component-name))

(defun %generate-component-initializers (actor-components)
  (flet ((generate-component-forms (components)
           (let ((component-forms))
             (dolist (c components)
               (let ((component (%generate-component-symbol (first c))))
                 (push `(make-component ',component) component-forms)))
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

(defun %generate-component-thunks (actor-names component-table)
  (loop :for actor :in actor-names
        :for components = (gethash actor component-table)
        :append
        (loop :for (component-name . initargs) :in components
              :for component = (%generate-component-symbol component-name)
              :for symbol = (gensym "COMPONENT-")
              :collect `(let ((,symbol (get-component ',component ,actor)))
                          (setf (initializer-thunk ,symbol)
                                (lambda ()
                                  (reinitialize-instance
                                   ,symbol :actor ,actor ,@initargs)))))))

(defun %generate-relationships (scene-spec)
  (labels ((traverse (tree &optional parent)
             (destructuring-bind (child components . sub-tree) tree
               (declare (ignore components))
               (let ((result (list (cons parent child))))
                 (if sub-tree
                     (append
                      result
                      (apply #'append
                             (mapcar (lambda (x) (traverse x child)) sub-tree)))
                     result)))))
    (loop :with children = (apply #'append (mapcar #'traverse scene-spec))
          :for (parent . child) :in children
          :when parent
            :collect `(add-child (get-component 'transform ,parent)
                                 (get-component 'transform ,child)))))

(defun %generate-actor-spawn (core-state actor-names)
  (loop :for actor :in actor-names
        :collect `(spawn-actor ,core-state ,actor)))

(defun parse-scene (scene-name scene-spec)
  (with-gensyms (core-state actor-table actor-name)
    (let* ((scene-spec `((@universe ((transform)) ,@scene-spec)))
           (actor-names (%generate-actor-names scene-spec))
           (actor-components (%generate-actor-components-table
                              scene-spec actor-names))
           (bindings (%generate-actor-bindings
                      actor-names actor-table)))
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
                actor-names actor-components)
             ,@(%generate-relationships scene-spec)
             ,@(%generate-actor-spawn
                core-state actor-names)
             (add-scene-tree-root ,core-state @universe)
             (values ,core-state ,actor-table)))))))

(defun get-scene (core-state scene-name)
  (gethash scene-name (scene-table core-state)))

(defun load-scene (core-state scene-name)
  (funcall (scene (get-scene core-state scene-name)) core-state))

(defmethod extension-file-types ((owner (eql 'scene)))
  (list "scene"))

(defun prepare-scenes (core-state path)
  (let ((*scene-table* (make-hash-table :test #'eq)))
    (flet ((%prepare ()
             (load-extensions 'scene path)
             *scene-table*))
      (merge-scene-table core-state (%prepare))
      core-state)))

(defmacro scene-definition (name (&key enabled) &body body)
  `(let ((scene (make-instance 'scene-definition
                               :scene ,(apply #'parse-scene name body)
                               :data (apply #'parse-scene ,name ',body))))
     (when ,enabled
       (setf (gethash ,name *scene-table*) scene))))
