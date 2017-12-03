(in-package :first-light)

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
  (loop :with table = (or table (make-hash-table))
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

(defun %generate-component-thunks (actor-names component-table)
  (loop :for actor :in actor-names
        :for components = (gethash actor component-table)
        :append
        (loop :for (component . initargs) :in components
              :for symbol = (gensym "COMPONENT-")
              :collect `(let ((,symbol (actor-component-by-type ,actor
                                                                ',component)))
                          (setf (initializer-thunk ,symbol)
                                (lambda ()
                                  (reinitialize-instance
                                   ,symbol :actor ,actor ,@initargs)))))))

(defun %generate-relationships (core-state scene-spec)
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
    (loop :with root = `(scene-tree ,core-state)
          :with children = (apply #'append (mapcar #'traverse scene-spec))
          :for (parent . child) :in children
          :collect `(add-child
                     (actor-component-by-type ,(or parent root) 'transform)
                     (actor-component-by-type ,child 'transform)))))

(defun %generate-actor-spawn (core-state actor-names)
  (loop :for actor :in actor-names
        :collect `(spawn-actor ,core-state ,actor)))

(defun parse-scene (scene-name scene-spec)
  (with-gensyms (core-state actor-table actor-name)
    (let* ((actor-names (%generate-actor-names scene-spec))
           (actor-components (%generate-actor-components-table
                              scene-spec actor-names))
           (bindings (%generate-actor-bindings
                      actor-names actor-table)))
      `(lambda (,core-state)
         (let ((,actor-table (make-hash-table)))
           (dolist (,actor-name ',actor-names)
             (setf (gethash ,actor-name ,actor-table)
                   (make-actor :id ,actor-name :scene ,scene-name)))
           (let ,bindings
             ,@(%generate-component-initializers actor-components)
             ,@(%generate-component-thunks actor-names actor-components)
             ,@(%generate-relationships core-state scene-spec)
             ,@(%generate-actor-spawn core-state actor-names)
             (values ,core-state ,actor-table)))))))

(defun get-scene (core-state scene-name)
  (gethash scene-name (scenes core-state)))

(defun load-scene (core-state name)
  (funcall (get-scene core-state name) core-state))

(defun load-default-scene (core-state)
  (if-let ((default (cfg (context core-state) :default-scene)))
    (load-scene core-state default)
    (error "No default scene specified in settings.cfg.")))

(defmethod extension-file-type ((extension-type (eql 'scene)))
  "scene")

(defmethod prepare-extension ((extension-type (eql 'scene)) owner path)
  (let ((%temp-scene (make-hash-table)))
    (declare (special %temp-scene))
    (flet ((%prepare ()
             (load-extensions extension-type path)
             %temp-scene))
      (with-slots (%scene-tree) owner
        (setf %scene-tree (%make-scene-tree owner)))
      (maphash
       (lambda (key value)
         (setf (gethash key (scenes owner)) value))
       (%prepare)))))

(defmacro define-scene (name (&key enabled) &body body)
  `(let ((scene ,(apply #'parse-scene `',name body)))
     (declare (special %temp-scene))
     ,(when enabled
       `(setf (gethash ',name %temp-scene) scene))))
