(in-package :fl.core)

(defun %type-check-actor (actor actors-list)
  (unless (char= (char (symbol-name actor) 0) #\@)
    (error "Actor names must begin with '@': ~a" actor))
  (when (find actor actors-list)
    (error "Actors cannot be duplicated: ~a" actor)))

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
           (au:flatten component))))
    (dolist (actor component-actors)
      (unless (find actor actor-names)
        (error "A component references the undefined actor: ~a" actor)))))

(defun %generate-actor-components-table (scene-spec actor-names &optional table)
  (loop :with table = (or table (au:dict #'eq))
        :for (actor components . child) :in scene-spec
        :do (dolist (component (reverse components))
              (%type-check-actor-component actor-names component)
              (push component (au:href table actor)))
            (%generate-actor-components-table child actor-names table)
        :finally (return table)))

(defun %generate-actor-bindings (actor-names table)
  (mapcan
   (lambda (actor)
     `((,actor (au:href ,table ',actor))))
   actor-names))

(defun %generate-component-initializers (core-state actor-components)
  (flet ((generate-component-forms (components)
           (let ((component-forms))
             (dolist (c components)
               (push `(make-component ',(first c) (context ,core-state)) component-forms))
             component-forms)))
    (let ((result))
      (maphash
       (lambda (actor components)
         (push `(attach-multiple-components ,actor (list ,@(generate-component-forms components)))
               result))
       actor-components)
      result)))

(defun %generate-component-thunks (actor-names component-table)
  (loop :for actor :in actor-names
        :for components = (au:href component-table actor)
        :append
        (loop :for (component . initargs) :in components
              :for symbol = (au:unique-name "COMPONENT-")
              :collect `(let ((,symbol (actor-component-by-type ,actor ',component)))
                          (setf (initializer-thunk ,symbol)
                                (lambda ()
                                  (reinitialize-instance ,symbol :actor ,actor ,@initargs)))))))

(defun %generate-relationships (core-state scene-spec)
  (labels ((traverse (tree &optional parent)
             (destructuring-bind (child components . sub-tree) tree
               (declare (ignore components))
               (let ((result (list (cons parent child))))
                 (if sub-tree
                     (append
                      result
                      (apply #'append (mapcar (lambda (x) (traverse x child)) sub-tree)))
                     result)))))
    (loop :with root = `(scene-tree ,core-state)
          :with children = (apply #'append (mapcar #'traverse scene-spec))
          :for (parent . child) :in children
          :collect `(,(au:ensure-symbol 'add-child 'fl.comp.transform)
                     (actor-component-by-type ,(or parent root) 'transform)
                     (actor-component-by-type ,child 'transform)))))

(defun %generate-actor-spawn (core-state actor-names)
  (loop :for actor :in actor-names
        :collect `(spawn-actor ,actor (context ,core-state))))

(defun parse-scene (scene-name scene-spec)
  (au:with-unique-names (core-state actor-table actor-name)
    (let* ((actor-names (%generate-actor-names scene-spec))
           (actor-components (%generate-actor-components-table scene-spec actor-names))
           (bindings (%generate-actor-bindings actor-names actor-table)))
      `(lambda (,core-state)
         (let ((,actor-table (au:dict #'eq)))
           (dolist (,actor-name ',actor-names)
             (setf (au:href ,actor-table ,actor-name)
                   (make-actor (context ,core-state) :id ,actor-name :scene ,scene-name)))
           (let ,bindings
             ,@(%generate-component-initializers core-state actor-components)
             ,@(%generate-component-thunks actor-names actor-components)
             ,@(%generate-relationships core-state scene-spec)
             ,@(%generate-actor-spawn core-state actor-names)
             (values ,core-state ,actor-table)))))))

(defun get-scene (core-state scene-name)
  (au:href (scenes core-state) scene-name))

(defun load-scene (core-state name)
  (au:if-let ((scene (get-scene core-state name)))
    (funcall (get-scene core-state name) core-state)
    (error "Cannot find the scene name: ~s." name)))

(defmethod extension-file-type ((extension-type (eql 'scene)))
  "scene")

(defmethod prepare-extension ((extension-type (eql 'scene)) owner path)
  (let ((%temp-scene (au:dict #'eq)))
    (declare (special %temp-scene))
    (flet ((%prepare ()
             (load-extensions extension-type path)
             %temp-scene))
      (with-slots (%scene-tree) owner
        (setf %scene-tree (%make-scene-tree owner)))
      (maphash
       (lambda (key value)
         (setf (au:href (scenes owner) key) value))
       (%prepare)))))

(defmacro define-scene (name (&key enabled viewport) &body body)
  `(let ((scene ,(parse-scene `',name body)))
     (declare (special %temp-scene))
     ,(when enabled
        `(setf (au:href %temp-scene ',name) scene))))
