(in-package :%first-light)

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
           (fl.util:flatten component))))
    (dolist (actor component-actors)
      (unless (find actor actor-names)
        (error "A component references the undefined actor: ~a" actor)))))

(defun %generate-actor-components-table (scene-spec actor-names &optional table)
  (let ((table (or table (fl.util:dict #'eq))))
    (dolist (actor scene-spec)
      (destructuring-bind (actor components . child) actor
        (unless (member 'transform components :key #'car)
          (push '(transform) components))
        (dolist (component (reverse components))
          (%type-check-actor-component actor-names component)
          (push component (fl.util:href table actor)))
        (%generate-actor-components-table child actor-names table)))
    table))

(defun %generate-actor-bindings (actor-names table)
  (mapcan
   (lambda (actor)
     `((,actor (fl.util:href ,table ',actor))))
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
         (push `(attach-multiple-components ,actor ,@(generate-component-forms components))
               result))
       actor-components)
      result)))

(defun %generate-component-thunks (actor-names component-table)
  (loop :for actor :in actor-names
        :for components = (fl.util:href component-table actor)
        :append
        (loop :for (component . initargs) :in components
              :for symbol = (fl.util:unique-name "COMPONENT-")
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
          :collect `(,(fl.util:ensure-symbol 'add-child 'fl.comp)
                     (actor-component-by-type ,(or parent root) 'transform)
                     (actor-component-by-type ,child 'transform)))))

(defun %generate-actor-spawn (core-state actor-names)
  (loop :for actor :in actor-names
        :collect `(spawn-actor ,actor (context ,core-state))))

(defun parse-scene (scene-name context scene-spec)
  (fl.util:with-unique-names (core-state actor-table actor-name)
    (let* ((actor-names (%generate-actor-names scene-spec))
           (actor-components (%generate-actor-components-table scene-spec actor-names))
           (bindings (%generate-actor-bindings actor-names actor-table)))
      `(lambda (,core-state)
         (let ((,actor-table (fl.util:dict #'eq))
               (,context (context ,core-state)))
           (declare (ignorable context))
           (dolist (,actor-name ',actor-names)
             (setf (fl.util:href ,actor-table ,actor-name)
                   (make-actor (context ,core-state) :id ,actor-name :scene ,scene-name)))
           (let ,bindings
             ,@(%generate-component-initializers core-state actor-components)
             ,@(%generate-component-thunks actor-names actor-components)
             ,@(%generate-relationships core-state scene-spec)
             ,@(%generate-actor-spawn core-state actor-names)
             (values ,core-state ,actor-table)))))))

(defun get-scene (core-state scene-name)
  (fl.util:href (scenes core-state) scene-name))

(defun load-scene (core-state name)
  (fl.util:if-let ((scene (get-scene core-state name)))
    (funcall (get-scene core-state name) core-state)
    (error "Cannot find the scene name: ~s." name)))

(defmethod extension-file-type ((extension-type (eql :scene)))
  "scene")

(defmethod prepare-extension ((extension-type (eql :scene)) core-state)
  (let ((%temp (fl.util:dict #'eq)))
    (declare (special %temp))
    (flet ((%prepare ()
             (map-extensions (context core-state) extension-type)
             %temp))
      (setf (slot-value core-state '%scene-tree) (%make-scene-tree core-state))
      (fl.util:do-hash (k v (%prepare))
        (setf (fl.util:href (scenes core-state) k) v)))))

(defmacro define-scene (name (&key (enabled t) (context 'context)) &body body)
  (fl.util:with-unique-names (scene)
    `(let ((,scene ,(parse-scene `',name context body)))
       (declare (special %fl::%temp))
       ,(when enabled
          `(setf (fl.util:href %fl::%temp ',name) ,scene))
       (export ',name))))
