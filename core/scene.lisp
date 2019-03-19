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
           (au:flatten component))))
    (dolist (actor component-actors)
      (unless (find actor actor-names)
        (error "A component references the undefined actor: ~a" actor)))))

(defun %generate-actor-components-table (scene-spec actor-names &optional table)
  (let ((table (or table (au:dict #'eq))))
    (dolist (actor scene-spec)
      (destructuring-bind (actor components . child) actor
        (unless (member 'fl.comp:transform components :key #'car)
          (push '(fl.comp:transform) components))
        (dolist (component (reverse components))
          (%type-check-actor-component actor-names component)
          (push component (au:href table actor)))
        (%generate-actor-components-table child actor-names table)))
    table))

(defun %generate-component-initializers (core-state actor-components)
  (flet ((generate-component-forms (components)
           (let ((component-forms))
             (dolist (c components)
               (push `(make-component (context ,core-state) ',(first c)) component-forms))
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
        :for components = (au:href component-table actor)
        :append
        (loop :for (component . initargs) :in components
              :for symbol = (au:unique-name "COMPONENT-")
              :collect `(let ((,symbol (actor-component-by-type ,actor ',component)))
                          (setf (initializer-thunk ,symbol)
                                (lambda ()
                                  (reinitialize-instance ,symbol :actor ,actor ,@initargs)))))))

(defun %parse-scene-actor-relationships (core-state scene-spec)
  (labels ((traverse (tree &optional parent)
             (destructuring-bind (child components . sub-tree) tree
               (declare (ignore components))
               (let ((result (list (cons parent child))))
                 (if sub-tree
                     (append result (mapcan (lambda (x) (traverse x child)) sub-tree))
                     result)))))
    (loop :with root = `(scene-tree ,core-state)
          :for (parent . child) :in (mapcan #'traverse scene-spec)
          :collect `(fl.comp:transform-add-child
                     (actor-component-by-type ,(or parent root) 'transform)
                     (actor-component-by-type ,child 'transform)))))

(defun %generate-actor-spawn (actor-names)
  (loop :for actor :in actor-names
        :collect `(spawn-actor ,actor)))

(defun collect-scene-actor-bindings (actor-names actor-table)
  (mapcan
   (lambda (x)
     `((,x (au:href ,actor-table ',x))))
   actor-names))

(defun parse-scene (context scene-spec)
  (au:with-unique-names (core-state actor-table actor-name)
    (let* ((actor-names (%generate-actor-names scene-spec))
           (actor-components (%generate-actor-components-table scene-spec actor-names)))
      `(lambda (,core-state)
         (let ((,actor-table (au:dict #'eq))
               (,context (context ,core-state)))
           (dolist (,actor-name ',actor-names)
             (setf (au:href ,actor-table ,actor-name)
                   (make-actor ,context :id ,actor-name)))
           (let ,(collect-scene-actor-bindings actor-names actor-table)
             ,@(%generate-component-initializers core-state actor-components)
             ,@(%generate-component-thunks actor-names actor-components)
             ,@(%parse-scene-actor-relationships core-state scene-spec)
             ,@(%generate-actor-spawn actor-names)
             (au:noop)))))))

(defmacro define-scene (name (&key (context 'context)) &body body)
  (au:with-unique-names (definition code scene)
    `(symbol-macrolet ((,definition (fl.data:get 'scenes))
                       (,code (fl.data:get 'scene-code)))
       (let ((,scene ,(parse-scene context body)))
         (unless ,definition
           (fl.data:set 'scenes (au:dict #'eq)))
         (unless ,code
           (fl.data:set 'scene-code (au:dict #'eq)))
         (setf (au:href ,definition ',name) ,scene
               (au:href ,code ',name) (parse-scene ',context ',body))
         (export ',name)))))

(defun get-scene (core-state scene-name)
  (au:href (scenes core-state) scene-name))

(defun get-scene-code (scene-name)
  (let ((table (fl.data:get 'scene-code)))
    (au:href table scene-name)))

(defun load-scene (core-state name)
  (au:if-let ((scene (get-scene core-state name)))
    (funcall (get-scene core-state name) core-state)
    (error "Cannot find the scene name: ~s." name)))

(defun load-scene-definitions (core-state)
  (setf (slot-value core-state '%scene-tree) (%make-scene-tree core-state))
  (au:do-hash (k v (fl.data:get 'scenes))
    (setf (au:href (scenes core-state) k) v)))
