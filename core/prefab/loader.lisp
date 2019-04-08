(in-package :first-light.prefab)

(defun make-actors (context prefab)
  (let ((actors (au:dict #'equalp)))
    (au:do-hash (path node (parse-tree prefab))
      (with-slots (%name %id %display-id) node
        (setf (au:href actors path)
              (make-actor context
                          :prefab-node node
                          :id %id
                          :display-id (or %display-id %name)))))
    actors))

(defun make-actor-components (context actors setter)
  (let (components)
    (au:do-hash-values (actor actors)
      (au:do-hash (type table (au:href (components-table (prefab-node actor))))
        (au:do-hash-values (data table)
          (let ((component (make-component context type)))
            (attach-component actor component)
            (push (list data actor component) components)))))
    (dolist (c components)
      (destructuring-bind (data actor component) c
        (funcall setter :current-actor actor)
        (let ((args (loop :for (k v) :on (getf data :args) :by #'cddr
                          :append (list k (funcall v context)))))
          (apply #'reinitialize-instance component :actor actor args))))))

(defun make-actor-relationships (context prefab actors &optional parent)
  (let ((parent (or parent (scene-tree (core context))))
        (root (au:href actors (path (root prefab)))))
    (au:do-hash-values (actor actors)
      (let ((node (prefab-node actor)))
        (au:do-hash-values (child (children node))
          (fl.comp:transform-add-child
           (actor-component-by-type actor 'fl.comp:transform)
           (actor-component-by-type (au:href actors (path child))
                                    'fl.comp:transform)))))
    (fl.comp:transform-add-child
     (actor-component-by-type parent 'fl.comp:transform)
     (actor-component-by-type root 'fl.comp:transform))))

(defun make-factory (prefab setter)
  (lambda (core)
    (let* ((context (context core))
           (actors (make-actors context prefab)))
      (funcall setter :actors actors)
      (make-actor-components context actors setter)
      (make-actor-relationships context prefab actors)
      (au:do-hash-values (actor actors)
        (spawn-actor actor)))))

(defun load-prefabs (core prefabs)
  (%fl:make-scene-tree core)
  (dolist (spec prefabs)
    (destructuring-bind (name library) spec
      (let ((prefab (find-prefab name library)))
        (funcall (func prefab) core)))))
