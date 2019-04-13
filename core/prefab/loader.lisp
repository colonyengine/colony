(in-package :first-light.prefab)

(defun make-actors (context prefab)
  (let ((actors (au:dict #'equalp))
        (root))
    (au:do-hash (path node (parse-tree prefab))
      (with-slots (%name %id %display-id) node
        (let ((actor (make-actor context
                                 :prefab-node node
                                 :id %id
                                 :display-id (or %display-id %name))))
          (setf (au:href actors path) actor)
          (unless (parent node)
            (setf root actor)))))
    (values actors
            root)))

(defun make-actor-components (context actors setter)
  (let ((components (au:dict #'eq)))
    (au:do-hash-values (actor actors)
      (au:do-hash (type table (au:href (components-table (prefab-node actor))))
        (unless (au:href components actor)
          (setf (au:href components actor) (au:dict #'eq)))
        (au:do-hash (id data table)
          (unless (au:href components actor type)
            (setf (au:href components actor type) (au:dict #'equalp)))
          (let ((component (make-component context type)))
            (unless (au:href components actor type id)
              (setf (au:href components actor type id) (au:dict #'eq)))
            (setf (au:href components actor type id component) data)
            (attach-component actor component)))))
    (funcall setter :components components)
    (au:do-hash (actor actor-table components)
      (funcall setter :current-actor actor)
      (au:do-hash-values (id-table actor-table)
        (au:do-hash-values (component-table id-table)
          (au:do-hash (component data component-table)
            (funcall setter :current-component component)
            (let ((args (loop :for (k v) :on (getf data :args) :by #'cddr
                              :append (list k (funcall v context)))))
              (apply #'reinitialize-instance component :actor actor args))))))))

(defun make-actor-relationships (context prefab actors parent)
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
  (lambda (core &key parent)
    (au:mvlet* ((context (context core))
                (actors root (make-actors context prefab)))
      (funcall setter :actors actors)
      (make-actor-components context actors setter)
      (make-actor-relationships context prefab actors parent)
      (au:do-hash-values (actor actors)
        (spawn-actor actor))
      root)))

(defun load-prefabs (core prefabs &key parent ttl)
  (let (roots)
    (dolist (spec prefabs)
      (destructuring-bind (name library) spec
        (let* ((prefab (find-prefab name library))
               (actor (funcall (func prefab) core :parent parent)))
          (when ttl
            (destroy actor :ttl ttl))
          (push actor roots))))
    (values-list (nreverse roots))))
