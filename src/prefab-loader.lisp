(in-package #:virality.prefabs)

(defun make-actors (context prefab)
  (let ((actors (u:dict #'equalp))
        root)
    (u:do-hash (path node (parse-tree prefab))
      (with-slots (%name %id %display-id) node
        (let ((actor (v:make-actor context
                                   :prefab-node node
                                   :id %id
                                   :display-id (or %display-id %name))))
          (setf (u:href actors path) actor)
          (unless (parent node)
            (setf root actor)))))
    (values actors
            root)))

(defun make-actor-components (context actors)
  (let ((components (u:dict)))
    (u:do-hash-values (actor actors)
      (u:do-hash (type table (components-table (v::prefab-node actor)))
        (unless (u:href components actor)
          (setf (u:href components actor) (u:dict)))
        (u:do-hash (id data table)
          (unless (u:href components actor type)
            (setf (u:href components actor type) (u:dict #'equalp)))
          (let ((component (v:make-component context type)))
            (unless (u:href components actor type id)
              (setf (u:href components actor type id) (u:dict)))
            (setf (u:href components actor type id component) data)
            (v:attach-component actor component)))))
    (u:do-hash (actor actor-table components)
      (u:do-hash-values (id-table actor-table)
        (u:do-hash-values (component-table id-table)
          (u:do-hash (component data component-table)
            ;; Now, for each argument value itself, we adjust the exact lexical
            ;; scope it closed over with enough stuff for REF to work.
            (flet ((%init-injected-ref-environment (v)
                     (funcall (env-injection-control-func v)
                              :actors actors)
                     (funcall (env-injection-control-func v)
                              :components components)
                     (funcall (env-injection-control-func v)
                              :current-actor actor)
                     (funcall (env-injection-control-func v)
                              :current-component component)))
              (let ((args (loop :for (k v) :on (getf data :args) :by #'cddr
                                :append
                                (list k (progn
                                          ;; Set up the injected REF environment
                                          ;; specific to >THIS< argument value
                                          ;; which may have been replaced per
                                          ;; policy rules, etc, etc, etc
                                          (%init-injected-ref-environment v)
                                          (funcall (thunk v) context))))))
                (apply #'reinitialize-instance
                       component :actor actor args)))))))))

(defun make-actor-relationships (context prefab actors parent)
  (let ((parent (or parent (v::scene-tree (v::core context))))
        (root (u:href actors (path (root prefab)))))
    (u:do-hash-values (actor actors)
      (let ((node (v::prefab-node actor)))
        (u:do-hash-values (child (children node))
          (c/xform:add-child
           (v:component-by-type actor 'c/xform:transform)
           (v:component-by-type (u:href actors (path child))
                                'c/xform:transform)))))
    (c/xform:add-child
     (v:component-by-type parent 'c/xform:transform)
     (v:component-by-type root 'c/xform:transform))))

(defun make-factory (prefab)
  (lambda (core &key parent)
    (u:mvlet* ((context (v:context core))
               (actors root (make-actors context prefab)))
      (make-actor-components context actors)
      (make-actor-relationships context prefab actors parent)
      (u:do-hash-values (actor actors)
        (v:spawn-actor actor))
      root)))

(defun load-prefab (core spec parent)
  (destructuring-bind (name library &key ttl) spec
    (let* ((prefab (find-prefab name library))
           (actor (funcall (func prefab) core :parent parent)))
      (v:destroy-after-time actor :ttl ttl)
      actor)))

(defun make-prefab-instance (core prefab-descriptor &key parent)
  (let (roots)
    (dolist (spec prefab-descriptor)
      (push (load-prefab core spec parent) roots))
    (nreverse roots)))
