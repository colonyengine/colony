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

(defun make-actor-components (context actors)
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
    (au:do-hash (actor actor-table components)
      (au:do-hash-values (id-table actor-table)
        (au:do-hash-values (component-table id-table)
          (au:do-hash (component data component-table)
            ;; Now, for each argument value itself, we adjust the exact
            ;; lexical scope it closed over with enough stuff for REF to
            ;; work.
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

(defun make-factory (prefab)
  (lambda (core &key parent)
    (au:mvlet* ((context (context core))
                (actors root (make-actors context prefab)))
      (make-actor-components context actors)
      (make-actor-relationships context prefab actors parent)
      (au:do-hash-values (actor actors)
        (spawn-actor actor))
      root)))

(defun load-prefab (core spec parent)
  (destructuring-bind (name library &key ttl) spec
    (let* ((prefab (find-prefab name library))
           (actor (funcall (func prefab) core :parent parent)))
      (destroy-after-time actor :ttl ttl)
      actor)))

(defun make-prefab-instance (core prefab-descriptor &key parent)
  (let (roots)
    (dolist (spec prefab-descriptor)
      (push (load-prefab core spec parent) roots))
    (nreverse roots)))
