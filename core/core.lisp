(in-package #:%first-light)

(defvar *core-debug*)

(defclass core ()
  ((%resources :reader resources
               :initform (fl.data:get 'resources))
   (%options :accessor options)
   (%running-p :accessor running-p
               :initform t)
   (%rcache :reader rcache
            :initform (au:dict #'eq))
   (%frame-manager :accessor frame-manager
                   :initform nil)
   (%display :reader display
             :initform nil)
   (%scene-tree :reader scene-tree)
   (%cameras :accessor cameras
             :initform nil)
   (%input-data :reader input-data
                :initform (fl.input:make-input-data))
   (%shaders :accessor shaders)
   (%materials :accessor materials
               :initform (%make-materials-table))
   (%textures :accessor textures
              :initform (%make-textures-table))
   (%context :reader context)
   (%tables :reader tables
            :initform (make-instance 'bookkeeping-tables))
   (%call-flows :reader call-flows
                :initform (au:dict #'eq))
   (%collider-system :accessor collider-system
                     :initform nil)
   (%analyzed-graphs :reader analyzed-graphs
                     :initform (au:dict #'equalp))
   (%recompilation-queue :reader recompilation-queue
                         :initarg :recompilation-queue
                         :initform (fl.dst:make-queue :simple-cqueue))))

(defclass bookkeeping-tables ()
  ((%component-search-table :reader component-search-table
                            :initform (au:dict #'eq))
   (%component-preinit-by-type-view :reader component-preinit-by-type-view
                                    :initform (au:dict #'eq))
   (%component-init-by-type-view :reader component-init-by-type-view
                                 :initform (au:dict #'eq))
   (%component-active-by-type-view :reader component-active-by-type-view
                                   :initform (au:dict #'eq))
   (%component-predestroy-view :reader component-predestroy-view
                               :initform (au:dict #'eq))
   (%component-destroy-by-type-view :reader component-destroy-by-type-view
                                    :initform (au:dict #'eq))
   (%components-by-id :reader components-by-id
                      :initform (au:dict #'equalp))
   (%actor-predestroy-view :reader actor-predestroy-view
                           :initform (au:dict #'eq))
   (%actor-preinit-db :reader actor-preinit-db
                      :initform (au:dict #'eq))
   (%actor-init-db :reader actor-init-db
                   :initform (au:dict #'eq))
   (%actor-active-db :reader actor-active-db
                     :initform (au:dict #'eq))
   (%actor-destroy-db :reader actor-destroy-db
                      :initform (au:dict #'eq))
   (%actors-by-id :reader actors-by-id
                  :initform (au:dict #'equalp))
   (%objects-by-uuid :reader objects-by-uuid
                     :initform (au:dict #'equalp))))

(defun pending-preinit-tasks-p (core)
  "Return T if there are ANY components or actors in the preinit data structures
in CORE."
  (or (plusp (hash-table-count (actor-preinit-db (tables core))))
      (block done
        (au:do-hash-values (v (component-preinit-by-type-view
                               (tables core)))
          (when (plusp (hash-table-count v))
            (return-from done t))))))

(defun pending-predestroy-tasks-p (core)
  "Return T if there are ANY components or actors that are in the predestroy
  data structures in CORE."
  (or (plusp (hash-table-count (component-predestroy-view (tables core))))
      (plusp (hash-table-count (actor-predestroy-view (tables core))))))

(defun pending-destroy-tasks-p (core)
  "Return T of there are ANY components or actors that are in the destroy data
structures in CORE."
  (or (plusp (hash-table-count (actor-destroy-db (tables core))))
      (block done
        (au:do-hash-values (v (component-destroy-by-type-view (tables core)))
          (when (plusp (hash-table-count v))
            (return-from done t))))))

(defun make-scene-tree (core)
  (let* ((context (context core))
         (actor (make-actor context :id 'universe :display-id "Universe"))
         (transform (make-component context 'transform :actor actor)))
    (attach-component actor transform)
    (spawn-actor actor :parent nil)
    (execute-flow core :default
                  'initialize-phase
                  'entry/initialize-phase
                  :come-from-state-name 'ef-make-scene-tree)
    (setf (slot-value core '%scene-tree) actor)))

(defgeneric shared-storage (context key)
  (:method (context key)
    (au:href (shared-storage-table context) key))
  (:method (context (key component))
    (shared-storage context (component-type key))))

(defgeneric (setf shared-storage) (value context key)
  (:method (value context key)
    (setf (au:href (shared-storage-table context) key) value))
  (:method (value context (key component))
    (setf (shared-storage context (component-type key)) value)))

;;; Interim caching code for (often) resources. Uses nested hash tables like the
;;; shared-storage for components.
;;; TODO: change this when the real cache code shows up.

(defgeneric rcache-layout (entry-type)
  (:method ((entry-type symbol))
    '(eql)))

(defgeneric rcache-lookup (context entry-type &rest keys)
  (:method ((entry-type symbol) context &rest keys)
    (apply #'rcache-lookup entry-type context keys)))

(defgeneric rcache-construct (context entry-type &rest keys)
  (:method ((entry-type symbol) context &rest keys)
    (apply #'rcache-construct entry-type context keys)))

(defgeneric rcache-remove (context entry-type &rest keys)
  (:method ((entry-type symbol) context &rest keys)
    (apply #'rcache-remove entry-type context keys)))

(defgeneric rcache-dispose (context entry-type removed-value)
  (:method ((entry-type symbol) context removed-value)
    (rcache-dispose entry-type context removed-value)))

;; This might call rcache-construct if needed.
(defmethod rcache-lookup (context (entry-type symbol)
                          &rest keys)
  (let ((core (core context)))
    (ensure-nested-hash-table (rcache core)
                              ;; NOTE: 'eq is for the rcache table itself.
                              (list* 'eq (rcache-layout entry-type))
                              (list* entry-type keys))
    (multiple-value-bind (value presentp)
        (apply #'au:href (rcache core) (list* entry-type keys))
      (unless presentp
        (setf value (apply #'rcache-construct context entry-type keys)
              (apply #'au:href (rcache core) (list* entry-type keys)) value))
      value)))

;; This might call rcache-dispose if needed.
(defmethod rcache-remove (context (entry-type symbol) &rest keys)
  (let ((core (core context)))
    (multiple-value-bind (value presentp)
        (apply #'au:href (rcache core) (list* entry-type keys))
      (when presentp
        (remhash (apply #'au:href (rcache core) (list* entry-type keys))
                 (rcache core))
        (rcache-dispose context entry-type value)))))

(defun map-scene-tree (func parent-actor &optional (level 0))
  "Similar to FL.COMP::MAP-NODES, this instead maps over the actor starting
at the PARENT-ACTOR root. Level is how far you are down in the tree and is
useful for indention purposes. This function maps FUNC over each actor."
  (funcall func parent-actor level)
  (let ((parent-actor-transform
          (actor-component-by-type parent-actor 'fl.comp:transform)))
    ;; TODO: Fix FL.COMP::CHILDREN (on the transform component) to be exported.
    (dolist (child (fl.comp::children parent-actor-transform))
      (map-scene-tree func (actor child) (1+ level)))))

(defun print-scene-tree (core)
  "Print an ascii representation of the scene tree indented to show children."
  (map-scene-tree
   (lambda (actor level)
     (let ((prefix-level 5))
       ;; NOTE: prefix-level is used for left justifying the level number to a
       ;; certian number of tens places for each actor. It makes the output
       ;; easier to read.
       (format t "~v@<~D~> ~v,,,v<~>Actor: ~S~%"
               prefix-level level level #\Space (display-id actor))
       (au:do-hash-values (component (components actor))
         (format t " ~v,,,v<~> + (~(~A~):~(~A~)) [~S]~%"
                 ;; 5 for the left justified
                 (+ prefix-level level) #\Space
                 (first
                  (package-nicknames
                   (symbol-package (component-type component))))
                 (component-type component) (display-id component)))))
   (scene-tree core)))
