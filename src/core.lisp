(in-package #:virality.engine)

(defvar *core-debug*)

(defclass core ()
  ((%resources :reader resources
               :initform (meta 'resources))
   (%options :accessor options)
   (%running-p :accessor running-p
               :initform t)
   (%rcache :reader rcache
            :initform (u:dict))
   (%frame-manager :accessor frame-manager
                   :initform nil)
   (%display :reader display
             :initform nil)
   (%scene-tree :reader scene-tree)
   (%cameras :accessor cameras
             :initform nil)
   (%input-data :reader input-data
                :initform (make-input-data))
   (%shaders :accessor shaders)
   (%materials :accessor materials
               :initform (%make-materials-table))
   (%textures :accessor textures
              :initform (%make-textures-table))
   (%context :reader context)
   (%tables :reader tables
            :initform (make-instance 'bookkeeping-tables))
   (%call-flows :reader call-flows
                :initform (u:dict))
   (%collider-system :accessor collider-system
                     :initform nil)
   (%analyzed-graphs :reader analyzed-graphs
                     :initform (u:dict #'equalp))
   (%recompilation-queue :reader recompilation-queue
                         :initarg :recompilation-queue
                         :initform (queues:make-queue :simple-cqueue))))

(defclass bookkeeping-tables ()
  ((%component-search-table :reader component-search-table
                            :initform (u:dict))
   (%component-preinit-by-type-view :reader component-preinit-by-type-view
                                    :initform (u:dict))
   (%component-init-by-type-view :reader component-init-by-type-view
                                 :initform (u:dict))
   (%component-active-by-type-view :reader component-active-by-type-view
                                   :initform (u:dict))
   (%component-predestroy-view :reader component-predestroy-view
                               :initform (u:dict))
   (%component-destroy-by-type-view :reader component-destroy-by-type-view
                                    :initform (u:dict))
   (%components-by-id :reader components-by-id
                      :initform (u:dict #'equalp))
   (%actor-predestroy-view :reader actor-predestroy-view
                           :initform (u:dict))
   (%actor-preinit-db :reader actor-preinit-db
                      :initform (u:dict))
   (%actor-init-db :reader actor-init-db
                   :initform (u:dict))
   (%actor-active-db :reader actor-active-db
                     :initform (u:dict))
   (%actor-destroy-db :reader actor-destroy-db
                      :initform (u:dict))
   (%actors-by-id :reader actors-by-id
                  :initform (u:dict #'equalp))
   (%objects-by-uuid :reader objects-by-uuid
                     :initform (u:dict #'equalp))))

(defun pending-preinit-tasks-p (core)
  "Return T if there are ANY components or actors in the preinit data structures
in CORE."
  (or (plusp (hash-table-count (actor-preinit-db (tables core))))
      (block done
        (u:do-hash-values (v (component-preinit-by-type-view
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
        (u:do-hash-values (v (component-destroy-by-type-view (tables core)))
          (when (plusp (hash-table-count v))
            (return-from done t))))))

(defun make-scene-tree (core)
  (let* ((context (context core))
         (actor (make-actor context :id 'universe :display-id "Universe"))
         (transform (make-component context 'comp:transform :actor actor)))
    (attach-component actor transform)
    (spawn-actor actor :parent nil)
    (execute-flow core :default
                  'initialize-phase
                  'entry/initialize-phase
                  :come-from-state-name 'ef-make-scene-tree)
    (setf (slot-value core '%scene-tree) actor)))

(defgeneric shared-storage (context key)
  (:method (context key)
    (u:href (shared-storage-table context) key))
  (:method (context (key component))
    (shared-storage context (component-type key))))

(defgeneric (setf shared-storage) (value context key)
  (:method (value context key)
    (setf (u:href (shared-storage-table context) key) value))
  (:method (value context (key component))
    (setf (shared-storage context (component-type key)) value)))

;;; Interim caching code for (often) resources. Uses nested hash tables like the
;;; shared-storage for components.
;;; TODO: change this when the real cache code shows up.

(defmethod rcache-layout ((entry-type symbol))
  '(eql))

(defmethod rcache-lookup (context (entry-type symbol) &rest keys)
  (apply #'rcache-lookup context entry-type keys))

(defmethod rcache-construct (context (entry-type symbol) &rest keys)
  (apply #'rcache-construct context entry-type keys))

(defmethod rcache-remove (context (entry-type symbol) &rest keys)
  (apply #'rcache-remove context entry-type keys))

(defmethod rcache-dispose (context (entry-type symbol) removed-value)
  (rcache-dispose context entry-type removed-value))

;; This might call rcache-construct if needed.
(defmethod rcache-lookup (context (entry-type symbol) &rest keys)
  (let ((core (core context)))
    (ensure-nested-hash-table (rcache core)
                              ;; NOTE: 'eq is for the rcache table itself.
                              (list* 'eq (rcache-layout entry-type))
                              (list* entry-type keys))
    (multiple-value-bind (value presentp)
        (apply #'u:href (rcache core) (list* entry-type keys))
      (unless presentp
        (setf value (apply #'rcache-construct context entry-type keys)
              (apply #'u:href (rcache core) (list* entry-type keys)) value))
      value)))

;; This might call rcache-dispose if needed.
(defmethod rcache-remove (context (entry-type symbol) &rest keys)
  (let ((core (core context)))
    (multiple-value-bind (value presentp)
        (apply #'u:href (rcache core) (list* entry-type keys))
      (when presentp
        (remhash (apply #'u:href (rcache core) (list* entry-type keys))
                 (rcache core))
        (rcache-dispose context entry-type value)))))

(defun map-scene-tree (func parent-actor &optional (level 0))
  "Similar to COMP:MAP-NODES, this instead maps over the actor starting at the
PARENT-ACTOR root. Level is how far you are down in the tree and is useful for
indention purposes. This function maps FUNC over each actor."
  (funcall func parent-actor level)
  (let ((parent-actor-transform
          (actor-component-by-type parent-actor 'comp:transform)))
    (dolist (child (comp:children parent-actor-transform))
      (map-scene-tree func (actor child) (1+ level)))))

(defun print-scene-tree (core)
  "Print an ascii representation of the scene tree indented to show children."
  (map-scene-tree
   (lambda (actor level)
     (let ((prefix-level 5))
       ;; NOTE: prefix-level is used for left justifying the level number to a
       ;; certian number of tens places for each actor. It makes the output
       ;; easier to read.
       (format t "~v@<~d~> ~v,,,v<~>Actor: ~s~%"
               prefix-level level level #\Space (display-id actor))
       (u:do-hash-values (component (components actor))
         (format t " ~v,,,v<~> + (~(~a~):~(~a~)) [~s]~%"
                 ;; 5 for the left justified
                 (+ prefix-level level) #\Space
                 (first
                  (package-nicknames
                   (symbol-package (component-type component))))
                 (component-type component) (display-id component)))))
   (scene-tree core)))
