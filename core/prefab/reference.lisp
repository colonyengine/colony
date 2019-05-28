(in-package #:first-light.prefab)

(defclass reference ()
  ((%id :reader id
        :initarg :id)
   (%current-actor :reader current-actor
                   :initarg :current-actor)
   (%current-component :reader current-component
                       :initarg :current-component)
   (%actors :reader actors
            :initarg :actors)
   (%components :reader components
                :initarg :components)
   (%component :reader component
               :initarg :component)
   (%merge-id :reader merge-id
              :initarg :merge-id)))

(defun ref (id &key component-type merge-id)
  (declare (ignore id component-type merge-id))
  (au:noop))

(defun parse-reference-path/absolute (reference)
  (with-slots (%id %actors) reference
    (ensure-path-no-trailing-slash %id)
    (ensure-path-valid %id)
    (ensure-path-not-parent %id)
    (au:href %actors %id)))

(defun parse-reference-path/parent (reference)
  (with-slots (%id %current-actor %actors) reference
    (labels ((find-actor (path node)
               (let* ((node (parent node))
                      (parent-path (path node))
                      (rest (subseq path 3)))
                 (if (and (au:string-starts-with-p rest "../")
                          (> (count #\/ parent-path) 1))
                     (find-actor rest node)
                     (values parent-path
                             rest)))))
      (au:mvlet* ((parent sub-path (find-actor
                                    %id (prefab-node %current-actor)))
                  (sub-path (string-left-trim "./" sub-path)))
        (ensure-path-no-trailing-slash sub-path)
        (ensure-path-valid sub-path)
        (ensure-path-not-parent sub-path)
        (au:href %actors (make-node-path parent sub-path))))))

(defun parse-reference-path/relative (reference)
  (with-slots (%id %current-actor %actors) reference
    (let* ((parent-path (path (prefab-node %current-actor)))
           (path (make-node-path parent-path %id)))
      (ensure-path-no-trailing-slash path)
      (ensure-path-valid path)
      (ensure-path-not-parent path)
      (au:href %actors path))))

(defun parse-reference-path (reference)
  (case (char (id reference) 0)
    (#\/ (parse-reference-path/absolute reference))
    (#\. (parse-reference-path/parent reference))
    (t (parse-reference-path/relative reference))))

(defun get-reference-actor (reference)
  (with-slots (%id %current-actor) reference
    (cond
      ((eq %id :self)
       %current-actor)
      ((stringp %id)
       (parse-reference-path reference)))))

(defun get-reference-component (reference)
  (with-slots (%id %current-component %merge-id %component %components)
      reference
    (if (eq %component :self)
        %current-component
        (let* ((actor (get-reference-actor reference))
               (type-table (au:href %components actor %component %merge-id)))
          (cond
            ((and (null %merge-id)
                  type-table
                  (= (hash-table-count type-table) 1))
             (first (au:hash-keys type-table)))
            (type-table
             (au:href type-table %merge-id))
            (t (error "Component reference ~s not found." %component)))))))

(defun lookup-reference (args current-actor current-component actors components)
  (destructuring-bind (id &key component merge-id) args
    (let ((reference (make-instance 'reference
                                    :id id
                                    :component component
                                    :merge-id merge-id
                                    :current-actor current-actor
                                    :current-component current-component
                                    :actors actors
                                    :components components)))
      (if (component reference)
          (get-reference-component reference)
          (get-reference-actor reference)))))
