(in-package :first-light.prefab)

(defclass reference ()
  ((%id :reader id
        :initarg :id)
   (%current :reader current
             :initarg :current)
   (%actor-table :reader actor-table
                 :initarg :actor-table)
   (%component-type :reader component-type
                    :initarg :component-type)
   (%merge-id :reader merge-id
              :initarg :merge-id)))

(defun make-reference (id current actor-table component-type merge-id)
  (make-instance 'reference
                 :id id
                 :current current
                 :actor-table actor-table
                 :component-type component-type
                 :merge-id merge-id))

(defun parse-reference-path/absolute (reference)
  (with-slots (%id %actor-table) reference
    (ensure-path-no-trailing-slash %id)
    (ensure-path-valid %id)
    (ensure-path-not-parent %id)
    (au:href %actor-table %id)))

(defun parse-reference-path/parent (reference)
  (with-slots (%id %current %actor-table) reference
    (labels ((find-actor (path node)
               (let* ((node (parent node))
                      (parent-path (path node))
                      (rest (subseq path 3)))
                 (if (and (au:string-starts-with-p rest "../")
                          (> (count #\/ parent-path) 1))
                     (find-actor rest node)
                     (values parent-path
                             rest)))))
      (au:mvlet* ((parent sub-path (find-actor %id (prefab-node %current)))
                  (sub-path (string-left-trim "./" sub-path)))
        (ensure-path-no-trailing-slash sub-path)
        (ensure-path-valid sub-path)
        (ensure-path-not-parent sub-path)
        (au:href %actor-table (make-node-path parent sub-path))))))

(defun parse-reference-path/relative (reference)
  (with-slots (%id %current %actor-table) reference
    (let* ((parent-path (path (prefab-node %current)))
           (path (make-node-path parent-path %id)))
      (ensure-path-no-trailing-slash path)
      (ensure-path-valid path)
      (ensure-path-not-parent path)
      (au:href %actor-table path))))

(defun parse-reference-path (reference)
  (case (char (id reference) 0)
    (#\/ (parse-reference-path/absolute reference))
    (#\. (parse-reference-path/parent reference))
    (t (parse-reference-path/relative reference))))

(defun parse-reference-actor (reference)
  (with-slots (%id %current) reference
    (cond
      ((eq %id :self)
       %current)
      ((stringp %id)
       (parse-reference-path reference)))))

(defun parse-reference-component (reference)
  ;; TODO: Support getting right type by merge-id.
  (let ((actor (parse-reference-actor reference)))
    (actor-component-by-type actor (component-type reference))))

(defun parse-reference (reference)
  (if (component-type reference)
      (parse-reference-component reference)
      (parse-reference-actor reference)))
