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
                     (return-from find-actor
                       (let* ((cleaned (remove-if (lambda (x) (find x "./")) rest))
                              (final-path (make-node-path parent-path cleaned)))
                         (au:href %actor-table final-path)))))))
      (find-actor %id (prefab-node %current)))))

(defun parse-reference-path/relative (reference)
  (with-slots (%id %current %actor-table) reference
    (let* ((parent-path (path (prefab-node %current)))
           (final-path (make-node-path parent-path %id)))
      (au:href %actor-table final-path))))

(defun parse-reference-path (reference)
  ;; TODO: ensure all valid forms of an actor path
  (case (elt (id reference) 0)
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
