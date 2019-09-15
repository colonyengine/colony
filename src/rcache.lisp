(in-package #:virality.engine)

;;; Protocol methods

(defmethod rcache-layout (entry-type)
  '(eql))

(defmethod rcache-peek (context (entry-type symbol) &rest keys)
  (with-slots (%rcache) (core context)
    (ensure-nested-hash-table %rcache
                              ;; NOTE: 'eq is for the rcache table itself.
                              (list* 'eq (rcache-layout entry-type))
                              (list* entry-type keys))
    (apply #'u:href %rcache (list* entry-type keys))))

;; This might call rcache-construct if needed.
(defmethod rcache-lookup (context (entry-type symbol) &rest keys)
  (with-slots (%rcache) (core context)
    (ensure-nested-hash-table %rcache
                              ;; NOTE: 'eq is for the rcache table itself.
                              (list* 'eq (rcache-layout entry-type))
                              (list* entry-type keys))
    (multiple-value-bind (value presentp)
        (apply #'u:href %rcache (list* entry-type keys))
      (unless presentp
        (setf value (apply #'rcache-construct context entry-type keys)
              (apply #'u:href %rcache (list* entry-type keys)) value))
      value)))

;; This might call rcache-dispose if needed.
(defmethod rcache-remove (context (entry-type symbol) &rest keys)
  (with-slots (%rcache) (core context)
    (multiple-value-bind (value presentp)
        (apply #'u:href %rcache (list* entry-type keys))
      (when presentp
        (remhash (apply #'u:href %rcache (list* entry-type keys)) %rcache)
        (rcache-dispose context entry-type value)))))
