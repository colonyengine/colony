(in-package #:virality)

(defmethod resource-cache-layout (domain)
  ;; Only the 4 types are allowed: EQ EQL EQUAL EQUALP
  '(eql))

;; TODO: Should return multiple values of the value first, then T if in the
;; cache and returned, :queued if queueed but not materialized, and NIL if
;; not in the cache.
(defmethod resource-cache-peek (context (domain symbol) &rest keys)
  (with-slots (%resource-cache) (core context)
    ;; NOTE: 'eq is for the resource-cache table ;; itself.
    (u:ensure-nested-hash-table %resource-cache
                                (list* 'eq (resource-cache-layout domain))
                                (list* domain keys))
    (apply #'u:href %resource-cache (list* domain keys))))

(defmethod resource-cache-construct (context domain &rest keys)
  (declare (ignore context keys))
  (error "resource-cache-construct: Cannot construct unknown domain: ~A"
         domain))

;; This might call resource-cache-construct if needed.
;; TODO: Should return the same thing as peek.
(defmethod resource-cache-lookup (context (domain symbol) &rest keys)
  (with-slots (%resource-cache) (core context)
    ;; NOTE: 'eq is for the resource-cache table itself.
    (u:ensure-nested-hash-table %resource-cache
                                (list* 'eq (resource-cache-layout domain))
                                (list* domain keys))
    (multiple-value-bind (value found-p)
        (apply #'u:href %resource-cache (list* domain keys))
      (unless found-p
        (setf value (apply #'resource-cache-construct context domain keys)
              (apply #'u:href %resource-cache (list* domain keys)) value))
      value)))

(defmethod resource-cache-dispose (context domain removed-value)
  (declare (ignore context removed-value))
  (error "resource-cache-dispose: Cannot dispose unknown domain: ~A"
         domain))

;; This might call resource-cache-dispose if needed.
(defmethod resource-cache-remove (context (domain symbol) &rest keys)
  (with-slots (%resource-cache) (core context)
    (multiple-value-bind (value found-p)
        (apply #'u:href %resource-cache (list* domain keys))
      (when found-p
        (remhash (apply #'u:href %resource-cache (list* domain keys))
                 %resource-cache)
        (resource-cache-dispose context domain value)))))
