(in-package #:colony)

;; -------------------------------------------------------------------------
;; NOTE: The current resource-cache algorithm that we're getting rid of.
;; -------------------------------------------------------------------------

(defmethod resource-cache-layout (domain)
  ;; Only the 4 types are allowed: EQ EQL EQUAL EQUALP
  '(eql))

;; TODO: Should return multiple values of the value first, then T if in the
;; cache and returned, :queued if queueed but not materialized, and NIL if
;; not in the cache.
(defmethod resource-cache-peek (context (domain symbol) &rest keys)
  (with-slots (%old-resource-cache) (core context)
    ;; NOTE: 'eq is for the resource-cache table ;; itself.
    (u:ensure-nested-hash-table %old-resource-cache
                                (list* 'eq (resource-cache-layout domain))
                                (list* domain keys))
    (apply #'u:href %old-resource-cache (list* domain keys))))

(defmethod resource-cache-construct (context domain &rest keys)
  (declare (ignore context keys))
  (error "resource-cache-construct: Cannot construct unknown domain: ~A"
         domain))

;; This might call resource-cache-construct if needed.
;; TODO: Should return the same thing as peek.
(defmethod resource-cache-lookup (context (domain symbol) &rest keys)
  (with-slots (%old-resource-cache) (core context)
    ;; NOTE: 'eq is for the resource-cache table itself.
    (u:ensure-nested-hash-table %old-resource-cache
                                (list* 'eq (resource-cache-layout domain))
                                (list* domain keys))
    (multiple-value-bind (value found-p)
        (apply #'u:href %old-resource-cache (list* domain keys))
      (unless found-p
        (setf value (apply #'resource-cache-construct context domain keys)
              (apply #'u:href %old-resource-cache (list* domain keys)) value))
      value)))

(defmethod resource-cache-dispose (context domain removed-value)
  (declare (ignore context removed-value))
  (error "resource-cache-dispose: Cannot dispose unknown domain: ~A"
         domain))

;; This might call resource-cache-dispose if needed.
(defmethod resource-cache-remove (context (domain symbol) &rest keys)
  (with-slots (%old-resource-cache) (core context)
    (multiple-value-bind (value found-p)
        (apply #'u:href %old-resource-cache (list* domain keys))
      (when found-p
        (remhash (apply #'u:href %old-resource-cache (list* domain keys))
                 %old-resource-cache)
        (resource-cache-dispose context domain value)))))

;; -------------------------------------------------------------------------
;; TODO: Slowly replace the above with the below.
;; -------------------------------------------------------------------------

(in-package #:colony.resource-cache)

;; Implementation of base CACHE-ITEM.

(defun make-cache-item (&rest init-args)
  "Produce a base CACHE-ITEM and return it. The INIT-ARGS may contain keyword
value pairs for these slots:
 :opaque-data val - An arbitrary user specified data ignored by the
                    resource-cache API.
 :policy val - One of: :unlocked, or :locked. Describes if this entry can be
               evicted.
 :location val - One of: :cl-heap, :ffi-heap, :gpu-memory, :disk. The :disk
                 is used only for writing cached items to disk.
 :size val - Size in bytes of the associated value, if applicable.
 :value val - The actual representation of the cached item."
  (apply #'make-instance 'cache-item init-args))

;; Implementation of CACHE-DOMAIN

(defun make-cache-domain (domain-id &optional layout)
  "Return a cache domain object for the DOMAIN-ID using the LAYOUT which
describes a possibly nested hash table schema for this domain. The LAYOUT is a
list of test functions in which only EQ, EQL, EQUAL, and EQUALP are valid.
Later the layout will be paired with a set of keys. If layout is NIL, it will
default to a single depth hash table with a default test of EQL."
  (let* ((layout (if layout layout `(,#'eql)))
         (cache (u:dict (car layout))))
    (%make-cache-domain :did domain-id :layout layout :cache cache)))

(defun cdref (cache-domain &rest keys)
  "Query the nested CACHE-DOMAIN at the index KEYS and return two values as
in GETHASH. The length of KEYS must be at least one, and equal to or less than
the length of the LAYOUT in the CACHE-DOMAIN."
  (u:ensure-nested-hash-table (cache cache-domain)
                              (layout cache-domain)
                              keys)
  (multiple-value-bind (result present)
      (apply #'u:href (cache cache-domain) keys)
    (if present
        (incf (hits cache-domain))
        (incf (misses cache-domain)))
    (values result present)))

(defun (setf cdref) (new-obj cache-domain &rest keys)
  "Insert the NEW-OBJ at the index KEYS in the CACHE-DOMAIN. Return
NEW-OBJ. KEYS must be at least one in length, but not more than the length of
LAYOUT in the CACHE-DOMAIN."
  (u:ensure-nested-hash-table (cache cache-domain)
                              (layout cache-domain)
                              keys)
  (incf (inserts cache-domain))
  (setf (apply #'u:href (cache cache-domain) keys) new-obj))

(defun cdrem (cache-domain &rest keys)
  "Remove the value in the CACHE-DOMAIN indexed by KEYS.
Return two values: the first value is T or NIL if it was present in the
CACHE-DOMAIN or not. The second value is the removed value or NIL if not
present."
  (u:ensure-nested-hash-table (cache cache-domain)
                              (layout cache-domain)
                              keys)
  (loop :with table = (cache cache-domain)
        :for (key . rest) :on keys
        :unless rest
          :return (progn (incf (removes cache-domain))
                         (multiple-value-bind (value present)
                             (gethash key table)
                           (when present
                             (remhash key table))
                           ;; TODO: Maybe keep a stack of hash tables down to
                           ;; here, and then remove the ones which are empty in
                           ;; reverse order. Decide if cleaning up memory is
                           ;; more useful than live leaking it over time.
                           (values value present)))
        :do (setf table (gethash key table))))

;; Implementation of RESOURCE-CACHE

(defun ensure-cache-domain (resource-cache domain-id &optional layout)
  "If the cache-domain under the DOMAIN id exists in resource-cache RC, ignore
LAYOUT and do nothing. If the cache-domain does not exist, construct one with
the LAYOUT and insert it into RC. See MAKE-RESOURCE-CACHE for a description of
the LAYOUT format.Return the cache-domain."
  (u:ensure-gethash domain-id (domains resource-cache)
                    (make-cache-domain domain-id layout)))

(defun make-resource-cache (&optional domain-warmups)
  "Construct a RESOURCE-CACHE and return it. If DOMAIN-WARMUPS are supplied,
they will cause cache-domains to be created automatically with the layouts
provided. The format of DOMAIN-WARMUPS is a list consisting of none or more of
these forms:
  (domain-id)
  (domain-id (layout0))
  (domain-id (layout0 ... layoutN))
where domain-id is an EQUAL comparable object, and layout0 .. layoutN are one
of the functions EQ, EQL, EQUAL, EQUALP. If the LAYOUT is NIL, then the
default layout will be `(,#'EQL)."
  (let ((rc (%make-resource-cache)))
    (loop :for (domain-id layout) :in domain-warmups
          :do (ensure-cache-domain rc domain-id layout))
    rc))

(defun rcref (resource-cache domain-id &rest keys)
  "Lookup the value associated with the KEYS in the specified DOMAIN-ID.
Return two values: the first value is the value or nil if not present, the
second value is if the value is present in the specified domain. If the domain
doesn't exist, return (values nil nil)."
  (multiple-value-bind (cache-domain present)
      (gethash domain-id (domains resource-cache))
    (if present
        (apply #'cdref cache-domain keys)
        (values nil nil))))

(defun (setf rcref) (new-obj resource-cache domain-id &rest keys)
  "Ensure there is a cache-domain for the DOMAIN-ID and then setf the NEW-OBJ
at the index KEYS in that domain. NOTE: If the cache-domain doesn't exist, it
will be constructed with the default LAYOUT. See MAKE-RESOURCE-CACHE."
  (let ((cd (ensure-cache-domain resource-cache domain-id)))
    (setf (apply #'cdref cd keys) new-obj)))

(defun rcrem (resource-cache domain-id &rest keys)
  "Return two values: the first value is T or NIL of the value was removed or
not removed at the KEYS index in the cache-domain specified by DOMAIN-ID. The
second value is the value that was removed or NIL otherwise."
  (multiple-value-bind (cache-domain present)
      (gethash domain-id (domains resource-cache))
    (if present
        (apply #'cdrem cache-domain keys)
        (values nil nil))))

(defun rcrefd (resource-cache domain-id)
  "Return a reference to the cache-domain for DOMAIN-ID. Return two values:
the first value is the cache-domain object or NIL if not present, and the
second value is T if the cache-domain existed and NIL if not."
  (gethash domain-id (domains resource-cache)))

;; NOTE: There is no (setf rcrefd) function because you can either create the
;; cache-domain, or remove it, but it should not be changed once it is made.

(defun rcremd (resource-cache domain-id)
  "Remove the cache-domain DOMAIN from the RESOURCE-CACHE. Return two
values: The first value is T or NIL if the cache-domain existed or not, and
the second is the cache-domain object if it did exist or NIL otherwise."
  (multiple-value-bind (cache-domain present)
      (gethash domain-id (domains resource-cache))
    (when present
      (remhash domain-id (domains resource-cache)))
    (values present cache-domain)))




;;; --------------------------------------------------------------------------
;;; The cache warming (scheduler/executor) API
;;; --------------------------------------------------------------------------

;; --------------------------------------------------------------------------
;; The resource cache scheduling API
;; --------------------------------------------------------------------------

(defun make-resource-cache-scheduler (&rest init-args)
  (apply #'make-instance 'resource-cache-scheduler init-args))

;; Ultimately, this must lock the resource-cache-scheduler.
;; TODO: For now, only the main thread can call this.
;;
;; Return a list of all the caching-tasks out of order.
(defmethod schedule (resource-cache-scheduler &key &allow-other-keys)
  "Return an out of order list of all the available caching-tasks. The
caching-tasks are removed from the scheduler's ownership."
  (let ((scheduled-tasks nil)
        (unscheduled-tasks (unscheduled-tasks resource-cache-scheduler)))
    (u:do-hash-values (dht unscheduled-tasks)
      (u:do-hash-values (tasks dht)
        (u:do-hash-values (task tasks)
          (push task scheduled-tasks))
        (clrhash tasks)))
    scheduled-tasks))

;; TODO: Explain why the appdev might not want to mess with this much.
(defmethod submit (resource-cache-scheduler (caching-task caching-task))
  "Put a CACHING-TASK back into the unscheduled pool in the
RESOURCE-CACHE-SCHEDULER for scheduling again sometime in the future. Return
the CACHING-TASK."
  (let* ((task-type (class-name (class-of caching-task)))
         (domain-id (domain-id caching-task))
         (unscheduled-tasks (unscheduled-tasks resource-cache-scheduler))
         (ct caching-task)
         (ttt (u:ensure-gethash task-type
                                unscheduled-tasks
                                (make-hash-table)))
         (dht (u:ensure-gethash domain-id ttt (make-hash-table))))
    (u:ensure-gethash ct dht ct)))

;; TODO: Explain why the appdev might not want to mess with this much.
(defmethod revoke (resource-cache-scheduler (caching-task caching-task))
  ;; If the caching-task is in the scheduler, remove it. In both cases just
  ;; drop the reference to it from the resource-cache API's point of view and
  ;; let the GC collect it. Finalization or discarding should have cleaned up
  ;; any resource used by the caching-task.
  (let* ((task-type (class-name (class-of caching-task)))
         (ttt (u:ensure-gethash task-type
                                (unscheduled-tasks resource-cache-scheduler)
                                (make-hash-table)))
         (dht (u:ensure-gethash (domain-id caching-task) ttt
                                (make-hash-table))))
    (remhash caching-task dht)
    (values nil nil)))

;; -------------------------------------------------------------------------
;; The Cache Warming Protocol
;; -------------------------------------------------------------------------

(defmethod acquire-caching-task (resource-cache-scheduler task-type domain-id
                                 &rest init-args)
  "Allocate or reinitialize a pool instance of TASK-TYPE with INIT-ARGS, then
store into the RESOURCE-CACHE-SCHEDULER under the DOMAIN-ID category.
Return two values:
 The first value is the keyword :initialized.
 The second value is the caching-task (which is also book kept in the
   RESOURCE-CACHE-SCHEDULER so you can often ignore it)."

  ;; TODO: If recycling, use reinitialize-instance here after getting
  ;; an instance of the _exact_ task-type from the pool.
  (let* ((ct (apply #'make-instance task-type
                    :domain-id domain-id
                    :core (core resource-cache-scheduler)
                    init-args)))
    (values :initalized
            (submit resource-cache-scheduler ct))))

(defmethod consider-caching-task (caching-task resource-cache-scheduler)
  (declare (ignore caching-task resource-cache-scheduler))
  (error "This method must be specialized on caching-task."))

(defmethod compute-caching-task (caching-task resource-cache-scheduler)
  (declare (ignore caching-task resource-cache-scheduler))
  (error "This method mustbe specialized on caching-task."))

(defmethod finalize-caching-task (caching-task resource-cache-scheduler)
  (declare (ignore caching-task resource-cache-scheduler))
  (error "This method must be specialized on caching-task."))

(defmethod discard-caching-task (caching-task resource-cache-scheduler)
  (declare (ignore caching-task resource-cache-scheduler))
  (error "This method must be specialized on caching-task."))

(defmethod dispose-caching-task (caching-task resource-cache-scheduler)
  (declare (ignore caching-task resource-cache-scheduler))
  (error "This method must be specialized on caching-task."))

(defmethod release-caching-task (caching-task resource-cache-scheduler)
  (revoke resource-cache-scheduler caching-task))

;; --------------------------------------------------------------------------
;; The executor API.
;; --------------------------------------------------------------------------

(defun make-sequential-resource-cache-executor (&rest init-args)
  (apply #'make-instance 'sequential-resource-cache-executor
         init-args))

(defun make-concurrent-resource-cache-executor (&rest init-args)
  (apply #'make-instance 'concurrent-resource-cache-executor
         init-args))

(defun make-resource-cache-executor (kind &rest init-args)
  (ecase kind
    (:sequential
     (apply #'make-sequential-resource-cache-executor init-args))
    (:concurrent
     (apply #'make-concurrent-resource-cache-executor init-args))))

(defmethod execute (resource-cache-executor resource-cache-scheduler)
  (error "Unknown executor algorithm!"))

(defmethod execute ((resource-cache-executor
                     sequential-resource-cache-executor)
                    resource-cache-scheduler)
  (let ((total-processed 0))
    ;; Do one step of the state machine for each task, we're usually
    ;; going to submit when we reach the next state. Anything in the
    ;; finalized state gets released and doesn't submit anything.
    ;;
    ;; TODO: This is sketchy cause it doesn't enforce the correct edge
    ;; transitions. Add that.
    (loop :for tasks = (schedule resource-cache-scheduler)
          :while tasks
          :do (dolist (task tasks)
                (multiple-value-bind (transition-func valid-transitions)
                    (ecase (state task)
                      ((:initialized :retry-reservation)
                       (values #'consider-caching-task
                               '(:reserved :retry-reservation :discarded)))
                      ((:reserved :retry-computation)
                       (values #'compute-caching-task
                               '(:computed :retry-computation :discarded)))
                      ((:computed :retry-finalization)
                       (values #'finalize-caching-task
                               '(:finalized :retry-finalization :discarded)))
                      ((:discarded :retry-discarding)
                       (values #'discard-caching-task
                               '(:disposed :retry-discarding)))
                      ((:finalized :retry-disposing)
                       (values #'dispose-caching-task
                               '(:disposed :retry-disposing)))
                      (:disposed
                       (incf total-processed)
                       (values #'release-caching-task
                               '(nil))))
                  ;; TODO: Deal with VALUE better here.
                  (multiple-value-bind (next-state value)
                      (funcall transition-func task resource-cache-scheduler)
                    (declare (ignore value))
                    (unless (member next-state valid-transitions)
                      (error "execute: invalid-transition: ~S, expected: ~S"
                             next-state valid-transitions))
                    (setf (state task) next-state)
                    (when next-state ;; nil means it was released.
                      (submit resource-cache-scheduler task))))))

    ;; In case the body puts more tasks in, we catch it in this loop.
    total-processed))

;; TODO: It is for sure that the thread synchronization of this control path is
;; incorrect and we need to lock more things than we believe--both in the
;; queues to manage the work and also in accesses to the CORE or other things.
(defmethod execute ((resource-cache-executor
                     concurrent-resource-cache-executor)
                    resource-cache-scheduler)
  (error "Not implemented yet!")
  nil)
