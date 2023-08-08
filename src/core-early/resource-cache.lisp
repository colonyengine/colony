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

;; -------------------------------------------------------------------------
;; TODO: Slowly replace the above with the below.

(in-package #:virality.resource-cache)

;; Implementation of CACHE-DOMAIN

(defun make-cache-domain (domain-id &optional layout)
  "Return a cache domain object for the DOMAIN-ID using the LAYOUT which
describes a possibly nested hash table schema for this domain. The LAYOUT is a
list of test functions in which only EQ, EQL, EQUAL, and EQUALP are valid.
Later the layout will be paired with a set of keys. If layout is NIL, it will
default to a single depth hash table with a default test of EQL."
  (let* ((layout (if layout layout `(,#'eql)))
         (cache (u:dict (car layout))))
    (%make-cache-domain :domain-id domain-id :layout layout :cache cache)))

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


;; TODO: move to some unit test suite.
(defun cdtest ()
  (let ((key0 '(abc . 0))
        (key1a '(ijk . 0))
        (key1b '(xyz . 0))
        ;; TODO: Make the API cognizant if I try to use more keys than
        ;; possible. Should we error? Should we just extend using EQUAL by
        ;; default? Maybe put a flag in the cache-domain to let us do one or
        ;; the other by choice?
        (cd (make-cache-domain :texture (list #'equal #'equal))))
    (flet ((emit (cd &rest keys)
             (multiple-value-bind (value present)
                 (apply #'cdref cd keys)
               (format t "(cdref cd ~{~S ~})-> value: ~S, present: ~A~%"
                       keys value present))))

      (emit cd key0)
      (setf (cdref cd key0) "path/to/file.png")
      (emit cd key0)

      (emit cd key1a key1b)
      (setf (cdref cd key1a key1b) "another/path/to/file.png")
      (emit cd key1a key1b)
      (format t "-------------------~%")
      (emit cd key0)
      (emit cd key1a key1b)

      (cdrem cd key0)
      (emit cd key0)
      (cdrem cd key1a key1b)
      (emit cd key1a key1b)

      cd)))


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

(defun rcremd (resource-cache domain)
  "Remove the cache-domain DOMAIN from the RESOURCE-CACHE. Return two
values: The first value is T or NIL if the cache-domain existed or not, and
the second is the cache-domain object if it did exist or NIL otherwise."
  (multiple-value-bind (cache-domain present)
      (gethash domain (domains resource-cache))
    (when present
      (remhash domain (domains resource-cache)))
    (values present cache-domain)))


;; TODO: move over to a unit test.
(defun rctest ()
  (let ((rc (make-resource-cache `((:foo (,#'eql ,#'eql))))))
    (ensure-cache-domain rc :bar `(,#'equal))

    (flet ((emit (rc domain-id &rest keys)
             (multiple-value-bind (value present)
                 (apply #'rcref rc domain-id keys)
               (format t "(rcref rc ~S ~{~S ~})-> value: ~S, present: ~A~%"
                       domain-id keys value present))))

      (emit rc :foo :a 10)
      (setf (rcref rc :foo :a 10) "a/b/c")
      (emit rc :foo :a 10)
      (rcrem rc :foo :a 10)
      (emit rc :foo :a 10)

      (format t "--------------~%")

      (let ((key "/file/path"))
        (emit rc :bar key)
        (setf (rcref rc :bar key) 100)
        (emit rc :bar key)
        (rcrem rc :bar key)
        (emit rc :bar key))
      )))
