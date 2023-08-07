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

(defun make-cache-domain (domain &optional layout)
  "Return a cache domain object for the DOMAIN using the LAYOUT which describes
a possibly nested hash table schema for this domain. The LAYOUT is a list of
test functions in which only EQ, EQL, EQUAL, and EQUALP are valid.  Later the
layout will be paired with a set of keys. If layout is NIL, it will default to
a single depth hash table with a default test of EQL."
  (let* ((layout (if layout layout `(,#'eql)))
         (cache (u:dict (car layout))))
    (%make-cache-domain :domain domain :layout layout :cache cache)))

(defun cdref (cd &rest keys)
  "Query the nested cache-domain CD at the index KEYS and return two values as
in GETHASH. The length of KEYS must be at least one, and equal to or less than
the length of the LAYOUT in the CD."
  (u:ensure-nested-hash-table (cache cd)
                              (layout cd)
                              keys)
  (multiple-value-bind (result present)
      (apply #'u:href (cache cd) keys)
    (if present
        (incf (hits cd))
        (incf (misses cd)))
    (values result present)))

(defun (setf cdref) (new-obj cd &rest keys)
  "Insert the NEW-OBJ at the index KEYS in the cache-domain CD. Return
NEW-OBJ. KEYS may be at least one in length, but not more than the length of
LAYOUT in the cache-domain."
  (u:ensure-nested-hash-table (cache cd)
                              (layout cd)
                              keys)
  (incf (inserts cd))
  (setf (apply #'u:href (cache cd) keys) new-obj))

(defun cdrem (cd &rest keys)
  "Remove the value in the cache-domain CD indexed by KEYS.
Return two values: the first value is the removed value or NIL if not present
and the second value is T or NIL if it was present in the cache-domain or not."
  (u:ensure-nested-hash-table (cache cd)
                              (layout cd)
                              keys)
  (loop :with table = (cache cd)
        :for (key . rest) :on keys
        :unless rest
          :return (progn (incf (removes cd))
                         (multiple-value-bind (value present)
                             (gethash key table)
                           (remhash key table)
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
               (format t "(cdref cd ~{~A ~})-> value: ~A, present: ~A~%"
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

(defun ensure-cache-domain (rc domain &optional layout)
  "If the cache-domain under the DOMAIN id exists in resource-cache RC, ignore
LAYOUT and do nothing. If the cache-domain does not exist, construct one with
the LAYOUT and insert it into RC. Return the cache-domain."
  (multiple-value-bind (cd present) (u:href (domains rc) domain)
    (if present
        cd
        (setf (u:href (domains rc) domain)
              (make-cache-domain domain layout)))))

(defun make-resource-cache (&optional domain-warmups)
  "Construct a RESOURCE-CACHE and return it. If DOMAIN-WARMUPS are supplied,
they will cause cache-domains to be created automatically with the layouts
provided. The format of DOMAIN-WARMUPS is a list consisting of none or more of
these forms:
  (domain-id)
  (domain-id (layout0))
  (domain-id (layout0 ... layoutN))
where domain-id is an EQUAL comparable object, and layout0 .. layoutN are one
of the functions EQ, EQL, EQUAL, EQUALP."
  (let ((rc (%make-resource-cache)))
    (loop :for (domain layout) :in domain-warmups
          :do (ensure-cache-domain rc domain layout))
    rc))

(defun rcref (rc domain &rest keys)
  (let ((cd (ensure-cache-domain rc domain)))
    (apply #'cdref cd keys)))

(defun (setf rcref) (new-obj rc domain &rest keys)
  (let ((cd (ensure-cache-domain rc domain)))
    (setf (apply #'cdref cd keys) new-obj)))

(defun rcrem (rc domain &rest keys)
  (let ((cd (ensure-cache-domain rc domain)))
    (apply #'cdrem cd keys)))

;; KEEP GOING: Implement rcremd (for domains). Implement a basic test for rc.

(defun rcremd (rc domain)
  (declare (ignore rc domain))
  nil)
