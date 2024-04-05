(in-package #:colony.clone)

;;; -------------------------------
;; The INTENTION API
;;; -------------------------------

;; We choose to currently represent the sorting of the INTENTION types with
;; a simple internal field that helps us prevent a combinatorial explosion
;; with the types and methods. We can choose to specialize on the actual
;; types if we desire too.
(defmethod compare-intention (intention-left intention-right)
  (let ((left-sort-id (sort-id intention-left))
        (right-sort-id (sort-id intention-right)))
    (cond
      ((< left-sort-id right-sort-id) -1)
      ((= left-sort-id right-sort-id) 0)
      (t 1))))

(defun intention< (intention-left intention-right)
  (let ((value (compare-intention intention-left intention-right)))
    (= value -1)))

(defun intention<= (intention-left intention-right)
  (let ((value (compare-intention intention-left intention-right)))
    (or (= value -1) (= value 0))))

(defun intention= (intention-left intention-right)
  (let ((value (compare-intention intention-left intention-right)))
    (= value 0)))

(defun intention>= (intention-left intention-right)
  (let ((value (compare-intention intention-left intention-right)))
    (or (= value 0) (= value 1))))

(defun intention> (intention-left intention-right)
  (let ((value (compare-intention intention-left intention-right)))
    (= value 1)))

(defun intention/= (intention-left intention-right)
  (let ((value (compare-intention intention-left intention-right)))
    (/= value 0)))

;;;; Creation of INTENTIONs
;;;; NOTE: The pedantic versions are completely not implemented yet.
(defun make-no-specific-intention ()
  (make-instance 'no-specific-intention))

(defun make-cons-intention ()
  (make-instance 'cons-intention))

(defun make-list-intention ()
  (make-instance 'list-intention))

(defun make-alist-intention ()
  (make-instance 'alist-intention))

(defun make-graph-intention ()
  (make-instance 'graph-intention))

;;; -------------------------------
;; The EQL-MAP API
;;; -------------------------------

(defun make-eql-map-entry (&rest args)
  (apply #'make-instance 'eql-map-entry args))

(defun make-eql-map (&key stats-move-original
                       stats-move-clone
                       stats-allocation
                       stats-array-clone-speed-fast
                       stats-array-clone-speed-slow)
  (let* (;; Disjunction to let us know how to enable any stats.
         (enable-stats (or stats-move-original
                           stats-move-clone
                           stats-allocation
                           stats-array-clone-speed-fast
                           stats-array-clone-speed-slow))
         (stats-table (when enable-stats (u:dict #'eq))))
    ;; initialize each of the requested domains...
    (when stats-table
      (when stats-move-original
        ;; a table to keep track of moved references/values from original
        ;; data source into a cloned source.
        ;; This is the :move-original domain.
        ;; Key: type-of form
        ;; Value: integer count of copies
        (setf (u:href stats-table :move-original) (u:dict #'equal)))
      (when stats-move-clone
        ;; a table to keep track of moved references/values from cloned
        ;; objects to other cloned objects via setf.
        ;; This is the :move-cloned domain.
        ;; Key: type-of form
        ;; Value: integer count of copies
        (setf (u:href stats-table :move-clone) (u:dict #'equal)))
      (when stats-allocation
        ;; a table to keep track of actual memory allocations.
        ;; This is the :allocation domain.
        ;; Key: type-of form
        ;; Value: integer count of allocations
        (setf (u:href stats-table :allocation) (u:dict #'equal)))
      (when stats-array-clone-speed-fast
        ;; a table to keep track of arrays we cloned in fast mode.
        ;; This is the :array-clone-speed-fast domain.
        ;; Key: type-of form
        ;; Value: integer count of fast copies
        (setf (u:href stats-table :array-clone-speed-fast) (u:dict #'equal)))
      (when stats-array-clone-speed-slow
        ;; a table to keep track of arrays we cloned in slow mode.
        ;; This is the :array-clone-speed-slow domain.
        ;; Key: type-of form
        ;; Value: integer count of fast copies
        (setf (u:href stats-table :array-clone-speed-slow) (u:dict #'equal)))
      )
    (make-instance 'eql-map :stats stats-table)))

(defun make-eql-map-with-stats ()
  (make-eql-map :stats-move-original t
                :stats-move-clone t
                :stats-allocation t
                :stats-array-clone-speed-fast t
                :stats-array-clone-speed-slow t))


(defun eql-map-ref (eql-map original-object)
  "Return the EQL-MAP-ENTRY assocated with the ORIGINAL-OBJECT in EQL-MAP
or NIL if not present."
  (u:href (entry-table eql-map) original-object))

(defun (setf eql-map-ref) (eql-map-entry eql-map original-object)
  "Associate the EQL-MAP-ENTRY instance with the ORIGINAL-OBJECT in the EQL-MAP
table. Return the EQL-MAP-ENTRY."
  (setf (u:href (entry-table eql-map) original-object) eql-map-entry))

(defun eql-map-visited-p (eql-map object)
  "Return NIL if not visited or the EQL-MAP-ENTRY associated with the OBJECT if
present (which means it had been visited)."
  (eql-map-ref eql-map object))

(defun eql-map-mark-visited (eql-map object)
  "Return the EQL-MAP-ENTRY associated with OBJECT in the EQL-MAP table. If
there is no EQL-MAP-ENTRY, create one and store it into the EQL-MAP and then
return it."
  (or (eql-map-ref eql-map object)
      (setf (eql-map-ref eql-map object)
            (make-eql-map-entry :origin object))))

(defun eql-map-mark-target (eql-map original-object cloned-object intent)
  "Visit the ORIGINAL-OBJECT (if not already visited), the mark the transition
as present and store the CLONED-OBJECT as the TARGET and store the INTENT into
the EQL-MAP-ENTRY.  Return the CLONED-OBJECT."
  (let ((eql-map-entry (eql-map-mark-visited eql-map original-object)))
    (setf (transition-p eql-map-entry) t
          (target eql-map-entry) cloned-object
          (intent eql-map-entry) intent)
    cloned-object))

(defun eql-map-record (eql-map cloned-object domain &key (event 1)
                                                      (type-of-p t))
  "Record into the EQL-MAP that a statistics EVENT occurred for the
CLONED-OBJECT in the specified DOMAIN (one of: :move, :allocation). The EVENT
can be arbitrary and handled by the DOMAIN processing it, but it is often an
increment of the number of times the event happened in that domain, so it
defaults to 1. If :TYPE-OF-P is true (the default), then compute the TYPE-OF
the CLONED-OBJECT for the statistics, if nil then use CLONED-OBJECT directly.

The event has this meaning for each domain:

 :move-original
   EVENT is an integer representing how many moves of originally encountered
   data happened.
 :move-clone
   EVENT is an integer representing how many moves of cloned data to other
   cloned data happened.
 :allocation
   EVENT is an integer representing how many allocations occurred."

  (flet ((get-type-of (obj)
           (if type-of-p
               (type-of obj)
               obj)))
    (u:when-let ((stats (stats eql-map)))
      (ecase domain
        ((:allocation :move-original :move-clone :array-clone-speed-fast
          :array-clone-speed-slow)
         ;; These domains keep data the exact same way.
         (u:when-let ((alloc-table (u:href stats domain)))
           (let ((key (get-type-of cloned-object)))
             (setf (gethash key alloc-table)
                   (+ (gethash key alloc-table 0) event)))))))))

(defun eql-map-get-stats-domain (eql-map domain)
  (u:when-let ((tbl (stats eql-map)))
    (u:href tbl domain)))

(defun eql-map-stats-match-p (eql-map &rest stat-specs)
  "Check that the statistics described in the STAT-SPECS rest list for the
specified domains are exactly as found in the statistics table of the EQL-MAP
instance.

Return four values (the last two are NIL if value 0 is T):
  Value 0: T if the stats matched and NIL otherwise.
  Value 1: A keyword indicating the reason for the first value.
           Legal Values are:
            :ok -  The stats all matched.
            :error-stats-found -  Unexpected stats found in a domain.
            :error-empty-domain - An unexpectedly empty domain.
            :error-wrong-stats - Number of stats don't match for a domain..
            :error-missing-stat - An expected stat is not present in domain.
            :error-wrong-stat - Required stat has the wrong value in domain.
  Value 2: The first domain in which the problem was discovered.
  Value 3: A fully formed error string describing the problem.

The STAT-SPECS can be NIL or be a list of STAT-SPEC forms. The STAT-SPEC format
may be one of these:

 ;; Move is when a reference or object (like a fixnum) is SETFed in a plain way
 ;; somewhere else.
 (:move (type-name-0 number-0) ... (type-name-N number-N))

 ;; Allocation is when an object is copied via memory allocation.
 (:allocation (type-name-0 number-0) ... (type-name-N number-N))

The first entry means that in domain-0 the type-name-0 key must have a
statistic of exactly number-0 and so on. Any keys found in domain-0 that are
not specified in the STAT-SPEC will cause an assertion to fail. If the same
type-name-0 occurs in multiple pairs for the same domain, it is coalesced into
a single pair with the summation of the numbers automatically before processing
it.

STAT-SPEC forms that look like this:

 (:move)
 (:allocation)

indicate that that if the domain exists, there must be no statistic entries in
it. If there are more or less keys in the domain than specified then fail."

  (labels ((signal-error (error-code domain message-fmt &rest args)
             (return-from eql-map-stats-match-p
               (values nil
                       error-code
                       domain
                       (apply #'format nil message-fmt args))))
           (error-stats-found (domain)
             (signal-error
              :error-stats-found
              domain
              "No stats expected for domain ~S, but some found!" domain))
           (error-empty-domain (domain)
             (signal-error
              :error-empty-domain
              domain
              "Required statistics for domain ~S, but none found!" domain))
           (error-wrong-stats (domain expected-stats found-stats)
             (signal-error
              :error-wrong-stats
              domain
              "Required ~A statistics for domain ~S, but instead found ~A!"
              expected-stats domain found-stats))
           (error-missing-stat (domain key)
             (signal-error
              :error-missing-stat
              domain
              "Cannot find statistic key ~S in domain ~S!" key domain))
           (error-wrong-stat (domain stat-name expected-value actual-value)
             (signal-error
              :error-wrong-stat
              domain
              "In domain ~S, expected (~S ~A) : found (~S ~A)"
              domain stat-name expected-value stat-name actual-value))

           ;; NOTE: A helper to make human specification of the tests easier.
           ;;
           ;; The stat-specs might look like this:
           ;; ((:move (cons 1) (cons 2))
           ;;  (:allocation (hash-table 1) (cons 3)))
           ;; and function coalesced ALL spec inside of a list of stat-specs
           ;; into a form like:
           ;; ((:move (cons 3))
           ;;  (:allocation (hash-table 1) (cons 3)))
           (coalesce-stat-specs (original-stat-specs)
             (let ((coalesced-stat-specs nil)
                   (stat-tbl (u:dict #'equal)))
               (dolist (spec original-stat-specs)
                 (destructuring-bind (domain . statistics) spec
                   ;; sum all of the nums for each type-name
                   (loop :for (tname num) :in statistics
                         :do (u:ensure-gethash tname stat-tbl 0)
                             (incf (u:href stat-tbl tname) num))
                   ;; rebuild the newly coalesced spec and store it off
                   (push (cons domain
                               (let ((new-stats nil))
                                 (u:do-hash (tname num stat-tbl)
                                   (push (list tname num) new-stats))
                                 (nreverse new-stats)))
                         coalesced-stat-specs)
                   ;; Get ready for next collection of statistics in the next
                   ;; domain we process.
                   (clrhash stat-tbl)))
               coalesced-stat-specs)))

    (let ((processed-domains (u:dict 'eq)))
      ;; First, coalesce the statistics for each domain in the stat-specs
      ;; into a new stat-specs form for each domain where each state is
      ;; specified only once.
      (let ((coalesced-stat-specs (coalesce-stat-specs stat-specs)))
        ;; Then, process the coalesced version of stat-specs.
        (dolist (stat-spec coalesced-stat-specs)
          (destructuring-bind (domain . statistics) stat-spec
            (ecase domain
              ((:move-original :move-clone :allocation :array-clone-speed-fast
                :array-clone-speed-slow)
               ;; Record the fact we processed this domain
               (setf (u:href processed-domains domain) t)

               (let ((tbl (eql-map-get-stats-domain eql-map domain)))
                 (cond
                   ((null statistics)
                    ;; Ensure there were no stats kept for this domain.
                    (unless (or (null tbl)
                                (zerop (hash-table-count tbl)))
                      (error-stats-found domain)))
                   (t
                    ;; Ensure we have a table....
                    (unless tbl
                      (error-empty-domain domain))
                    ;; ... and the right number of keys
                    (let ((expected-stats (length statistics))
                          (found-stats (hash-table-count tbl)))
                      (unless (= expected-stats found-stats)
                        (error-wrong-stats domain expected-stats found-stats)))
                    ;; ...and now check that the keys are what we expect!
                    (dolist (stat statistics)
                      (destructuring-bind (type-name expected-num) stat
                        (u:mvlet ((stat-value presentp (u:href tbl type-name)))
                          (if presentp
                              (unless (eql stat-value expected-num)
                                ;; oops stat didn't match!
                                (error-wrong-stat domain type-name
                                                  expected-num stat-value))
                              ;; ...oops didn't find expected stat!
                              (error-missing-stat domain type-name)))))))))))))

      ;; Check to see if there are any non empty domains we didn't process.
      ;; If so, that's an error.
      (u:do-hash-keys (domain-key (stats eql-map))
        (unless (u:href processed-domains domain-key)
          (ecase domain-key
            ((:move-original :move-clone :allocation :array-clone-speed-fast
              :array-clone-speed-slow)
             (unless
                 (zerop (hash-table-count(u:href (stats eql-map) domain-key)))
               (error-stats-found domain-key)))))))
    (values t :ok)))

;; TODO: This is very terrible, only for debugging purposes at this time.
(defun eql-map-dump (eql-map &optional (strm t))
  (flet ((safe-slot-value (obj slot-name slot-reader)
           (if (slot-boundp obj slot-name)
               (funcall slot-reader obj)
               "<UBND>")))
    (format strm "eql-map with ~A entries:~%"
            (hash-table-count (entry-table eql-map)))
    (maphash
     (lambda (original-object eql-map-entry)
       (format strm " k:")
       (print-unreadable-object (original-object strm :type t :identity t)
         (format strm "~S" original-object))
       (format strm "~%  c:")
       (print-unreadable-object (eql-map-entry strm :type t :identity t)
         (format strm "~%    o:  ~S~%    tp: ~S~%    t:  ~S~%    i:  ~S"
                 (safe-slot-value eql-map-entry '%origin #'origin)
                 (safe-slot-value eql-map-entry '%transition-p #'transition-p)
                 (safe-slot-value eql-map-entry '%target #'target)
                 (safe-slot-value eql-map-entry '%intent #'intent)))
       (format strm "~%"))
     (entry-table eql-map))))

(defun eql-map-dump-stats (eql-map)
  (unless (stats eql-map)
    (format t "EQL-MAP: No stats available.")
    (return-from eql-map-dump-stats nil))

  (format t "EQL-MAP statistics:~%")
  (format t " Visited Allocatable Nodes: ~A~%"
          (hash-table-count (entry-table eql-map)))

  ;; All of these domains can be printed out in the same way.
  (dolist (domain '(:move-original :move-clone :allocation
                    :array-clone-speed-fast :array-clone-speed-slow))
    (u:when-let ((tbl (eql-map-get-stats-domain eql-map domain)))
      (format t " Domain ~(~S~)~%" domain)
      (if (plusp (hash-table-count tbl))
          (u:do-hash (type-name num tbl)
            (format t "  ~8@A ~S~%" num type-name))
          (format t "  ~8@A~%" "NONE")))))

;; Helper macro for the eql-map statistics tests.
(defmacro validate-eql-map-stats (match-form)
  (u:with-gensyms (matchedp reason domain msg)
    `(u:mvlet* ((,matchedp ,reason ,domain ,msg
                           ,match-form))
       (unless ,matchedp
         (error "eql-map matching failed:~% reason: ~S~% domain: ~S~% msg: ~S"
                ,reason ,domain ,msg))
       t)))

;;;; Creation of CLONE-POLICIES
(defun make-identity-clone ()
  (make-instance 'identity-clone))

(defun make-shallow-clone ()
  (make-instance 'shallow-clone))

(defun make-deep-clone ()
  (make-instance 'deep-clone))

;; These are NOT to be exported. They basically represent an easy way to not
;; have memory churn in heavy copy situations.
(defparameter *identity* (make-identity-clone))
(defparameter *shallow* (make-shallow-clone))
(defparameter *deep* (make-deep-clone))

;; These are NOT to be exported.
(defparameter *no-specific-intention* (make-no-specific-intention))
(defparameter *cons-intention* (make-cons-intention))
(defparameter *list-intention* (make-list-intention))
(defparameter *alist-intention* (make-alist-intention))
(defparameter *graph-intention* (make-graph-intention))

;; Shortcut API for very common cloning policies.
(defun clone-identity (object &optional (eql-map nil eql-map-supp-p))
  "Perform an identity clone of the OBJECT. The clone is a nop and the OBJECT
is returned uncloned."
  (clone object *identity* *no-specific-intention*
         (if eql-map-supp-p eql-map (make-eql-map))))

(defun clone-shallow (object &optional (eql-map nil eql-map-supp-p))
  "A nickname for CLONE-SHALLOW-LIST. See that function."
  (clone-shallow-list object (if eql-map-supp-p eql-map (make-eql-map))))

(defun clone-shallow-cons (object &optional (eql-map nil eql-map-supp-p))
  "Perform a shallow clone of the OBJECT and return a copy. Note that lists (or
other structures built with cons cells only have their very first cons cell
shallow copied and that's it!"
  (clone object *shallow* *cons-intention*
         (if eql-map-supp-p eql-map (make-eql-map))))

(defun clone-shallow-list (object &optional (eql-map nil eql-map-supp-p))
  "Perform a shallow clone of the OBJECT and return a copy. Note that lists
only have their toplevel cons cells shallow copied!"
  (clone object *shallow* *list-intention*
         (if eql-map-supp-p eql-map (make-eql-map))))

(defun clone-shallow-alist (object &optional (eql-map nil eql-map-supp-p))
  "Perform a shallow clone of the OBJECT and return a copy. Note that lists are
treated as alists and only the alist structure is shallow copied!"
  (clone object *shallow* *alist-intention*
         (if eql-map-supp-p eql-map (make-eql-map))))

(defun clone-shallow-graph (object &optional (eql-map nil eql-map-supp-p))
  "Perform a shallow clone of the OBJECT and return a copy. Note that lists are
treated as trees and only the entire tree structure is shallow copied which
will produce a new tree whose non-cons car values are simply copied!"
  (clone object *shallow* *graph-intention*
         (if eql-map-supp-p eql-map (make-eql-map))))

(defun clone-deep (object &optional (eql-map nil eql-map-supp-p))
  "Perform a deep clone of the OBJECT using the GRAPH-INTENTION and return a
copy."
  (clone object *deep* *graph-intention*
         (if eql-map-supp-p eql-map (make-eql-map))))

;;; Helper functions
(defun fast-copyable-array-p (obj)
  "Return T if the array can be qopied with REPLACE because all of the elements
are of the same known value type. Return NIL otherwise."
  (let ((element-type (array-element-type obj)))
    (some (u:curry #'equal element-type)
          ;; Check in order of most common occurrance.
          ;; Try to make this faster...
          (list 'single-float
                '(unsigned-byte 8)
                '(unsigned-byte 32)
                '(unsigned-byte 64)
                '(unsigned-byte 16)
                '(signed-byte 8)
                '(signed-byte 32)
                '(signed-byte 64)
                '(signed-byte 16)
                'double-float
                'fixnum
                'bit))))

;;; The CLONE API and CLONE-OBJECT API methods.  CLONE is the entry point to
;;; clone an object and must allocate the memory for the new object, if
;;; appropriate, it wll then call CLONE-OBJECT to complete the cloning process.
;;
;;; NOTE: In most places where it is intended to so an identity clone, I've not
;;; actually written the CLONE call and instead just used the raw value.  This
;;; is because deep cloning things like large single-float arrays would be
;;; prohibitively expensive.
;;;
;;; NOTE: We also only store in eql-map the graph of allocatable things and how
;;; they reference each other. Value-like things which are not allocatable
;;; like symbols, functions, numbers, etc, are not represented in the eql-map
;;; graph. This is to present large slowdown and huge memory use when cloning
;;; large arrays of single-floats and similar.

;;; -------------------------------
;; catch all for clone-object stuff we haven't implemented.
;; prolly need to rewrite this to be a no-applicable-method method because the
;; progn dispatch causes this one to be legitmately called first.
;;; -------------------------------
(defmethod no-applicable-method ((mthd (eql #'clone-object)) &rest args)
  (destructuring-bind (cloned-object
                       original-object
                       policy
                       intention
                       last-known-intention
                       eql-map
                       . key-args)
      args

    (let* ((c-o-str (format nil "cloned-object[type: ~S]: ~S"
                            (type-of cloned-object) cloned-object))
           (o-o-str (format nil "original-object[type: ~S]: ~S"
                            (type-of original-object) original-object))
           (pol-str (format nil "policy: ~A" policy))
           (int-str (format nil "intention: ~A" intention))
           (lki-str (format nil "last-known-intention: ~A"
                            last-known-intention))
           (map-str (with-output-to-string (s)
                      (eql-map-dump eql-map s)))
           (key-str (format nil "keyargs: ~{~S ~}" key-args))
           (output-str (format nil (concatenate
                                    'string
                                    "No applicable method error for:~%"
                                    "  ~A~%~%"
                                    "It is unknown how to clone:~%"
                                    " ~A~%"
                                    " ~A~%"
                                    " ~A~%"
                                    " ~A~%"
                                    " ~A~%"
                                    " ~A~%"
                                    " ~A~%")
                               mthd
                               c-o-str
                               o-o-str
                               pol-str
                               int-str
                               lki-str
                               key-str
                               map-str)))
      (error output-str))))

(defmethod no-applicable-method ((mthd (eql #'clone)) &rest args)
  (destructuring-bind (object
                       policy
                       intention
                       eql-map
                       . key-args)
      args

    (let* ((c-o-str (format nil "object[type: ~S]: ~S"
                            (type-of object) object))
           (pol-str (format nil "policy: ~A" policy))
           (int-str (format nil "intention: ~A" intention))
           (map-str (with-output-to-string (s)
                      (eql-map-dump eql-map s)))
           (key-str (format nil "keyargs: ~{~S ~}" key-args))
           (output-str
             (format nil
                     (concatenate
                      'string
                      "No applicable method error for:~%"
                      "  ~A~%~%"
                      "It is unknown how to clone:~%"
                      " ~A~%"
                      " ~A~%"
                      " ~A~%"
                      " ~A~%"
                      " ~A~%"
                      "Concerning the type: ~S~%"
                      "If you encountered this expecting an IDENTITY~%"
                      "clone, then you'll have to write a~%"
                      "CLONE method specializing on this object type and~%"
                      "IDENTITY-CLONE.~%"
                      "If you were expecting to perform a SHALLOW or DEEP~%"
                      "clone, then you'll ALSO HAVE TO add methods for~%"
                      "ALLOCATABLEP, CLONE-ALLOCATE, and CLONE-OBJECT~%"
                      "that each specialize on this object type and the~%"
                      "appropriate cloning policy.~%")
                     mthd
                     c-o-str
                     pol-str
                     int-str
                     key-str
                     map-str
                     (type-of object))))
      (error output-str))))


(defmethod allocatablep (object)
  "The default response to any class of OBJECT is that it is not allocatable.
Various classes must be whitelisted into the cloning system."
  nil)

(defmethod allocatablep ((object cons))
  t)

(defmethod allocatablep ((object array))
  t)

(defmethod allocatablep ((object hash-table))
  t)


;;; -------------------------------
;; Cloning low level value-like non-collection-like things.
;;
;; The default IDENTITY-CLONE policy for any object is to insert itself into
;; the EQL-MAP with itself as a transition and return exactly itself with no
;; new memory allocation of the object and no copying of information and no
;; recursive copying.
;;
;; This is the base cloning method for "by value"-like things such as:
;; Symbols
;; Characters
;; Functions/Closures
;; Numbers,
;; Pathnames,
;; and other atomic things which are not actually a collections.
;;; -------------------------------

(defun %clone-identity (object policy intention eql-map &key)
  (declare (ignore policy))
  ;; Anything not allocatable is automatically returned and the most minor
  ;; statistics kept.
  (unless (allocatablep object)
    (eql-map-record eql-map object :move-original)
    (return-from %clone-identity (values object eql-map)))

  ;; For anything allocatable, we make a mark in the eql-map over it to keep
  ;; better statistics about it.
  (let ((eql-map-entry (eql-map-mark-visited eql-map object)))
    (values
     (cond
       ((transition-p eql-map-entry)
        (eql-map-record eql-map (target eql-map-entry) :move-clone)
        (target eql-map-entry))
       (t
        (eql-map-record eql-map object :move-original)
        (eql-map-mark-target eql-map object object intention)))
     eql-map)))

;; NOTE: Explicitly whitelist what we identity clone. The case we're attempting
;; to catch is that if a user of this library uses a type no one has seen
;; before, we shouldn't silently identity-clone it. It is almost always the
;; case they are attempting to deep clone it and will be very surprised if it
;; just identity cloned magically.
(defmethod clone ((object function) (policy identity-clone) intention eql-map
                  &key)
  (%clone-identity object policy intention eql-map))

(defmethod clone ((object character) (policy identity-clone) intention eql-map
                  &key)
  (%clone-identity object policy intention eql-map))

(defmethod clone ((object pathname) (policy identity-clone) intention eql-map
                  &key)
  (%clone-identity object policy intention eql-map))

(defmethod clone ((object symbol) (policy identity-clone) intention eql-map
                  &key)
  (%clone-identity object policy intention eql-map))

(defmethod clone ((object number) (policy identity-clone) intention eql-map
                  &key)
  (%clone-identity object policy intention eql-map))

(defmethod clone ((object cons) (policy identity-clone) intention eql-map
                  &key)
  (%clone-identity object policy intention eql-map))

(defmethod clone ((object array) (policy identity-clone) intention eql-map
                  &key)
  (%clone-identity object policy intention eql-map))

(defmethod clone ((object hash-table) (policy identity-clone) intention eql-map
                  &key)
  (%clone-identity object policy intention eql-map))

;;; -------------------------------
;; Cloning cons/collection/array/etc like things which require actually new
;; memory allocation to perform the clone. The work of the memory allocation is
;; offloaded to CLONE-ALLOCATE. Return a clone of the OBJECT.  NOTE: We almost
;; never specialize on the object and the allocating-clone policy. We only do
;; it (if ever) in special circumstances).
;;; -------------------------------
(defmethod clone (object (policy allocating-clone) intention eql-map
                  &key)

  ;; Anything not alloctable is passed to the whitelisted identity cloner.
  (unless (allocatablep object)
    (return-from clone
      (clone object *identity* intention eql-map)))

  ;; Otherwise it'll get maybe duplicated and visited in the eql-map graph.

  ;; Note: we may have already visited it once in some other control path.
  (let ((eql-map-entry (eql-map-mark-visited eql-map object)))
    (if (transition-p eql-map-entry)
        ;; We already have a transition...
        (if (intention= intention (intent eql-map-entry))
            ;; then good to go, we can just reuse what we have!
            (progn
              (let ((the-value (target eql-map-entry)))
                ;; NOTE: The first time we likely recorded the cloning of the
                ;; target into SOME domain, but subsequent times will just be
                ;; moves since we aren't going to (potentially) allocate it
                ;; again. This might be a little confusing to people looking at
                ;; the stats since a (clone-deep (list #1=(cons 1 2) #1# #1#
                ;; #1#) will have 4 CONS allocations for the list structure
                ;; plus 1 CONS allocation for the first element, and then 3
                ;; CONS moves (among other minor stats) for reusing the same
                ;; reference in the later list structure.
                (eql-map-record eql-map the-value :move-clone)
                (values the-value eql-map)))
            ;; else we don't allocate any memory, but allow a "redo" of the
            ;; cloning of the data by picking the appropriate clone-object.
            (let ((last-intention (intent eql-map-entry)))
              ;; Fixup the intention before calling clone-object in case we
              ;; encounter this object again in some further recursion from the
              ;; clone-object we are about to call.
              ;;
              ;; NOTE: This is tricky. We only change the stored intention of
              ;; the new intention is "more complex". Otherwise, we leave it as
              ;; we found it and the clone-object can do whatever work is
              ;; necessary given the intention difference.
              (when (intention> intention last-intention)
                (setf (intent eql-map-entry) intention))
              (values (clone-object (target eql-map-entry) object policy
                                    intention last-intention eql-map)
                      eql-map)))
        ;; else formally and newly allocate it and clone the contents.
        (let ((cloned-object (clone-allocate object eql-map)))
          (eql-map-mark-target eql-map object cloned-object intention)
          (values (clone-object cloned-object object policy intention
                                (make-no-specific-intention) eql-map)
                  eql-map)))))

;;; ---------------------------------------------------------------------------
;; Allocating different objects section
;;; ---------------------------------------------------------------------------

;; Everything in the clone system beyond the types supported in this file must
;; be whitelisted.
(defmethod clone-allocate (object (eql-map eql-map))
  (let ((object-type (type-of object)))
    (error "clone-allocate: No allocation decision for object of type ~S.~%"
           object-type)))

;; Allocating a function just returns itself
(defmethod clone-allocate ((object function) (eql-map eql-map))
  (eql-map-record eql-map object :move-original)
  object)

;; Allocating a character just returns itself
(defmethod clone-allocate ((object character) (eql-map eql-map))
  (eql-map-record eql-map object :move-original)
  object)

;; Allocating a pathname (for now) just returns itself
(defmethod clone-allocate ((object pathname) (eql-map eql-map))
  (eql-map-record eql-map object :move-original)
  object)

;; Allocating a symbol just returns itself
(defmethod clone-allocate ((object symbol) (eql-map eql-map))
  (eql-map-record eql-map object :move-original)
  object)

;; Allocating a number just returns itself (catches the numeric tower).
(defmethod clone-allocate ((object number) (eql-map eql-map))
  (eql-map-record eql-map object :move-original)
  object)

;; How to allocate a new cons cell.
(defmethod clone-allocate ((object cons) (eql-map eql-map))
  (eql-map-record eql-map object :allocation)
  (cons nil nil))

;; How to allocate a new array (that copies the properties of the original).
(defmethod clone-allocate ((object array) (eql-map eql-map))
  ;; TODO: This doesn't handle displaced arrays.
  (multiple-value-bind (displaced-to displaced-index-offset)
      (array-displacement object)
    (unless (and (null displaced-to)
                 (eql  displaced-index-offset 0))
      (error "Cloning displaced arrays is not yet supported.")))

  (eql-map-record eql-map object :allocation)
  (make-array (array-dimensions object)
              :element-type (array-element-type object)
              :adjustable (adjustable-array-p object)
              :fill-pointer (when (array-has-fill-pointer-p object)
                              (fill-pointer object))))


;; How to allocate a new hash table (that copies the properties of the
;; original).
(defmethod clone-allocate ((object hash-table) (eql-map eql-map))
  ;; TODO: SBCL (and likely others) often have special features specific to its
  ;; implementation of hash tables that we currently do not extract and
  ;; copy. Example: For SBCL there are additional keyword arguments of
  ;; :hash-function, :weakness, and :synchronized to MAKE-HASH-TABLE.  We may
  ;; find in time we want to add these things on a vendor by vendor basis.
  (eql-map-record eql-map object :allocation)
  (make-hash-table
   :test (hash-table-test object)
   :size (hash-table-size object)
   :rehash-size (hash-table-rehash-size object)
   :rehash-threshold (hash-table-rehash-threshold object)))

;;; ---------------------------------------------------------------------------
;; Identity cloning of various types section
;;
;; These are clone-objects for things that just return themselves with no
;; actual copying of any data. We do this instead of a catch-all method of type
;; T because then we can proactively catch things we don't support and make a
;; decision about it as opposed to silently producing possible garbage during
;; the cloning operations. Currently, there is no handling of intention for
;; these types either, so we leave them to be T.
;;; ---------------------------------------------------------------------------

;;; -------------------------------
;; Identity Cloning a function
;;; -------------------------------

;; identity-clone + ANY intention
(defmethod clone-object progn ((cloned-object function)
                               (original-object function)
                               (policy identity-clone)
                               intention
                               last-known-intention
                               eql-map
                               &key)

  ;; NOTE: cloned-object should be identical to original-object.
  cloned-object)

;;; -------------------------------
;; Identity Cloning a character
;;; -------------------------------

;; identity-clone + ANY intention
(defmethod clone-object progn ((cloned-object character)
                               (original-object character)
                               (policy identity-clone)
                               intention
                               last-known-intention
                               eql-map
                               &key)

  ;; NOTE: cloned-object should be identical to original-object.
  cloned-object)

;;; -------------------------------
;; Identity Cloning a pathname
;;; -------------------------------

;; identity-clone + ANY intention
(defmethod clone-object progn ((cloned-object pathname)
                               (original-object pathname)
                               (policy identity-clone)
                               intention
                               last-known-intention
                               eql-map
                               &key)

  ;; NOTE: cloned-object should be identical to original-object.
  cloned-object)

;;; -------------------------------
;; Identity Cloning a symbol
;;; -------------------------------

;; identity-clone + ANY intention
(defmethod clone-object progn ((cloned-object symbol)
                               (original-object symbol)
                               (policy identity-clone)
                               intention
                               last-known-intention
                               eql-map
                               &key)

  ;; NOTE: cloned-object should be identical to original-object.
  cloned-object)

;;; -------------------------------
;; Identity Cloning a number
;;; -------------------------------

;; identity-clone + ANY intention
(defmethod clone-object progn ((cloned-object number)
                               (original-object number)
                               (policy identity-clone)
                               intention
                               last-known-intention
                               eql-map
                               &key)

  ;; NOTE: cloned-object should be identical to original-object.
  cloned-object)


;;; ---------------------------------------------------------------------------
;; Shallow cloning of various types section
;;; ---------------------------------------------------------------------------

;;; -------------------------------
;; Shallow Cloning a cons cell
;;; -------------------------------

;; shallow-clone + cons-intention
;;
;; shallow clones the SINGLE cons cell given to it with no recursion,
;; remapping, or cycle detection what-so-ever.
(defmethod clone-object progn ((cloned-object cons)
                               (original-object cons)
                               (policy shallow-clone)
                               (intention cons-intention)
                               (last-known-intention no-specific-intention)
                               eql-map
                               &key)

  (destructuring-bind (l . r) original-object
    (eql-map-record eql-map l :move-original)
    (eql-map-record eql-map r :move-original)
    (setf (car cloned-object) l
          (cdr cloned-object) r))
  cloned-object)

;; shallow-clone + list-intention
;;
;; shallow clones the toplevel list structure ONLY. Can handle cycles and
;; certain kinds of shared structure in the list structure.
(defmethod clone-object progn ((cloned-object cons)
                               (original-object cons)
                               (policy shallow-clone)
                               (intention list-intention)
                               (last-known-intention no-specific-intention)
                               eql-map
                               &key)

  ;; Note: We've technically visited the start of the list already in CLONE.
  (destructuring-bind (l . r) original-object
    ;; We always shallow copy the car no matter what it was.
    ;; NOTE: if the car points back into the list structure...too bad use
    ;; deep copy for such a thing.
    (eql-map-record eql-map l :move-original)
    (setf (car cloned-object) l)

    ;; If the cdr isn't a cons (hence an improper list), the answer is easy.
    (unless (consp r)
      (eql-map-record eql-map r :move-original)
      (setf (cdr cloned-object) r)
      (return-from clone-object cloned-object))

    ;; But if the cdr was a cons, then the answer is much harder since the cdr
    ;; can represent: a complete proper list, a currently proper list that then
    ;; turns into an improper list, or a cycle at some point (including
    ;; immediately).

    (loop :with end = cloned-object
          :for original-cell :on r
          :do (let ((eql-map-entry
                      (eql-map-mark-visited eql-map original-cell)))
                (if (transition-p eql-map-entry)
                    ;; If we encounter a transitioned cons entry in the list
                    ;; structure, then we consider that it will either be a
                    ;; cycle, or we're about to traverse data we already
                    ;; copied. In this case, we just fixate the link to the
                    ;; transitioned item and call it a day for cloning the
                    ;; list.
                    (if (intention= intention (intent eql-map-entry))
                        (progn
                          (eql-map-record eql-map (target eql-map-entry)
                                          :move-clone)
                          (setf (cdr end) (target eql-map-entry))
                          (return))
                        (error "Unsupported list intention pair: ~A and ~A"
                               intention (intent eql-map-entry)))

                    ;; If not a transition, then original-cell hasn't been
                    ;; cloned (or transitioned) before in the list traversal,
                    ;; so copy it and continue (but beware the cdr might be an
                    ;; improper list!).
                    (let* ((new-cell (clone-allocate original-cell eql-map))
                           (l-original (car original-cell))
                           (r-original (cdr original-cell))
                           (list-continues-p (consp r-original)))

                      ;; The car of the new-cell is an easy fixup since we're
                      ;; doing a shallow copy. If the car points into the
                      ;; original list structure, you're going to have a bad
                      ;; time because you should use deep copy for that.
                      (eql-map-record eql-map l-original :move-original)
                      (setf (car new-cell) l-original)

                      ;; For the cdr, we could discover that we have an
                      ;; improper list.
                      (unless list-continues-p
                        (eql-map-record eql-map r-original :move-original)
                        (setf (cdr new-cell) r-original))

                      ;; Finally transition the original-cell to the new-cell.
                      (eql-map-mark-target eql-map original-cell new-cell
                                           intention)

                      ;; Prepare to keep traversing the list structure if need
                      ;; be.
                      (setf (cdr end) new-cell
                            end new-cell)))))

    ;; Return the list structure entry point!
    cloned-object))

;; shallow-clone + alist-intention
;;
;; Shallow copy the list structure and the consp in the car spot of each list
;; cons cell when available. We do it manually as opposed to recursive calls
;; cause this needs to be efficient.
(defmethod clone-object progn ((cloned-object cons)
                               (original-object cons)
                               (policy shallow-clone)
                               (intention alist-intention)
                               (last-known-intention no-specific-intention)
                               eql-map
                               &key)

  (flet ((clone-maybe-kv-cell (maybe-kv-cell)
           ;; We specifically preserve the original contents in the shallow
           ;; clone of the cons cell (or return a previous one we've seen
           ;; before). If there is self-referential information here, there
           ;; will be a surprise in the clone!
           (if (consp maybe-kv-cell)
               (clone-shallow-cons maybe-kv-cell eql-map)
               ;; Otherwise, just use the non-cons object.
               (progn
                 (eql-map-record eql-map maybe-kv-cell :move-original)
                 maybe-kv-cell))))

    (destructuring-bind (l . r) original-object
      ;; We always shallow copy the car no matter what it was.
      ;; NOTE: if the car points back into the list structure...too bad use
      ;; deep copy for such a thing.
      (setf (car cloned-object)
            (clone-maybe-kv-cell l))

      ;; If the cdr isn't a cons (hence an improper list), the answer is easy.
      (unless (consp r)
        (eql-map-record eql-map r :move-original)
        (setf (cdr cloned-object) r)
        (return-from clone-object cloned-object))

      ;; But if the cdr was a cons, then the answer is much harder since the
      ;; cdr can represent: a complete proper list, a currently proper list
      ;; that then turns into an improper list, or a cycle at some point
      ;; (including immediately).

      (loop :with end = cloned-object
            :for original-cell :on r
            :do (let ((eql-map-entry
                        (eql-map-mark-visited eql-map original-cell)))
                  (if (transition-p eql-map-entry)
                      ;; If we encounter a transitioned list structure cons
                      ;; entry, then we consider that it will either be a
                      ;; cycle, or we're about to traverse data we already
                      ;; copied. In this case, we just fixate the link to the
                      ;; transitioned item and call it a day for cloning the
                      ;; list.
                      (if (intention= intention (intent eql-map-entry))
                          (progn
                            (eql-map-record eql-map (target eql-map-entry)
                                            :move-clone)
                            (setf (cdr end) (target eql-map-entry))
                            (return))
                          (error "Unsupported list intention pair: ~A and ~A"
                                 intention (intent eql-map-entry)))

                      ;; If not a transition, then original-cell hasn't been
                      ;; cloned (or transitioned) before in the list traversal,
                      ;; so copy it and continue (but beware the cdr might be
                      ;; an improper list!).
                      (let* ((new-cell (clone-allocate original-cell eql-map))
                             (l-original (car original-cell))
                             (r-original (cdr original-cell))
                             (list-continues-p (consp r-original)))

                        ;; Transition the original-cell to the
                        ;; new-cell.
                        (eql-map-mark-target eql-map original-cell new-cell
                                             intention)

                        ;; The car of the new-cell is intended to be a kv cons
                        ;; cell, so clone it as such. If somehow it isn't then
                        ;; just copy it over.  If the car points into the
                        ;; original list structure, you're going to have a bad
                        ;; time because you should use deep copy for that.
                        (setf (car new-cell)
                              (clone-maybe-kv-cell l-original))

                        ;; For the cdr, we could discover that we have an
                        ;; improper list.
                        (unless list-continues-p
                          (eql-map-record eql-map r-original :move-original)
                          (setf (cdr new-cell) r-original))

                        ;; Prepare to keep traversing the list structure if
                        ;; need be.
                        (setf (cdr end) new-cell
                              end new-cell)))))

      ;; Return the list structure entry point!
      cloned-object)))

;; Helper for shallow clone + alist-intention. When we encounter a certain form
;; of shared structure: notably when a kv cell is actually part of the list
;; structure of an alist such as this: (let ((o (cons nil nil))) (setf (car o)
;; o (cdr o) o)) we attempt to clone (car o) as a cons cell with a
;; cons-intention -- but it is already cloned as part of the list structure
;; with alist-intention Since cons-inention is less complex than the
;; alist-intention of the list structure, we simply return the object
;; without any changes because the last-known-intention is more complex than
;; the intention we tried to go to.
(defmethod clone-object progn ((cloned-object cons)
                               (original-object cons)
                               (policy shallow-clone)
                               (intention cons-intention)
                               (last-known-intention alist-intention)
                               eql-map
                               &key)

  cloned-object)

;; shallow-clone + graph-intention
;;
;; (technically a tree made from cons cells may be a graph due to common
;; literal coalescing, so we'll have to process the entire thing as a
;; graph). Due to this we must treat both car AND cdr, when they are to a cons,
;; to be part of the graph structure and hence reference the eql-map and
;; cloned.
(defmethod clone-object progn ((cloned-object cons)
                               (original-object cons)
                               (policy shallow-clone)
                               (intention graph-intention)
                               (last-known-intention no-specific-intention)
                               eql-map
                               &key)

  ;; This function not only explores the child nodes, but also clones the graph
  ;; cons structure cells and maintains the structural equivalence of the
  ;; cloned graphed wrt the original graph.
  (flet ((explore-child-node (parent child setter q)
           (let ((parent-eql-map-entry (eql-map-visited-p eql-map parent))
                 (child-eql-map-entry (eql-map-visited-p eql-map child)))

             (if (not child-eql-map-entry)
                 ;; If it hadn't been explored, then either it is a cons cell
                 ;; and we keep cloning and exploring, or it isn't and we're
                 ;; done with that path (and shallow copy the value!)
                 (if (consp child)
                     (let ((new-child (clone-allocate child eql-map)))
                       ;; Visit child
                       (eql-map-mark-visited eql-map child)
                       ;; Generate the clone target edge.
                       (eql-map-mark-target eql-map child new-child intention)
                       ;; Preserve the link structure in the clone
                       ;; we're making.
                       (funcall setter
                                (target parent-eql-map-entry)
                                new-child)
                       ;; Finally enqueue the original car/cdr for further
                       ;; exploration
                       (queues:qpush q child))

                     ;; If not a cons, we just copy the reference to
                     ;; express the shallow clone of the original leaf value.
                     ;; Do _not_ put the leaf into the queue.
                     (progn
                       (eql-map-record eql-map child :move-original)
                       (funcall setter
                                (target parent-eql-map-entry)
                                child)))

                 ;; NOTE: This case catches certain situations like a graph
                 ;; consisting of a single cons cell pointing to itself, or
                 ;; when we're attempting to clone the graph and there are
                 ;; multiple roots into it. This ensure in those cases that the
                 ;; graph edge in the cloned graph is present.
                 ;;
                 ;; Since the child had been explored already, update the
                 ;; cloned parents's car/cdr to the cloned target to preserve
                 ;; the graph structure of the clone and we're done with the
                 ;; car/cdr edge. The BFS graph is keeping track of NODES that
                 ;; have been explored, not edges! So we ensure it is
                 ;; preserved.

                 (progn
                   ;; We KNOW that if it is in the eql-map table, it must have
                   ;; been cloned--because if how we treat allocatablep
                   ;; objects--so we're moving a known cloned reference here.
                   (eql-map-record eql-map (target child-eql-map-entry)
                                   :move-clone)
                   (funcall setter
                            (target parent-eql-map-entry)
                            (target child-eql-map-entry)))))))

    ;; Breadth First Search the graph, cloning the cons structure and the
    ;; graph structure
    (let ((q (queues:make-queue :simple-queue)))
      ;; The original-object has already been visited and the memory allocated
      ;; for the clone, but the cloned memory is not setup yet.
      (queues:qpush q original-object)
      (loop :until (zerop (queues:qsize q))
            :do (let* ((n (queues:qpop q))
                       (nl (car n))
                       (nr (cdr n)))
                  (explore-child-node n nl #'rplaca q)
                  (explore-child-node n nr #'rplacd q)))))

  ;; Then we return the root to the newly cloned graph.
  cloned-object)

;;; -------------------------------
;; Shallow Cloning an array
;;; -------------------------------

;; shallow-clone + ANY intention
;;
;; We simply copy the index values wholesale with no structural considerations
(defmethod clone-object progn ((cloned-object array)
                               (original-object array)
                               (policy shallow-clone)
                               intention
                               (last-known-intention no-specific-intention)
                               eql-map
                               &key)

  (cond
    ((fast-copyable-array-p original-object)
     ;; These types are all identical, very simple, and liable to be use for
     ;; graphics programming, textures, vertex arrays, etc. We attempt to
     ;; make this as fast as we can in the clone.
     (replace cloned-object original-object)
     (eql-map-record eql-map (array-element-type original-object)
                     :move-original :event (length original-object)
                                    :type-of-p nil)
     (eql-map-record eql-map cloned-object :array-clone-speed-fast))

    (t
     ;; Otherwise everything else gets the slow version cause it is hairy or
     ;; multidimensional or can hold many subtypes and those optimizations are
     ;; less straight-forward.
     (dotimes (index (array-total-size original-object))
       (let ((original-object-at-index (row-major-aref original-object index)))
         (eql-map-record eql-map original-object-at-index :move-original)
         (setf (row-major-aref cloned-object index)
               original-object-at-index)))
     (eql-map-record eql-map cloned-object :array-clone-speed-slow)))

  cloned-object)

;;; -------------------------------
;; Shallow Cloning a hash table.
;;; -------------------------------

;; shallow-clone + ANY intention
;;
;; We simply copy the key and values wholesale with no structural
;; considerations.
(defmethod clone-object progn ((cloned-object hash-table)
                               (original-object hash-table)
                               (policy shallow-clone)
                               intention
                               (last-known-intention no-specific-intention)
                               eql-map
                               &key)

  (u:do-hash (key value original-object)
    (eql-map-record eql-map key :move-original)
    (eql-map-record eql-map value :move-original)
    (setf (u:href cloned-object key)
          value))

  cloned-object)




;;; ---------------------------------------------------------------------------
;; Deep cloning of various types section
;;; ---------------------------------------------------------------------------

;;; -------------------------------
;; Deep Cloning cons cells.
;;; -------------------------------

;; deep-clone + graph-intention
(defmethod clone-object progn ((cloned-object cons)
                               (original-object cons)
                               (policy deep-clone)
                               (intention graph-intention)
                               (last-known-intention no-specific-intention)
                               eql-map
                               &key)

  ;; This function not only explores the child nodes, but also clones the graph
  ;; cons structure cells and maintains the structural equivalence of the
  ;; cloned graphed wrt the original graph.
  (flet ((explore-child-node (parent child setter q)
           (let ((parent-eql-map-entry (eql-map-visited-p eql-map parent))
                 (child-eql-map-entry (eql-map-visited-p eql-map child)))

             (if (not child-eql-map-entry)
                 ;; If it hadn't been explored, then either it is a cons cell
                 ;; and we keep cloning and exploring, or it isn't and we're
                 ;; done with that path (and deep copy the value!)
                 (if (consp child)
                     (let ((new-child (clone-allocate child eql-map)))
                       ;; Visit child
                       (eql-map-mark-visited eql-map child)
                       ;; Generate the clone target edge.
                       (eql-map-mark-target eql-map child new-child intention)
                       ;; Preserve the link structure in the clone
                       ;; we're making.
                       (funcall setter
                                (target parent-eql-map-entry)
                                new-child)
                       ;; Finally enqueue the original car/cdr for further
                       ;; exploration
                       (queues:qpush q child))

                     ;; If not a cons, we deep clone the reference to
                     ;; the original leaf value.
                     (funcall setter
                              (target parent-eql-map-entry)
                              (clone child policy intention eql-map)))

                 ;; NOTE: This case catches certain situations like a graph
                 ;; consisting of a single cons cell pointing to itself, or
                 ;; when we're attempting to clone the graph and there are
                 ;; multiple roots into it. This ensure in those cases that the
                 ;; graph edge in the cloned graph is present.
                 ;;
                 ;; We KNOW that the target had to be an allocatablep object to
                 ;; even be in the eql-map, so this is always a :move-clone
                 ;; here.
                 (progn
                   (eql-map-record eql-map (target child-eql-map-entry)
                                   :move-clone)
                   (funcall setter
                            (target parent-eql-map-entry)
                            (target child-eql-map-entry)))))))

    ;; Breadth First Search the graph, cloning the cons structure and the
    ;; graph structure
    (let ((q (queues:make-queue :simple-queue)))
      ;; The original-object has already been visited and the memory allocated
      ;; for the clone, but the cloned memory is not setup yet.
      (queues:qpush q original-object)
      (loop :until (zerop (queues:qsize q))
            :do (let* ((n (queues:qpop q))
                       (nl (car n))
                       (nr (cdr n)))
                  (explore-child-node n nl #'rplaca q)
                  (explore-child-node n nr #'rplacd q)))))

  ;; Then we return the root to the newly cloned graph.
  cloned-object)

;;; -------------------------------
;; Deep Cloning arrays.
;;; -------------------------------

;; deep-clone + graph-intention
(defmethod clone-object progn ((cloned-object array)
                               (original-object array)
                               (policy deep-clone)
                               intention
                               (last-known-intention no-specific-intention)
                               eql-map
                               &key)

  (cond
    ((fast-copyable-array-p original-object)
     ;; These types are all identical, very simple, and liable to be use for
     ;; graphics programming, textures, vertex arrays, etc. We attempt to
     ;; make this as fast as we can in the clone.
     (replace cloned-object original-object)
     (eql-map-record eql-map (array-element-type original-object)
                     :move-original :event (length original-object)
                                    :type-of-p nil)
     (eql-map-record eql-map cloned-object :array-clone-speed-fast))

    (t
     ;; Otherwise everything else gets the slow version cause it is hairy or
     ;; multidimensional or can hold many subtypes and those optimizations are
     ;; less straight-forward.
     (dotimes (index (array-total-size original-object))
       (setf (row-major-aref cloned-object index)
             (clone (row-major-aref original-object index) policy intention
                    eql-map)))
     (eql-map-record eql-map cloned-object :array-clone-speed-slow)))
  cloned-object)

;;; -------------------------------
;; Deep Cloning hash tables.
;;; -------------------------------

;; deep-clone + graph-intention
(defmethod clone-object progn ((cloned-object hash-table)
                               (original-object hash-table)
                               (policy deep-clone)
                               intention
                               (last-known-intention no-specific-intention)
                               eql-map
                               &key)
  (u:do-hash (key value original-object)
    (let ((cloned-key (clone key policy intention eql-map))
          (cloned-value (clone value policy intention eql-map)))
      (setf (u:href cloned-object cloned-key) cloned-value)))
  cloned-object)
