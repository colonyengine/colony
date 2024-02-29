(in-package #:virality.clone)

;;;; TODO: Much of this file should be put into a test package and converted to
;;;; parachute and put into its own package, directory system, and be part of
;;;; the Virality test system.

;;; ------------------------------------------------------------
;; TODO: Maybe put in vutils?
;;
;; A function to help generate graphs made from cons cells.
;; target is either a symbol representing a nodename or the form (:v xxx) where
;; xxx is some value we want for the car/cdr of the node. Don't make mistakes
;; when specifying the dsl, there is no error checking.
;; (a :r a)
;; (a :l b :l c :l (:v 42))
;; and so on.
;; If there are no computable roots, then one must specify the roots with:
;; (:roots a b c ... z)
;; NOTE that the nodes :roots names can be ANY named node in the graph beyond
;; the actual roots. This is useful when you need reference to some special
;; node in the graph that isn't actually a root.
;;
;; All leaf values must be specified otherwise they are considered "unbound"
;; and explicitly checked for. TODO: Make an ability to change this default
;; to just specifying NIL for anything not otherwise specified.
;;
;; Any :l and :r of any cell assigned more than once is flagged as an error
;; since it is almost certainly a typo in the DSL.
;;
;; Any :roots that doesn't exist is an error.
;;; ------------------------------------------------------------
(defun cons-graph (dsl)
  (labels ((syntax-roots-p (form)
             (and (consp form) (eq :roots (first form))))
           (check-unbound (cell-name cell link unbound-value)
             (ecase link
               (:l
                (unless (eq (car cell) unbound-value)
                  (error "cons-graph: Multiple assignment to car of cell: ~A "
                         cell-name)))
               (:r
                (unless (eq (cdr cell) unbound-value)
                  (error "cons-graph: Multiple assignment to cdr of cell: ~A "
                         cell-name)))))
           (construct-link (cell-name cell link value unbound)
             ;; NOTE: If we assign the same link multiple times, it is almost
             ;; surely an error and we stop and complain about the DSL.
             (check-unbound cell-name cell link unbound)
             (funcall (ecase link (:l #'rplaca) (:r #'rplacd))
                      cell value)))

    (let ((unbound (gensym "UNBOUND"))
          (table (make-hash-table))
          (specified-roots (make-hash-table))
          (roots (make-hash-table)))

      ;; Convert the DSL into a real graph of cons cells.
      (dolist (form dsl)
        (cond
          ;; Handle special syntax forms
          ((syntax-roots-p form)
           (loop :for root :in (cdr form)
                 :do (u:ensure-gethash root specified-roots t)))
          ;; a regular path form.
          (t
           (loop :for (source link target) :on form :by #'cddr
                 :do (when link
                       ;; Ensure the source exists.
                       (let ((source-cell
                               (u:ensure-gethash source table
                                                 (cons unbound unbound))))
                         ;; Each source is potentially a root if we've never
                         ;; seen it.
                         (u:ensure-gethash source roots t)
                         ;; Do we link to a value or another cons cell?
                         (if (consp target)
                             ;; Set the value at the direction requested.
                             (destructuring-bind (sym value) target
                               (assert (eq sym :v))
                               (construct-link source source-cell link value
                                               unbound))
                             ;; else ensure the target exists, make the link
                             ;; in the requested direction.
                             (let ((target-cell
                                     (u:ensure-gethash target table
                                                       (cons unbound
                                                             unbound))))
                               ;; a target can never be a root (unless
                               ;; explicitly made to be a root with a :roots
                               ;; syntax form)
                               (setf (gethash target roots) nil)
                               (construct-link source source-cell link
                                               target-cell unbound)))))))))


      ;; Now check that no node has an unbound leaf. EVERYTHING must be
      ;; specified.
      (let ((valid t)
            (unbound-nodes nil))
        (u:do-hash (name node table)
          (cond
            ((eq (car node) unbound)
             (setf valid nil)
             (pushnew (list name :l) unbound-nodes :test #'equal)
             (format t "The car side of ~A is unbound! Fix it!~%" name))
            ((eq (cdr node) unbound)
             (setf valid nil)
             (pushnew (list name :r) unbound-nodes :test #'equal)
             (format t "The cdr side of ~A is unbound! Fix it!~%" name))))
        (unless valid
          (error "There were unbound leaves for the specified nodes: ~S"
                 unbound-nodes)))

      ;; Manually mark roots if supplied. This will enforce root discovery
      ;; for graphs with no roots due to cycles.
      (u:do-hash-keys (name specified-roots)
        (unless (nth-value 1 (u:href roots name))
          (error "Attempt to set a root ~A but there is no node of that name!"
                 name))
        (setf (u:href roots name) t))

      ;; collect the roots
      (let ((collected-roots nil))
        (u:do-hash (name root-p roots)
          (when root-p
            (pushnew (list name :- (u:href table name)) collected-roots
                     :test #'equal)))
        (unless collected-roots
          (error "No roots found! Use (:roots a b c ... z) form."))

        (values collected-roots table)))))

(defun get-roots (collected-roots &rest root-names)
  (apply #'values
         (mapcar (lambda (r) (third (assoc r collected-roots)))
                 root-names)))

(defun test-gen-cons-graph ()
  (let* ((*print-circle* t)
         (dsl0 '((a :l (:v 1))
                 (a :r b :r (:v 100))
                 (b :l a)
                 (:roots a)))
         (dsl0-graph (gen-cons-graph dsl0)))
    (flet ((emit-graph (desc dsl graph)
             (format t "~A forms:~% ~{~S~% ~}~%" desc dsl)
             (format t "~A graph roots:~% ~{~S ~}~%" desc graph)))
      (emit-graph "DSL0" dsl0 dsl0-graph)
      )))

;;; ------------------------------------------------------------
;; Testing the EQL-MAP API
;;; ------------------------------------------------------------
(defun test-eql-map ()
  (let ((eql-map (make-eql-map))
        (o0 42)
        (o1 (cons 1 2))
        (o2 (list 1 2 3))
        (o3 "Hello There")
        (o4 (make-hash-table)))
    (eql-map-mark-visited eql-map o0)
    (eql-map-mark-target eql-map o0 42 (make-graph-intention) )
    (eql-map-mark-visited eql-map o1)
    (eql-map-mark-visited eql-map o2)
    (eql-map-mark-visited eql-map o3)
    (eql-map-mark-visited eql-map o4)
    (eql-map-mark-target eql-map o4 (make-hash-table)
                         (make-graph-intention) )
    (dump-eql-map eql-map)))

;;; ---------------------------------------------------------------------------
;; EQL-MAP helper functions for the tests.
;;; ---------------------------------------------------------------------------
(defun make-eql-map-with-stats ()
  (make-eql-map :stats-copy t
                :stats-allocation t))

(defun eql-map-get-stats-domain (eql-map domain)
  (u:when-let ((tbl (stats eql-map)))
    (u:href tbl domain)))

;; TODO: This is sort of cruddy code, and maybe should be moved into
;; clone.lisp.
(defun eql-map-dump-stats (eql-map)
  (when (stats eql-map)
    (format t "EQL-MAP statistics:~%"))

  (u:when-let ((tbl (eql-map-get-stats-domain eql-map :move)))
    (format t " Move Stats:~%")
    (if (plusp (hash-table-count tbl))
        (u:do-hash (type-name num-moved tbl)
          (format t "  ~8@A ~S~%" num-moved type-name))
        (format t "  ~8@A~%" "NONE")))

  (u:when-let ((tbl (eql-map-get-stats-domain eql-map :allocation)))
    (format t " Allocation Stats:~%")
    (if (plusp (hash-table-count tbl))
        (u:do-hash (type-name num-allocated tbl)
          (format t "  ~8@A ~S~%" num-allocated type-name))
        (format t "  ~8@A~%" "NONE"))))

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
              "In domain ~S, the stat ~S was expected to have value ~A, but actually had value ~A"
              domain stat-name expected-value actual-value))

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
              ((:move :allocation)
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
            ((:move :allocation)
             (unless
                 (zerop (hash-table-count(u:href (stats eql-map) domain-key)))
               (error-stats-found domain-key)))))))
    (values t :ok)))

;; Helper macro for the eql-map statistics tests.
(defmacro validate-eql-map-stats (match-form)
  (u:with-gensyms (matchedp reason domain msg)
    `(u:mvlet* ((,matchedp ,reason ,domain ,msg
                           ,match-form))
       (unless ,matchedp
         (error "eql-map matching failed:~% reason: ~S~% domain: ~S~% msg: ~S"
                ,reason ,domain ,msg))
       t)))


(defun id-type (val)
  "Return two values. The first value is VAL and the second value is the type
of VAL."
  (values val (type-of val)))

;;; ---------------------------------------------------------------------------
;; SHALLOW cloning tests.
;;; ---------------------------------------------------------------------------


;;; ------------------------------------------------------------
;; clone-shallow functions of ANY intention
;;; ------------------------------------------------------------

(defun test-clone-shallow-function-0 ()
  (u:mvlet* (;; Type FUNCTION
             (o o-type (id-type #'cl:identity))
             ;; NOTE: intention doesn't matter.
             (c eql-map (clone-shallow o (make-eql-map-with-stats))))

    (format t "Original | ~S~%" o)
    (format t "Cloned   | ~S~%" c)
    (finish-output)

    (assert (eq o c))

    (eql-map-dump-stats eql-map)

    (validate-eql-map-stats
     (eql-map-stats-match-p eql-map
                            `(:move (,o-type 1))))

    (format t "Passed.~%")
    t))

;;; ------------------------------------------------------------
;; clone-shallow character of ANY intention
;;; ------------------------------------------------------------

(defun test-clone-shallow-character-0 ()
  (u:mvlet* (;; Type CHARACTER aka STANDARD-CHAR
             (o o-type (id-type #\a))
             ;; NOTE: intention doesn't matter.
             (c eql-map (clone-shallow o (make-eql-map-with-stats))))

    (format t "Original | ~S~%" o)
    (format t "Cloned   | ~S~%" c)
    (finish-output)

    (assert (eql o c))

    (eql-map-dump-stats eql-map)
    (validate-eql-map-stats
     (eql-map-stats-match-p eql-map
                            `(:move (,o-type 1))))

    (format t "Passed.~%")
    t))

;;; ------------------------------------------------------------
;; clone-shallow pathname of ANY intention
;;; ------------------------------------------------------------

(defun test-clone-shallow-pathname-0 ()
  (u:mvlet* (;; Type PATHNAME
             (o o-type (id-type #P"/tmp/foo.txt"))
             ;; NOTE: intention doesn't matter.
             (c eql-map (clone-shallow o (make-eql-map-with-stats))))

    (format t "Original | ~S~%" o)
    (format t "Cloned   | ~S~%" c)
    (finish-output)

    (assert (equal o c))

    (eql-map-dump-stats eql-map)
    (validate-eql-map-stats
     (eql-map-stats-match-p eql-map `(:move (,o-type 1))))

    (format t "Passed.~%")
    t))

;;; ------------------------------------------------------------
;; clone-shallow symbol of ANY intention
;;; ------------------------------------------------------------

(defun test-clone-shallow-symbol-0 ()
  (u:mvlet* (;; Type SYMBOL
             (o o-type (id-type 'a))
             ;; NOTE: intention doesn't matter.
             (c eql-map (clone-shallow o (make-eql-map-with-stats))))

    (format t "Original | ~S~%" o)
    (format t "Cloned   | ~S~%" c)
    (finish-output)

    (assert (eq o c))

    (eql-map-dump-stats eql-map)
    (validate-eql-map-stats
     (eql-map-stats-match-p eql-map
                            `(:move (,o-type 1))))

    (format t "Passed.~%")
    t))

;;; ------------------------------------------------------------
;; clone-shallow integer of ANY intention
;;; ------------------------------------------------------------

(defun test-clone-shallow-integer-0 ()
  (u:mvlet* (;; Type INTEGER
             (o o-type (id-type 42))
             ;; NOTE: intention doesn't matter.
             (c eql-map (clone-shallow o (make-eql-map-with-stats))))

    (format t "Original | ~S~%" o)
    (format t "Cloned   | ~S~%" c)
    (finish-output)

    (assert (eql o c))

    (eql-map-dump-stats eql-map)
    (validate-eql-map-stats
     (eql-map-stats-match-p eql-map
                            `(:move (,o-type 1))))

    (format t "Passed.~%")
    t))

;;; ------------------------------------------------------------
;; clone-shallow cons of CONS intention
;;; ------------------------------------------------------------

(defun test-clone-shallow-cons-0 ()
  (u:mvlet* ((v0 v0-type (id-type 1))
             (v1 v1-type (id-type (make-hash-table)))
             (o o-type (id-type (cons v0 v1)))
             (c eql-map (clone-shallow-cons o (make-eql-map-with-stats))))

    (format t "Original | ~S~%" o)
    (format t "Cloned   | ~S~%" c)
    (finish-output)

    ;; This part must be cloned.
    (assert (not (eq o c)))

    ;; but the values of the car and cdr must still be eq/eql to original.
    (assert (eql (car c) (car o)))
    (assert (eq (cdr c) (cdr o)))

    (eql-map-dump-stats eql-map)
    (validate-eql-map-stats
     (eql-map-stats-match-p eql-map
                            `(:move (,v0-type 1) (,v1-type 1))
                            `(:allocation (,o-type 1))))

    (format t "Passed.~%")
    t))

(defun test-clone-shallow-cons-1 ()
  (u:mvlet* ((*print-circle* t)
             (o o-type (id-type (cons nil nil))))
    ;; Both car and cdr point to o.
    (setf (car o) o
          (cdr o) o)

    (u:mvlet ((c eql-map (clone-shallow-cons o (make-eql-map-with-stats))))
      (format t "Original | ~S~%" o)
      (format t "Cloned   | ~S~%" c)
      (finish-output)

      ;; The shallow clone created a new memory allocation.
      (assert (not (eq o c)))

      ;; But the very surprising thing is that the car and cdr were shallow
      ;; copied. They still point to the original o! This means that o and
      ;; c cannot be EQUAL to each other when cycles are present and a shallow
      ;; copy happens.
      (assert (eq (car c) o))
      (assert (eq (cdr c) o))

      (eql-map-dump-stats eql-map)
      (validate-eql-map-stats
       (eql-map-stats-match-p eql-map
                              ;; NOTE: the move of the original cons cell
                              ;; reference into the cloned cons cell.
                              `(:move (,o-type 2))
                              `(:allocation (,o-type 1))))

      (format t "Passed (with expected surprise due to cycle).~%")
      t)))

;;; ------------------------------------------------------------
;; clone-shallow cons of LIST intention
;;; ------------------------------------------------------------

(defun test-clone-shallow-list-0 ()
  "Test the usual case of a proper list with interesting stuff in it."
  (u:mvlet* ((v0 v0-type (id-type'a))
             (v1 v1-type (id-type 1))
             (v2 v2-type (id-type (cons #\a #\a)))
             (v3 v3-type (id-type (make-hash-table)))
             (v4 v4-type (id-type (list 10 20 30)))
             ;; And the implicit end of the list value.
             (list-end-type (type-of nil))
             (o o-type (id-type (list v0 v1 v2 v3 v4)))
             (c eql-map (clone-shallow-list o (make-eql-map-with-stats))))
    (format t "Original | ~S~%" o)
    (format t "Cloned   | ~S~%" c)

    (assert (symbolp (nth 0 o)))
    (assert (numberp (nth 1 o)))
    (assert (consp (nth 2 o)))
    (assert (hash-table-p (nth 3 o)))
    (assert (listp (nth 4 o)))

    (assert (symbolp (nth 0 c)))
    (assert (numberp (nth 1 c)))
    (assert (consp (nth 2 c)))
    (assert (hash-table-p (nth 3 c)))
    (assert (listp (nth 4 c)))

    (assert (equal o c))

    (loop :for o-cell :on o
          :for c-cell :on c
          :do (assert (not (eq o-cell c-cell))))

    (format t "Making changes...~%")
    (setf (nth 0 o) 'aaa)
    (setf (nth 1 o) 10)
    (setf (car (nth 2 o)) #\c)
    (setf (gethash 'foo (nth 3 o)) t)

    (setf (nth 0 c) 'bbb)
    (setf (nth 1 c) 100)
    (setf (cdr (nth 2 c)) #\d)
    (setf (gethash 'bar (nth 3 c)) t)

    (format t "Changed Original | ~S~%" o)
    (format t "Changed Cloned   | ~S~%" c)
    (finish-output)

    ;; Check certain constraints after the changes.

    (assert (not (equal o c)))
    (assert (not (eq (nth 0 o) (nth 0 c))))
    (assert (eql (nth 1 o) 10))
    (assert (eql (nth 1 c) 100))
    (assert (eq (nth 2 o) (nth 2 c)))
    (assert (eq (nth 3 o) (nth 3 c)))
    (assert (eq (nth 4 o) (nth 4 o)))

    (eql-map-dump-stats eql-map)
    (validate-eql-map-stats
     (eql-map-stats-match-p eql-map
                            `(:move (,v0-type 1)
                                    (,v1-type 1)
                                    (,v2-type 1)
                                    (,v3-type 1)
                                    (,v4-type 1)
                                    (,list-end-type 1))
                            `(:allocation (,o-type 5))))

    (format t "Passed.~%")
    t))

(defun test-clone-shallow-list-1 ()
  "Handle entries in the list which are identical."
  (u:mvlet* ((*print-circle* t)
             (v0 v0-type (id-type (cons 1 2)))
             (v1 v1-type (id-type (cons 'a 'b)))
             (v2 v2-type (id-type (cons 'c 'd)))
             ;; And one for the end of the list.
             (list-end-type (type-of nil))
             (o o-type (id-type (list v1 v0 v0 v2)))
             (c eql-map (clone-shallow-list o (make-eql-map-with-stats))))
    (format t "Original | ~S~%" o)
    (format t "Cloned   | ~S~%" c)
    (finish-output)

    (assert (equal o c))
    (assert (every #'consp o))
    (assert (every #'consp c))
    (assert (eq (second c) (third c)))

    ;; The list structure cons cells must be different.
    (loop :for o-cell :on o
          :for c-cell :on c
          :do (assert (not (eq o-cell c-cell))))

    ;; But the actual entries in the lists were shallow copied.
    (loop :for a :in o
          :for b :in c
          :do (assert (eq a b))) ;; NOTE: expecting all cons cells in list.

    (eql-map-dump-stats eql-map)
    (validate-eql-map-stats
     (eql-map-stats-match-p eql-map
                            `(:move (,v0-type 2)
                                    (,v1-type 1)
                                    (,v2-type 1)
                                    (,list-end-type 1))
                            `(:allocation (,o-type 4))))

    (format t "Passed.~%")
    t))

(defun test-clone-shallow-list-2 ()
  "Handle a cons cell that references itself in both car and cdr."
  (u:mvlet* ((*print-circle* t)
             (o o-type (id-type (cons nil nil))))
    ;; A single cons cell representing a list that has both itself as the first
    ;; element and a cycle for the rest of the list.
    (setf (car o) o
          (cdr o) o)

    (u:mvlet ((c eql-map (clone-shallow-list o (make-eql-map-with-stats))))
      (format t "Original | ~S~%" o)
      (format t "Cloned   | ~S~%" c)
      (finish-output)

      (assert (eq o (car o)))
      (assert (eq o (cdr o)))

      ;; The clone must have produced a new starting cons cell.
      (assert (not (eq c o)))
      ;; The car should have been shallow copied to original list.
      (assert (eq (car c) o))
      ;; But notice the cdr is pointing into the cloned list structure.
      (assert (eq (cdr c) c))

      (eql-map-dump-stats eql-map)
      (validate-eql-map-stats
       (eql-map-stats-match-p eql-map
                              ;; The car of the clone is a move of the original
                              ;; cons cell.
                              `(:move (,o-type 1))
                              ;; But the cons cell itself was cloned.
                              `(:allocation (,o-type 1))))

      (format t "Passed with expected surprise in list structure.~%")
      t)))

(defun test-clone-shallow-list-3 ()
  "Cdr of last cons cell points to head of list cons in a cycle."
  (u:mvlet* ((*print-circle* t)
             (v0 v0-type (id-type 1))
             (v1 v1-type (id-type 2))
             (v2 v2-type (id-type 3))
             (v3 v3-type (id-type 4))
             (o o-type (id-type (list v0 v1 v2 v3))))
    ;; Last cons cell cdr points to head of list in a cycle.
    (setf (cddddr o) o)
    (format t "Original | ~S~%" o)
    (finish-output)

    (u:mvlet ((c eql-map (clone-shallow-list o (make-eql-map-with-stats))))
      (format t "Cloned   | ~S~%" c)
      (finish-output)

      (assert (eq o (cddddr o)))
      (assert (eq c (cddddr c)))
      (assert (not (eq o c)))
      (assert (not (eq (cddddr o) (cdddr c))))

      (eql-map-dump-stats eql-map)
      (validate-eql-map-stats
       (eql-map-stats-match-p eql-map
                              `(:move (,v0-type 1)
                                      (,v1-type 1)
                                      (,v2-type 1)
                                      (,v3-type 1))
                              `(:allocation (,o-type 4))))

      (format t "Passed.~%")
      t)))

(defun test-clone-shallow-list-4 ()
  "Cdr of last cons cell points to middle of list cons in a cycle."
  (u:mvlet* ((*print-circle* t)
             (v0 v0-type (id-type 1))
             (v1 v1-type (id-type 2))
             (v2 v2-type (id-type 3))
             (v3 v3-type (id-type 4))
             (o o-type (id-type (list v0 v1 v2 v3))))
    ;; Last cons cell cdr points to middle of list in a cycle.
    (setf (cddddr o) (cdr o))
    (format t "Original | ~S~%" o)
    (finish-output)

    (u:mvlet ((c eql-map (clone-shallow-list o (make-eql-map-with-stats))))
      (format t "Cloned   | ~S~%" c)
      (finish-output)

      (assert (eq (cdr o) (cddddr o)))
      (assert (eq (cdr c) (cddddr c)))
      (assert (not (eq o c)))
      (assert (not (eq (cddddr o) (cddddr c))))
      (assert (not (eq (cdr o) (cdr c))))

      (eql-map-dump-stats eql-map)
      (validate-eql-map-stats
       (eql-map-stats-match-p eql-map
                              `(:move (,v0-type 1)
                                      (,v1-type 1)
                                      (,v2-type 1)
                                      (,v3-type 1))
                              `(:allocation (,o-type 4))))


      (format t "Passed.~%")
      t)))

;;; ------------------------------------------------------------
;; clone-shallow cons of ALIST intention
;;; ------------------------------------------------------------

(defun test-clone-shallow-alist-0 ()
  "Test a basic well formatted alist in the common form."
  (u:mvlet* ((v0 v0-type (id-type 'a))
             (v1 v1-type (id-type #\a))
             (v2 v2-type (id-type 'b))
             (v3 v3-type (id-type 'foo))
             (v4 v4-type (id-type 'c))
             (v5 v5-type (id-type 3))
             (v6 v6-type (id-type 'd))
             (v7 v7-type (id-type (make-hash-table)))
             (v8 v8-type (id-type 'e))
             (v9 v9-type (id-type (cons 100 200)))
             (v10 v10-type (id-type #\a))
             (v11 v11-type (id-type #\b))
             (v12 v12-type (id-type (cons 1 2)))
             (v13 v13-type (id-type (make-array 4)))
             (v14 v14-type (id-type (make-hash-table)))
             (v15 v15-type (id-type (lambda (x) x)))
             (list-end-type (type-of nil))
             (o o-type
                (id-type (list (cons v0 v1)
                               (cons v2 v3)
                               (cons v4 v5)
                               (cons v6 v7)
                               (cons v8 v9)
                               (cons v10 v11)
                               (cons v12 v13)
                               (cons v14 v15))))
             (c eql-map (clone-shallow-alist o (make-eql-map-with-stats))))

    (format t "Original | ~S~%" o)
    (format t "Cloned   | ~S~%" c)

    ;; Check that the list structure cells were cloned.
    (loop :for o-cell :on o
          :for c-cell :on c
          :do (assert (not (eq o-cell c-cell))))

    ;; Check that the alist cons cells were also cloned.
    (loop :for o-alist-cell :in o
          :for c-alist-cell :in c
          :do (assert (not (eq o-alist-cell c-alist-cell))))

    ;; Check that the contents of the alist cons cell were shallow cloned.
    (loop :for o-alist-cell :in o
          :for c-alist-cell :in c
          :do (assert (and (eql (car o-alist-cell) (car c-alist-cell))
                           (eql (cdr o-alist-cell) (cdr c-alist-cell)))))

    (eql-map-dump-stats eql-map)
    (validate-eql-map-stats
     (eql-map-stats-match-p eql-map
                            `(:move (,v0-type 1)
                                    (,v1-type 1)
                                    (,v2-type 1)
                                    (,v3-type 1)
                                    (,v4-type 1)
                                    (,v5-type 1)
                                    (,v6-type 1)
                                    (,v7-type 1)
                                    (,v8-type 1)
                                    (,v9-type 1)
                                    (,v10-type 1)
                                    (,v11-type 1)
                                    (,v12-type 1)
                                    (,v13-type 1)
                                    (,v14-type 1)
                                    (,v15-type 1)
                                    (,list-end-type 1))
                            `(:allocation (,o-type 16))))

    (format t "Passed.~%")
    t))

(defun test-clone-shallow-alist-1 ()
  "Test reconstruction of shared reference kv cons cells."
  (u:mvlet* ((v0 v0-type (id-type (make-array 4)))
             (v1 v1-type (id-type #\b))
             (v2 (cons v0 v1))
             (v3 v3-type (id-type 'a))
             (v4 v4-type (id-type 1))
             (v5 v5-type (id-type #\a))
             (v6 v6-type (id-type (make-hash-table)))
             (o o-type (id-type (list (cons v3 v4) v2 v2 (cons v5 v6))))
             (list-end-type (type-of nil))
             (c eql-map (clone-shallow-alist o (make-eql-map-with-stats))))
    (format t "Original | ~S~%" o)
    (format t "Cloned   | ~S~%" c)
    (finish-output)

    ;; Check that the list structure was cloned.
    (loop :for o-cell :on o
          :for c-cell :on c
          :do (assert (not (eq o-cell c-cell))))

    ;; Check that the alist cons cells were also cloned.
    (loop :for o-alist-cell :in o
          :for c-alist-cell :in c
          :do (assert (not (eq o-alist-cell c-alist-cell))))

    ;; Check that the contents of the alist cons cell were shallow cloned.
    (loop :for o-alist-cell :in o
          :for c-alist-cell :in c
          :do (assert (and (eql (car o-alist-cell) (car c-alist-cell))
                           (eql (cdr o-alist-cell) (cdr c-alist-cell)))))

    ;; check that the shared structure was preserved in the clone.
    (assert (eq (second c) (third c)))

    (eql-map-dump-stats eql-map)
    (validate-eql-map-stats
     (eql-map-stats-match-p eql-map
                            `(:move (,v0-type 1)
                                    (,v1-type 1)
                                    ;; v2 will be allocated, not moved.
                                    (,v3-type 1)
                                    (,v4-type 1)
                                    (,v5-type 1)
                                    (,v6-type 1)
                                    (,list-end-type 1))
                            ;; Only 7 because of a shared cons cell!
                            `(:allocation (,o-type 7))))

    (format t "Passed.~%")
    t))

(defun test-clone-shallow-alist-2 ()
  "Handle a single cons cell alist that references itself in both car and cdr."
  (u:mvlet ((*print-circle* t)
            (o o-type (id-type (cons nil nil))))

    ;; A single cons cell representing an alist that has both itself as the
    ;; first element and a cycle for the rest of the list.
    (setf (car o) o
          (cdr o) o)

    (u:mvlet* ((c eql-map (clone-shallow-alist o (make-eql-map-with-stats))))
      (format t "Original | ~S~%" o)
      (format t "Cloned   | ~S~%" c)
      (finish-output)

      (assert (eq o (car o)))
      (assert (eq o (cdr o)))

      ;; The clone must have produced a new starting cons cell.
      (assert (not (eq c o)))
      ;; The car should point to the newly created alist head which is also the
      ;; kv cell in the alist, just like in the original.
      (assert (eq (car c) c))
      ;; But notice the cdr is pointing into the cloned alist structure.
      (assert (eq (cdr c) c))

      (eql-map-dump-stats eql-map)
      (validate-eql-map-stats
       (eql-map-stats-match-p eql-map
                              `(:allocation (,o-type 1))))

      (format t "Passed.~%")
      t)))

(defun test-clone-shallow-alist-3 ()
  "An alist with one proper cons entry, but then a cycle to itself."
  (u:mvlet* ((*print-circle* t)
             (v0 v0-type (id-type 1))
             (v1 v1-type (id-type 2))
             (o o-type (id-type (list (cons v0 v1)))))
    (setf (cdr o) o)
    (u:mvlet ((c eql-map (clone-shallow-alist o (make-eql-map-with-stats))))
      (format t "Original | ~S~%" o)
      (format t "Cloned   | ~S~%" c)
      (finish-output)

      ;; starting cons cell is different.
      (assert (not (eq c o)))
      ;; cons cell of kv pair is different
      (assert (not (eq (car o) (car c))))
      ;; cdr points back to self.
      (assert (eq (cdr c) c))
      (assert (eq (cdr o) o))
      ;; cdr points to new list, not old.
      (assert (not (eq (cdr c) (cdr o))))

      (eql-map-dump-stats eql-map)
      (validate-eql-map-stats
       (eql-map-stats-match-p eql-map
                              `(:move (,v0-type 1)
                                      (,v1-type 1))
                              `(:allocation (,o-type 2))))

      (format t "Passed.~%")
      t)))

(defun test-clone-shallow-alist-4 ()
  "An alist with additional non-kv cells entries in it and no cycles."
  (u:mvlet* ((v0 v0-type (id-type 1))
             (v1 v1-type (id-type 2))
             (v2 v2-type (id-type 'foo))
             (v3 v3-type (id-type 3))
             (v4 v4-type (id-type 4))
             (v5 v5-type (id-type 42.5))
             (list-end-type (type-of nil))
             (o o-type (id-type (list (cons v0 v1) v2 (cons v3 v4) v5)))
             (c eql-map (clone-shallow-alist o (make-eql-map-with-stats))))
    (format t "Original | ~S~%" o)
    (format t "Cloned   | ~S~%" c)
    (finish-output)

    ;; Check that the list structure was cloned.
    (loop :for o-cell :on o
          :for c-cell :on c
          :do (assert (not (eq o-cell c-cell))))

    ;; Check that if the entry in the list was a cons, it was cloned, but
    ;; if not, they are eql.
    (loop :for o-entry :in o
          :for c-entry :in c
          :do (cond
                ;; if the entries are cons cells, then they must have been
                ;; cloned.
                ((and (consp o-entry) (consp c-entry))
                 (assert (not (eq o-entry c-entry))))
                ;; but of not, then they must be eql.
                ((and (not (consp o-entry)) (not (consp c-entry)))
                 (assert (eql o-entry c-entry)))
                ;; Otherwise omething went wrong.
                (t (assert nil))))

    (eql-map-dump-stats eql-map)
    (validate-eql-map-stats
     (eql-map-stats-match-p eql-map
                            `(:move (,v0-type 1)
                                    (,v1-type 1)
                                    (,v2-type 1)
                                    (,v3-type 1)
                                    (,v4-type 1)
                                    (,v5-type 1)
                                    (,list-end-type 1))
                            `(:allocation (,o-type 6))))

    (format t "Passed.~%")
    t))

(defun test-clone-shallow-alist-5 ()
  "An alist with additional non-kv cells entries it ending improperly."
  (u:mvlet* ((v0 v0-type (id-type 1))
             (v1 v1-type (id-type 2))
             (v2 v2-type (id-type 'foo))
             (v3 v3-type (id-type 42))
             (v4 v4-type (id-type 77))
             (o o-type (id-type (list (cons v0 v2) v1 v2 v3))))

    (setf (cdr (cdddr o)) v4)
    (u:mvlet* ((c eql-map (clone-shallow-alist o (make-eql-map-with-stats))))
      (format t "Original | ~S~%" o)
      (format t "Cloned   | ~S~%" c)
      (finish-output)

      ;; Check that the list structure was cloned.
      (loop :for o-cell :on o
            :for c-cell :on c
            :do (assert (not (eq o-cell c-cell))))

      ;; check contents of first element cons cell.
      (assert (and (eql (caar o) (caar c))))
      (assert (and (eql (cdar o) (cdar c))))

      ;; check second and third element
      (assert (eq (nth 1 o) (nth 1 c)))
      (assert (eq (nth 2 o) (nth 2 c)))

      ;; check fourth element improper cons cell.
      (assert (eql (car (cdddr o)) (car (cdddr c))))
      (assert (eql (cdr (cdddr o)) (cdr (cdddr c))))

      (eql-map-dump-stats eql-map)
      (validate-eql-map-stats
       (eql-map-stats-match-p eql-map
                              `(:move (,v0-type 1)
                                      (,v1-type 1)
                                      ;; v2 used twice in the original
                                      (,v2-type 2)
                                      (,v3-type 1)
                                      (,v4-type 1))
                              `(:allocation (,o-type 5))))


      (format t "Passed.~%")
      t)))

;;; ------------------------------------------------------------
;; clone-shallow cons of GRAPH intention
;;; ------------------------------------------------------------

(defun test-clone-shallow-graph-0 ()
  "Handle a cons cell that references two distinct atomic-like things."
  (u:mvlet* ((*print-circle* t)
             (v0 v0-type (id-type 100))
             (v1 v1-type (id-type 200))
             (graph (cons-graph
                      `((o :l (:v ,v0))
                        (o :r (:v ,v1)))))
             (o (get-roots graph 'o))
             (o-type (type-of o)))

    (u:mvlet ((c eql-map (clone-shallow-graph o (make-eql-map-with-stats))))
      (format t "Original | ~S~%" o)
      (format t "Cloned   | ~S~%" c)
      (finish-output)

      (assert (eql (car o) v0))
      (assert (eql (cdr o) v1))

      ;; The clone must have produced a new starting cons cell.
      (assert (not (eq c o)))

      ;; Modify the clone to see we don't modify the original.
      (setf (car c) 300
            (cdr c) 400)
      (format t "Modified Cloned   | ~S~%" c)
      ;; make sure didn't mess with original.
      (assert (eql (car o) v0))
      (assert (eql (cdr o) v1))
      ;; Ensure the change took in the clone.
      (assert (eql (car c) 300))
      (assert (eql (cdr c) 400))

      (eql-map-dump-stats eql-map)
      (validate-eql-map-stats
       (eql-map-stats-match-p eql-map
                              `(:move (,v0-type 1)
                                      (,v1-type 1))
                              `(:allocation (,o-type 1))))

      (format t "Passed.~%")
      t)))

(defun test-clone-shallow-graph-1 ()
  "Handle a cons cell whose car/cdr point to the same atomic thing."
  (u:mvlet* ((*print-circle* t)
             (v0 v0-type (id-type (make-hash-table)))
             (graph (cons-graph
                      `((o :l (:v ,v0))
                        (o :r (:v ,v0)))))
             (o (get-roots graph 'o))
             (o-type (type-of o)))

    (u:mvlet* ((c eql-map (clone-shallow-graph o (make-eql-map-with-stats))))
      (format t "Original | ~S~%" o)
      (format t "Cloned   | ~S~%" c)
      (finish-output)

      ;; Make sure original is still sane.
      (assert (eq (car o) v0))
      (assert (eq (cdr o) v0))

      ;; The clone must have produced a new starting cons cell.
      (assert (not (eq c o)))
      ;; But point to the original leaves.
      (assert (eq (car c) v0))
      (assert (eq (cdr c) v0))

      (eql-map-dump-stats eql-map)
      (validate-eql-map-stats
       (eql-map-stats-match-p eql-map
                              `(:move (,v0-type 2))
                              `(:allocation (,o-type 1))))

      (format t "Passed.~%")
      t)))

(defun test-clone-shallow-graph-2 ()
  "Handle a small tree made with cons cells."
  (u:mvlet* ((*print-circle* t)
             (v0 v0-type (id-type 1))
             (v1 v1-type (id-type 2))
             (v2 v2-type (id-type nil))
             (v3 v3-type (id-type 3))
             (v4 v4-type (id-type 4))
             (graph (cons-graph
                      `((:roots n0 n1 n2 n3)
                        (n0 :l n1)
                        (n0 :r n2)
                        (n1 :l (:v ,v0))
                        (n1 :r (:v ,v1))
                        (n2 :l n3)
                        (n2 :r (:v ,v2))
                        (n3 :l (:v ,v3))
                        (n3 :r (:v ,v4)))))
             (n0 n1 n2 n3 (get-roots graph 'n0 'n1 'n2 'n3))
             (n0-type (type-of n0)))

    (u:mvlet* ((c eql-map (clone-shallow-graph n0 (make-eql-map-with-stats)))
               (c0 c)
               (c1 (car c0))
               (c2 (cdr c0))
               (c3 (car c2))
               (vlc1 (car c1))
               (vrc1 (cdr c1))
               (vlc3 (car c3))
               (vrc3 (cdr c3)))
      (format t "Original | ~S~%" n0)
      (format t "Cloned   | ~S~%" c)
      (finish-output)

      ;; The clone must have produced a new starting cons cell.
      ;; Check all cons cells to ensure they are different.
      (assert (not (eq c0 n0)))
      (assert (not (eq c1 n1)))
      (assert (not (eq c2 n2)))
      (assert (not (eq c3 n3)))
      ;; Check that the actual values in the leaves are eql.
      (assert (eql (car n1) vlc1))
      (assert (eql (cdr n1) vrc1))
      (assert (eql (car n3) vlc3))
      (assert (eql (cdr n3) vrc3))

      (eql-map-dump-stats eql-map)
      (validate-eql-map-stats
       (eql-map-stats-match-p eql-map
                              `(:move (,v0-type 1)
                                      (,v1-type 1)
                                      (,v2-type 1)
                                      (,v3-type 1)
                                      (,v4-type 1))
                              `(:allocation (,n0-type 4))))

      (format t "Passed.~%")
      t)))

(defun test-clone-shallow-graph-3 ()
  "Handle a cons cell that references itself in both car and cdr."
  (u:mvlet* ((*print-circle* t)
             (graph (cons-graph
                      `((:roots o)
                        (o :l o)
                        (o :r o))))
             (o (get-roots graph 'o))
             (o-type (type-of o)))

    (u:mvlet* ((c eql-map (clone-shallow-graph o (make-eql-map-with-stats))))
      (format t "Original | ~S~%" o)
      (format t "Cloned   | ~S~%" c)
      (finish-output)

      ;; Check that the original has the property we desire.
      (assert (eq (car o) o))
      (assert (eq (cdr o) o))

      ;; The clone must have produced a new starting cons cell.
      (assert (not (eq c o)))
      ;; The car and cdr should have preserved the graph structure and point to
      ;; the NEW starting cons cell.
      (assert (eq (car c) c))
      (assert (eq (cdr c) c))

      (eql-map-dump-stats eql-map)
      (validate-eql-map-stats
       (eql-map-stats-match-p eql-map
                              `(:allocation (,o-type 1))))

      (format t "Passed.~%")
      t)))

(defun test-clone-shallow-graph-4 ()
  "Handle a directed acyclic graph with shared structure."
  (flet ((neq (x y)
           (not (eq x y))))
    (u:mvlet* ((*print-circle* t)
               (v0 v0-type (id-type 1))
               (v1 v1-type (id-type #P"/tmp/foo.txt"))
               (v2 v2-type (id-type 'a))
               (v3 v3-type (id-type "foo"))
               (v4 v4-type (id-type #\a))
               (v5 v5-type (id-type (make-hash-table)))
               (graph (cons-graph
                        `((:roots o oml omr obl obm obr)
                          (o :l oml :l obl :l (:v ,v0))
                          (o :r omr :r obr :r (:v ,v1))
                          (oml :r obm)
                          (omr :l obm)
                          (obl :r (:v ,v2))
                          (obm :l (:v ,v3))
                          (obm :r (:v ,v4))
                          (obr :l (:v ,v5)))))
               (obl obm obr oml omr o
                    (get-roots graph 'obl 'obm 'obr 'oml 'omr 'o))
               (o-type (type-of o))
               (c eql-map (clone-shallow-graph o (make-eql-map-with-stats))))

      (format t "Original | ~S~%" o)
      (format t "Cloned   | ~S~%" c)
      (finish-output)

      (let* ((cml (car c))
             (cmr (cdr c))
             (cbl (car cml))
             (cbm0 (cdr cml))
             (cbm1 (car cmr))
             (cbr (cdr cmr)))

        ;; Check that the cloned nodes exist
        (assert c)
        (assert cml)
        (assert cmr)
        (assert cbl)
        (assert cbm0)
        (assert cbm1)
        (assert cbr)

        ;; Check that the cloned nodes are actually different from their
        ;; counter parts
        (assert (neq o c))
        (assert (neq obl cbl))
        (assert (neq obm cbm0))
        (assert (neq obm cbm1))
        (assert (neq obr cbr))
        (assert (neq oml cbl))
        (assert (neq omr cmr))
        (assert (neq o c))

        ;; Check that the original graph's shared structure is present in
        ;; the clone.
        (assert (eq cbm0 cbm1))

        ;; Check that the leaves are what they purport to be (and are
        ;; identical to the original graph's leave due to shallow copy).
        (assert (eql (car obl) (car cbl)))
        (assert (eql (cdr obl) (cdr cbl)))
        (assert (eql (car obm) (car cbm0)))
        (assert (eql (cdr obm) (cdr cbm0)))
        (assert (eql (car obr) (car cbr)))
        (assert (eql (cdr obr) (cdr cbr)))

        (eql-map-dump-stats eql-map)
        (validate-eql-map-stats
         (eql-map-stats-match-p eql-map
                                `(:move (,v0-type 1)
                                        (,v1-type 1)
                                        (,v2-type 1)
                                        (,v3-type 1)
                                        (,v4-type 1)
                                        (,v5-type 1))
                                `(:allocation (,o-type 6))))

        (format t "Passed.~%")

        t))))

(defun test-clone-shallow-graph-5 ()
  "Handle a directed cyclic graph with complex shared structure."
  (flet ((neq (x y)
           (not (eq x y))))
    (u:mvlet* ((*print-circle* t)
               (graph (cons-graph
                        `((:roots l0 l1 l2 l3 l4)
                          ;; list structure with cycle
                          (l0 :r l1 :r l2 :r l3 :r l4 :r l0)
                          ;; complex shared and cyclic structure
                          (l0 :l l2)
                          (l1 :l l3)
                          (l2 :l l4)
                          (l3 :l l1)
                          (l4 :l l0))))
               (l0 l1 l2 l3 l4
                   (get-roots graph 'l0 'l1 'l2 'l3 'l4))
               (l0-type (type-of l0))
               (c eql-map (clone-shallow-graph l0 (make-eql-map-with-stats))))

      (format t "Original | ~S~%" l0)
      (format t "Cloned   | ~S~%" c)
      (finish-output)

      (let* ((c0 c)
             (c1 (cdr c0))
             (c2 (cdr c1))
             (c3 (cdr c2))
             (c4 (cdr c3)))

        ;; Check that the cloned nodes exist
        (assert c0)
        (assert c1)
        (assert c2)
        (assert c3)
        (assert c4)

        ;; Check that the cloned nodes are actually different from their
        ;; counter parts
        (assert (neq c0 l0))
        (assert (neq c1 l1))
        (assert (neq c2 l2))
        (assert (neq c3 l3))
        (assert (neq c4 l4))

        ;; Check that the graph structure is preserved.
        (assert (eql (car c0) c2))
        (assert (eql (car c1) c3))
        (assert (eql (car c2) c4))
        (assert (eql (car c3) c1))
        (assert (eql (car c4) c0))

        (eql-map-dump-stats eql-map)
        (validate-eql-map-stats
         (eql-map-stats-match-p eql-map
                                `(:allocation (,l0-type 5))))

        (format t "Passed.~%")

        t))))


;;; ------------------------------------------------------------
;; clone-shallow arrays of various types of ANY intention
;;; ------------------------------------------------------------

;; Utility function.
(defun assert-matching-array-properties (c o)
  ;; Ensure array properites are the same
  (assert (equal (array-dimensions c) (array-dimensions o)))
  (assert (equal (array-element-type c) (array-element-type o)))
  (assert (eql (adjustable-array-p c) (adjustable-array-p o)))
  (assert (eql (array-has-fill-pointer-p c) (array-has-fill-pointer-p o)))
  (when (array-has-fill-pointer-p c)
    (assert (eql (fill-pointer c) (fill-pointer o))))
  t)

(defun test-clone-shallow-array-simple-string-0 ()
  (u:mvlet* ((v0 v0-type (id-type #\a))
             ;; Type SIMPLE-STRING aka (SIMPLE-ARRAY CHARACTER *)
             (o o-type (id-type
                        (make-sequence 'simple-string 8 :initial-element v0)))
             ;; All intentions have the same behavior for arrays.
             (c eql-map (clone-shallow o (make-eql-map-with-stats))))

    (format t "Original | ~S~%" o)
    (format t "Cloned   | ~S~%" c)
    (finish-output)

    ;; Ensure it is newly allocated.
    (assert (not (eq c o)))

    (assert-matching-array-properties c o)

    ;; Then, ensure all index values were shallow copied.
    (dotimes (i (array-total-size c))
      (assert (eql (row-major-aref c i)
                   (row-major-aref o i))))

    (eql-map-dump-stats eql-map)
    (validate-eql-map-stats
     (eql-map-stats-match-p eql-map
                            `(:move (,v0-type 8))
                            `(:allocation (,o-type 1))))

    (format t "Passed.~%")
    t))



;; KEEP GOING add in more strenuous stats checking.
;; KEEP GOING adding in eql-map statistics.



(defun test-clone-shallow-array-simple-bit-vector-0 ()
  (let* (;; Type SIMPLE-BIT-VECTOR
         (o (make-sequence '(vector bit) 8 :initial-element 0))
         ;; All intentions have the same behavior for arrays.
         (c (clone-shallow o)))

    (format t "Original | ~S~%" o)
    (format t "Cloned   | ~S~%" c)
    (finish-output)

    ;; Ensure it is newly allocated.
    (assert (not (eq c o)))

    (assert-matching-array-properties c o)

    ;; Then, ensure all index values were shallow copied.
    (dotimes (i (array-total-size c))
      (assert (eql (row-major-aref c i)
                   (row-major-aref o i))))

    (format t "Passed.~%")
    t))

(defun test-clone-shallow-array-bit-vector-0 ()
  (let* (;; Type BIT-VECTOR
         (o (make-array 8 :element-type 'bit
                          :adjustable t
                          :initial-element 0))
         ;; All intentions have the same behavior for arrays.
         (c (clone-shallow o)))

    (format t "Original | ~S~%" o)
    (format t "Cloned   | ~S~%" c)
    (finish-output)

    ;; Ensure it is newly allocated.
    (assert (not (eq c o)))

    (assert-matching-array-properties c o)

    ;; Then, ensure all index values were shallow copied.
    (dotimes (i (array-total-size c))
      (assert (eql (row-major-aref c i)
                   (row-major-aref o i))))

    (format t "Passed.~%")
    t))

(defun test-clone-shallow-array-unique-simple-array-0 ()
  (let* (;; Type SIMPLE-ARRAY
         (o (make-array 3 :element-type '(unsigned-byte 8)
                          :initial-element 0))
         ;; All intentions have the same behavior for arrays.
         (c (clone-shallow o)))

    (format t "Original | ~S~%" o)
    (format t "Cloned   | ~S~%" c)
    (finish-output)

    ;; Ensure it is newly allocated.
    (assert (not (eq c o)))

    (assert-matching-array-properties c o)

    ;; Then, ensure all index values were shallow copied.
    (dotimes (i (array-total-size c))
      (assert (eql (row-major-aref c i)
                   (row-major-aref o i))))

    (format t "Passed.~%")
    t))

(defun test-clone-shallow-array-shared-simple-array-0 ()
  (let* ((item (cons 1 2))
         ;; Type SIMPLE-ARRAY
         (o (make-array 3 :element-type 'cons
                          :initial-element item))
         ;; All intentions have the same behavior for arrays.
         (c (clone-shallow o)))

    (format t "Original | ~S~%" o)
    (format t "Cloned   | ~S~%" c)
    (finish-output)

    ;; Ensure it is newly allocated.
    (assert (not (eq c o)))

    (assert-matching-array-properties c o)

    ;; Then, ensure all index values were shallow copied.
    (dotimes (i (array-total-size c))
      (assert (eql (row-major-aref c i)
                   (row-major-aref o i))))

    ;; Verify the shared structure in copy.
    (dotimes (i (array-total-size c))
      (assert (eql (row-major-aref c i)
                   (row-major-aref o (mod i (array-total-size c))))))

    (format t "Passed.~%")
    t))

(defun test-clone-shallow-array-unique-simple-vector-0 ()
  (let* (;; Type SIMPLE-VECTOR
         (o (make-array 3 :initial-element 0))
         ;; All intentions have the same behavior for arrays.
         (c (clone-shallow o)))

    (format t "Original | ~S~%" o)
    (format t "Cloned   | ~S~%" c)
    (finish-output)

    ;; Ensure it is newly allocated.
    (assert (not (eq c o)))

    (assert-matching-array-properties c o)

    ;; Then, ensure all index values were shallow copied.
    (dotimes (i (array-total-size c))
      (assert (eql (row-major-aref c i)
                   (row-major-aref o i))))

    (format t "Passed.~%")
    t))

(defun test-clone-shallow-array-shared-simple-vector-0 ()
  (let* ((item (cons 1 2))
         ;; Type SIMPLE-VECTOR
         (o (make-array 3 :initial-element item))
         ;; All intentions have the same behavior for arrays.
         (c (clone-shallow o)))

    (format t "Original | ~S~%" o)
    (format t "Cloned   | ~S~%" c)
    (finish-output)

    ;; Ensure it is newly allocated.
    (assert (not (eq c o)))

    (assert-matching-array-properties c o)

    ;; Then, ensure all index values were shallow copied.
    (dotimes (i (array-total-size c))
      (assert (eql (row-major-aref c i)
                   (row-major-aref o i))))

    ;; Verify the shared structure in copy.
    (dotimes (i (array-total-size c))
      (assert (eql (row-major-aref c i)
                   (row-major-aref o (mod i (array-total-size c))))))

    (format t "Passed.~%")
    t))

(defun test-clone-shallow-array-unique-vector-0 ()
  (let* (;; Type VECTOR
         (o (make-array 3 :adjustable t
                          :initial-element 0))
         ;; All intentions have the same behavior for arrays.
         (c (clone-shallow o)))

    (format t "Original | ~S~%" o)
    (format t "Cloned   | ~S~%" c)
    (finish-output)

    ;; Ensure it is newly allocated.
    (assert (not (eq c o)))

    (assert-matching-array-properties c o)

    ;; Then, ensure all index values were shallow copied.
    (dotimes (i (array-total-size c))
      (assert (eql (row-major-aref c i)
                   (row-major-aref o i))))

    (format t "Passed.~%")
    t))

(defun test-clone-shallow-array-shared-vector-0 ()
  (let* ((item (cons 1 2))
         ;; Type VECTOR
         (o (make-array 3 :adjustable t
                          :initial-element item))
         ;; All intentions have the same behavior for arrays.
         (c (clone-shallow o)))

    (format t "Original | ~S~%" o)
    (format t "Cloned   | ~S~%" c)
    (finish-output)

    ;; Ensure it is newly allocated.
    (assert (not (eq c o)))

    (assert-matching-array-properties c o)

    ;; Then, ensure all index values were shallow copied.
    (dotimes (i (array-total-size c))
      (assert (eql (row-major-aref c i)
                   (row-major-aref o i))))

    ;; Verify the shared structure in copy.
    (dotimes (i (array-total-size c))
      (assert (eql (row-major-aref c i)
                   (row-major-aref o (mod i (array-total-size c))))))

    (format t "Passed.~%")
    t))

(defun test-clone-shallow-array-unique-array-0 ()
  (let* (;; Type ARRAY
         (o (make-array '(3 4) :adjustable t
                               :initial-element 0))
         ;; All intentions have the same behavior for arrays.
         (c (clone-shallow o)))

    (format t "Original | ~S~%" o)
    (format t "Cloned   | ~S~%" c)
    (finish-output)

    ;; Ensure it is newly allocated.
    (assert (not (eq c o)))

    (assert-matching-array-properties c o)

    ;; Then, ensure all index values were shallow copied.
    (dotimes (i (array-total-size c))
      (assert (eql (row-major-aref c i)
                   (row-major-aref o i))))

    (format t "Passed.~%")
    t))

(defun test-clone-shallow-array-shared-array-0 ()
  (let* ((item (cons 1 2))
         ;; Type ARRAY
         (o (make-array '(3 4) :adjustable t
                               :initial-element item))
         ;; All intentions have the same behavior for arrays.
         (c (clone-shallow o)))

    (format t "Original | ~S~%" o)
    (format t "Cloned   | ~S~%" c)
    (finish-output)

    ;; Ensure it is newly allocated.
    (assert (not (eq c o)))

    (assert-matching-array-properties c o)

    ;; Then, ensure all index values were shallow copied.
    (dotimes (i (array-total-size c))
      (assert (eql (row-major-aref c i)
                   (row-major-aref o i))))

    ;; Verify the shared structure in copy.
    (dotimes (i (array-total-size c))
      (assert (eql (row-major-aref c i)
                   (row-major-aref o (mod i (array-total-size c))))))

    (format t "Passed.~%")
    t))

(defun test-clone-shallow-array-simple-base-string-0 ()
  (let* (;; Type SIMPLE-BASE-STRING
         (o (make-array 3 :element-type 'base-char
                          :initial-element #\a))
         ;; All intentions have the same behavior for arrays.
         (c (clone-shallow o)))

    (format t "Original | ~S~%" o)
    (format t "Cloned   | ~S~%" c)
    (finish-output)

    ;; Ensure it is newly allocated.
    (assert (not (eq c o)))

    (assert-matching-array-properties c o)

    ;; Then, ensure all index values were shallow copied.
    (dotimes (i (array-total-size c))
      (assert (eql (row-major-aref c i)
                   (row-major-aref o i))))

    (format t "Passed.~%")
    t))

(defun test-clone-shallow-array-base-string-0 ()
  (let* (;; Type BASE-STRING
         (o (make-array 3 :element-type 'base-char
                          :adjustable t
                          :initial-element #\a))
         ;; All intentions have the same behavior for arrays.
         (c (clone-shallow o)))

    (format t "Original | ~S~%" o)
    (format t "Cloned   | ~S~%" c)
    (finish-output)

    ;; Ensure it is newly allocated.
    (assert (not (eq c o)))

    (assert-matching-array-properties c o)

    ;; Then, ensure all index values were shallow copied.
    (dotimes (i (array-total-size c))
      (assert (eql (row-major-aref c i)
                   (row-major-aref o i))))

    (format t "Passed.~%")
    t))

(defun test-clone-shallow-array-unique-any-0 ()
  (let* ((contents '(nil (cons nil nil) 1 "hello" 'a 23.3 #\a
                     (u:dict #'equal) #P"/tmp/foo.txt"))
         (o (make-array (length contents) :initial-contents contents))
         ;; All intentions have the same behavior for arrays.
         (c (clone-shallow o)))

    (format t "Original | ~S~%" o)
    (format t "Cloned   | ~S~%" c)
    (finish-output)

    ;; Ensure it is newly allocated.
    (assert (not (eq c o)))

    (assert-matching-array-properties c o)

    ;; Then, ensure all index values were shallow copied.
    (dotimes (i (length c))
      (assert (eql (aref c i) (aref o i))))

    (format t "Passed.~%")
    t))

;;; ------------------------------------------------------------
;; clone-shallow hash-table of ANY intention
;;; ------------------------------------------------------------

;; helper to compare hash table properties.
(defun assert-matching-hash-table-properties (clone original)
  (let (;; Information from original hash table.
        (o-count (hash-table-count original))
        (o-test (hash-table-test original))
        (o-size (hash-table-size original))
        (o-rehash-size (hash-table-rehash-size original))
        (o-rehash-threshold (hash-table-rehash-threshold original))
        ;; Information from cloned hash table.
        (c-count (hash-table-count clone))
        (c-test (hash-table-test clone))
        (c-size (hash-table-size clone))
        (c-rehash-size (hash-table-rehash-size clone))
        (c-rehash-threshold (hash-table-rehash-threshold clone)))
    (assert (= c-count o-count))
    (assert (eq c-test o-test))
    (assert (= c-size o-size))
    (assert (= c-rehash-size o-rehash-size))
    (assert (= c-rehash-threshold o-rehash-threshold))
    t))

(defun test-clone-shallow-hash-table-0 ()
  (let* ((o (u:dict))
         (c (clone-shallow o)))

    (format t "Original | ~S~%" o)
    (format t "Cloned   | ~S~%" c)
    (finish-output)

    (assert (not (eq c o)))

    (assert-matching-hash-table-properties c o)

    (format t "Passed.~%")
    t))

(defun test-clone-shallow-hash-table-1 ()
  (let* ((key 'foo)
         (value 'bar)
         (o (u:dict #'eq key value))
         (c (clone-shallow o)))

    (format t "Original | ~S~%" o)
    (format t "Cloned   | ~S~%" c)
    (finish-output)

    ;; Ensure it is newly allocated
    (assert (not (eq c o)))

    (assert-matching-hash-table-properties c o)

    ;; Ensure the values exist in the clone.
    (assert (u:href c key))
    (assert (eq (u:href c key) value))

    (format t "Passed.~%")
    t))

(defun test-clone-shallow-hash-table-2 ()
  (let* ((key (list 1 2 3))
         (value (list 4 5 6))
         (o (u:dict #'equal key value))
         (c (clone-shallow o)))

    (format t "Original | ~S~%" o)
    (format t "Cloned   | ~S~%" c)
    (finish-output)

    (assert (not (eq c o)))

    (assert-matching-hash-table-properties c o)

    ;; Ensure the values exist in the clone and the expected shared structure
    ;; too.
    (assert (u:href c key))
    (assert (equal (u:href c key) value))
    (let ((o-key nil)
          (o-value nil)
          (c-key nil)
          (c-value nil))

      (u:do-hash (k v o)
        (push k o-key)
        (push v o-value))

      (u:do-hash (k v c)
        (push k c-key)
        (push v c-value))

      ;; The keys must be identical in the shallow copy.
      (assert (eq (first o-key) (first c-key)))
      ;; The values must be identical in the shallow copy.
      (assert (eq (first o-value) (first c-value)))

      (format t "Passed.~%")
      t)))

;;; ------------------------------------------------------------
;; Aggregate shallow cloning testing code.
;;; ------------------------------------------------------------

(defun test-clone-shallow ()
  (format t "Shallow clone tests.~%")

  (test-clone-shallow-function-0)
  (test-clone-shallow-character-0)
  (test-clone-shallow-pathname-0)
  (test-clone-shallow-symbol-0)
  (test-clone-shallow-integer-0)

  (test-clone-shallow-cons-0)
  (test-clone-shallow-cons-1)
  (test-clone-shallow-list-0)
  (test-clone-shallow-list-1)
  (test-clone-shallow-list-2)
  (test-clone-shallow-list-3)
  (test-clone-shallow-list-4)
  (test-clone-shallow-alist-0)
  (test-clone-shallow-alist-1)
  (test-clone-shallow-alist-2)
  (test-clone-shallow-alist-3)
  (test-clone-shallow-alist-4)
  (test-clone-shallow-alist-5)
  (test-clone-shallow-graph-0)
  (test-clone-shallow-graph-1)
  (test-clone-shallow-graph-2)
  (test-clone-shallow-graph-3)
  (test-clone-shallow-graph-4)
  (test-clone-shallow-graph-5)

  (test-clone-shallow-array-simple-string-0)
  (test-clone-shallow-array-simple-bit-vector-0)
  (test-clone-shallow-array-bit-vector-0)
  (test-clone-shallow-array-unique-simple-array-0)
  (test-clone-shallow-array-shared-simple-array-0)
  (test-clone-shallow-array-unique-simple-vector-0)
  (test-clone-shallow-array-shared-simple-vector-0)
  (test-clone-shallow-array-unique-vector-0)
  (test-clone-shallow-array-shared-vector-0)
  (test-clone-shallow-array-unique-array-0)
  (test-clone-shallow-array-shared-array-0)
  (test-clone-shallow-array-simple-base-string-0)
  (test-clone-shallow-array-base-string-0)
  (test-clone-shallow-array-unique-any-0)

  (test-clone-shallow-hash-table-0)
  (test-clone-shallow-hash-table-1)
  (test-clone-shallow-hash-table-2)

  (format t "Shallow clone tests passed!~%")

  t)

;;; ---------------------------------------------------------------------------
;; DEEP cloning tests.
;;; ---------------------------------------------------------------------------

;;; ------------------------------------------------------------
;; clone-deep functions of ANY intention
;;; ------------------------------------------------------------

(defun test-clone-deep-function-0 ()
  (let* (;; Type FUNCTION
         (o #'cl:identity)
         (c (clone-deep o)))

    (format t "Original | ~S~%" o)
    (format t "Cloned   | ~S~%" c)
    (finish-output)

    ;; Should return itself.
    (assert (eq o c))

    (format t "Passed.~%")
    t))

;;; ------------------------------------------------------------
;; clone-deep character of ANY intention
;;; ------------------------------------------------------------

(defun test-clone-deep-character-0 ()
  (let* (;; Type CHARACTER
         (o #\a)
         (c (clone-deep o)))

    (format t "Original | ~S~%" o)
    (format t "Cloned   | ~S~%" c)
    (finish-output)

    ;; Should return itself.
    (assert (eql o c))

    (format t "Passed.~%")
    t))

;;; ------------------------------------------------------------
;; clone-deep pathname of ANY intention
;;; ------------------------------------------------------------

(defun test-clone-deep-pathname-0 ()
  (let* (;; Type PATHNAME
         (o #P"/tmp/foo.txt")
         (c (clone-deep o)))

    (format t "Original | ~S~%" o)
    (format t "Cloned   | ~S~%" c)
    (finish-output)

    ;; Should return itself.
    (assert (equal o c))

    (format t "Passed.~%")
    t))

;;; ------------------------------------------------------------
;; clone-deep symbol of ANY intention
;;; ------------------------------------------------------------

(defun test-clone-deep-symbol-0 ()
  (let* (;; Type SYMBOL
         (o 'a)
         (c (clone-deep o)))

    (format t "Original | ~S~%" o)
    (format t "Cloned   | ~S~%" c)
    (finish-output)

    ;; Should return itself.
    (assert (eq o c))

    (format t "Passed.~%")
    t))

;;; ------------------------------------------------------------
;; clone-deep integer of ANY intention
;;; ------------------------------------------------------------

(defun test-clone-deep-integer-0 ()
  (let* (;; Type INTEGER
         (o 42)
         (c (clone-deep o)))

    (format t "Original | ~S~%" o)
    (format t "Cloned   | ~S~%" c)
    (finish-output)

    ;; Should return itself.
    (assert (eql o c))

    (format t "Passed.~%")
    t))

;;; ------------------------------------------------------------
;; clone-deep cons.
;;
;; NOTE: The graphs only use cons cells and identity-clone values.
;;; ------------------------------------------------------------

(defun test-clone-deep-cons-0 ()
  (let* ((o (cons 1 2))
         (c (clone-deep o)))

    (format t "Original | ~S~%" o)
    (format t "Cloned   | ~S~%" c)
    (finish-output)

    ;; This part must be cloned.
    (assert (not (eq o c)))

    ;; but the values of the car and cdr must still be eq/eql to original
    ;; since they are identity clone objects.
    (assert (eql (car c) (car o)))
    (assert (eql (cdr c) (cdr o)))

    (format t "Passed.~%")
    t))

(defun test-clone-deep-cons-1 ()
  (let* ((*print-circle* t)
         (o (cons nil nil)))
    ;; Both car and cdr point to o.
    (setf (car o) o
          (cdr o) o)

    (let ((c (clone-deep o)))
      (format t "Original | ~S~%" o)
      (format t "Cloned   | ~S~%" c)
      (finish-output)

      ;; Ensure new memory
      (assert (not (eq o c)))

      ;; Ensure structure is deep cloned.
      (assert (eq (car c) c))
      (assert (eq (cdr c) c))

      (format t "Passed.~%")
      t)))

(defun test-clone-deep-cons-2 ()
  (u:mvlet* ((*print-circle* t)
             (graph (cons-graph
                      '((:roots n0 n1 n2 n3)
                        (n0 :r n1 :r n2 :r n3 :r (:v nil))
                        (n0 :l (:v 1))
                        (n1 :l (:v #\a))
                        (n2 :l (:v #'+))
                        (n3 :l (:v '+)))))
             (n0 n1 n2 n3
                 (get-roots graph 'n0 'n1 'n2 'n3)))

    (let ((c (clone-deep n0)))
      (format t "Original | ~S~%" n0)
      (format t "Cloned   | ~S~%" c)
      (finish-output)

      ;; Ensure new memory
      (assert (not (eq n0 c)))

      ;; Ensure that each cons cell was cloned.
      (loop :for o-cell :on n0
            :for c-cell :on c
            :do (assert (not (eq o-cell c-cell))))

      (format t "Passed.~%")
      t)))

(defun test-clone-deep-cons-3 ()
  "Hideous forward referencing shared structure with cycles."
  (u:mvlet* ((*print-circle* t)
             (graph (cons-graph
                      '((:roots n0 n1 n2 n3)
                        (n0 :r n1 :r n2 :r n3 :r n0)
                        (n0 :l n1)
                        (n1 :l n2)
                        (n2 :l n3)
                        (n3 :l n0))))
             (n0 n1 n2 n3
                 (get-roots graph 'n0 'n1 'n2 'n3)))

    (let ((c (clone-deep n0)))
      (format t "Original | ~S~%" n0)
      (format t "Cloned   | ~S~%" c)
      (finish-output)

      ;; Ensure new memory
      (assert (not (eq n0 c)))

      (let* ((c0 c)
             (c1 (cdr c0))
             (c2 (cdr c1))
             (c3 (cdr c2)))
        ;; Ensure that each cons cell was cloned.
        (mapc (lambda (x y)
                (assert (not (eq x y))))
              (list n0 n1 n2 n3)
              (list c0 c1 c2 c3))
        ;; Ensure the structure of the graph was preserved.
        ;; 1. cyclic list structure
        (assert (eq (cdr c0) c1))
        (assert (eq (cdr c1) c2))
        (assert (eq (cdr c2) c3))
        (assert (eq (cdr c3) c0))
        ;; 2. forward references
        (assert (eq (car c0) c1))
        (assert (eq (car c1) c2))
        (assert (eq (car c2) c3))
        (assert (eq (car c3) c0))

        (format t "Passed.~%")
        t))))


(defun test-clone-deep-cons-4 ()
  "Hideous backwards referencing shared structure with cycles."
  (u:mvlet* ((*print-circle* t)
             (graph (cons-graph
                      '((:roots n0 n1 n2 n3)
                        (n0 :r n1 :r n2 :r n3 :r n0)
                        (n0 :l n3)
                        (n1 :l n0)
                        (n2 :l n1)
                        (n3 :l n2))))
             (n0 n1 n2 n3
                 (get-roots graph 'n0 'n1 'n2 'n3)))

    (let ((c (clone-deep n0)))
      (format t "Original | ~S~%" n0)
      (format t "Cloned   | ~S~%" c)
      (finish-output)

      ;; Ensure new memory
      (assert (not (eq n0 c)))

      (let* ((c0 c)
             (c1 (cdr c0))
             (c2 (cdr c1))
             (c3 (cdr c2)))
        ;; Ensure that each cons cell was cloned.
        (mapc (lambda (x y)
                (assert (not (eq x y))))
              (list n0 n1 n2 n3)
              (list c0 c1 c2 c3))
        ;; Ensure the structure of the graph was preserved.
        ;; 1. cyclic list structure
        (assert (eq (cdr c0) c1))
        (assert (eq (cdr c1) c2))
        (assert (eq (cdr c2) c3))
        (assert (eq (cdr c3) c0))
        ;; 2. backward references
        (assert (eq (car c0) c3))
        (assert (eq (car c1) c0))
        (assert (eq (car c2) c1))
        (assert (eq (car c3) c2))

        (format t "Passed.~%")
        t))))

(defun test-clone-deep-cons-5 ()
  "Hideous forward and backward referencing shared structure with cycles."
  (u:mvlet* ((*print-circle* t)
             (graph (cons-graph
                      '((:roots n0 n1 n2 n3)
                        (n0 :r n1 :r n2 :r n3 :r n0)
                        (n0 :l n2)
                        (n1 :l n3)
                        (n2 :l n0)
                        (n3 :l n1))))
             (n0 n1 n2 n3
                 (get-roots graph 'n0 'n1 'n2 'n3)))

    (let ((c (clone-deep n0)))
      (format t "Original | ~S~%" n0)
      (format t "Cloned   | ~S~%" c)
      (finish-output)

      ;; Ensure new memory
      (assert (not (eq n0 c)))

      (let* ((c0 c)
             (c1 (cdr c0))
             (c2 (cdr c1))
             (c3 (cdr c2)))
        ;; Ensure that each cons cell was cloned.
        (mapc (lambda (x y)
                (assert (not (eq x y))))
              (list n0 n1 n2 n3)
              (list c0 c1 c2 c3))
        ;; Ensure the structure of the graph was preserved.
        ;; 1. cyclic list structure
        (assert (eq (cdr c0) c1))
        (assert (eq (cdr c1) c2))
        (assert (eq (cdr c2) c3))
        (assert (eq (cdr c3) c0))
        ;; 2. backward references
        (assert (eq (car c0) c2))
        (assert (eq (car c1) c3))
        (assert (eq (car c2) c0))
        (assert (eq (car c3) c1))

        (format t "Passed.~%")
        t))))


(defun test-clone-deep-cons-6 ()
  "Irreducible loop with shared structure and cycles."
  (u:mvlet* ((*print-circle* t)
             (graph (cons-graph
                      '((:roots n0 n1 n2)
                        (n0 :l n1)
                        (n0 :r n2)
                        (n1 :l (:v 1))
                        (n1 :r n2)
                        (n2 :l n1)
                        (n2 :r n1))))
             (n0 n1 n2
                 (get-roots graph 'n0 'n1 'n2)))

    (let ((c (clone-deep n0)))
      (format t "Original | ~S~%" n0)
      (format t "Cloned   | ~S~%" c)
      (finish-output)

      ;; Ensure new memory
      (assert (not (eq n0 c)))

      (let* ((c0 c)
             (c1 (car c0))
             (c2 (cdr c0)))
        ;; Ensure that each cons cell was cloned.
        (mapc (lambda (x y)
                (assert (not (eq x y))))
              (list n0 n1 n2)
              (list c0 c1 c2))
        ;; Ensure the structure of the graph was preserved.
        (assert (eq (car c0) c1))
        (assert (eq (cdr c0) c2))

        (assert (eql (car c1) 1))
        (assert (eq (cdr c1) c2))

        (assert (eq (car c2) c1))
        (assert (eq (cdr c2) c1))

        (format t "Passed.~%")
        t))))


;;; ------------------------------------------------------------
;; clone-deep arrays.
;;
;; NOTE: The graphs only use arrays, cons cells, and identity-clone values.
;;; ------------------------------------------------------------

(defun test-clone-deep-array-simple-string-0 ()
  (let* (;; Type SIMPLE-STRING aka (SIMPLE-ARRAY CHARACTER *)
         (o (make-sequence 'simple-string 8 :initial-element #\a))
         ;; All intentions have the same behavior for arrays.
         (c (clone-deep o)))

    (format t "Original | ~S~%" o)
    (format t "Cloned   | ~S~%" c)
    (finish-output)

    ;; Ensure it is newly allocated.
    (assert (not (eq c o)))

    (assert-matching-array-properties c o)

    ;; Then, ensure all index values were deep copied.
    ;; In this case, the characters will be EQL to each other.
    (dotimes (i (array-total-size c))
      (assert (eql (row-major-aref c i)
                   (row-major-aref o i))))

    (format t "Passed.~%")
    t))

(defun test-clone-deep-array-simple-bit-vector-0 ()
  (let* (;; Type SIMPLE-BIT-VECTOR
         (o (make-sequence '(vector bit) 8 :initial-element 0))
         ;; All intentions have the same behavior for arrays.
         (c (clone-deep o)))

    (format t "Original | ~S~%" o)
    (format t "Cloned   | ~S~%" c)
    (finish-output)

    ;; Ensure it is newly allocated.
    (assert (not (eq c o)))

    (assert-matching-array-properties c o)

    ;; Then, ensure all index values were deep copied.
    ;; In this case, the bits will be EQL to each other.
    (dotimes (i (array-total-size c))
      (assert (eql (row-major-aref c i)
                   (row-major-aref o i))))

    (format t "Passed.~%")
    t))

(defun test-clone-deep-array-bit-vector-0 ()
  (let* (;; Type BIT-VECTOR
         (o (make-array 8 :element-type 'bit
                          :adjustable t
                          :initial-element 0))
         ;; All intentions have the same behavior for arrays.
         (c (clone-deep o)))

    (format t "Original | ~S~%" o)
    (format t "Cloned   | ~S~%" c)
    (finish-output)

    ;; Ensure it is newly allocated.
    (assert (not (eq c o)))

    (assert-matching-array-properties c o)

    ;; Then, ensure all index values were deep copied.
    ;; In this case, the bits will be EQL to each other.
    (dotimes (i (array-total-size c))
      (assert (eql (row-major-aref c i)
                   (row-major-aref o i))))

    (format t "Passed.~%")
    t))

(defun test-clone-deep-array-unique-simple-array-0 ()
  (let* (;; Type SIMPLE-ARRAY
         (o (make-array 3 :element-type '(unsigned-byte 8)
                          :initial-element 0))
         ;; All intentions have the same behavior for arrays.
         (c (clone-deep o)))

    (format t "Original | ~S~%" o)
    (format t "Cloned   | ~S~%" c)
    (finish-output)

    ;; Ensure it is newly allocated.
    (assert (not (eq c o)))

    (assert-matching-array-properties c o)

    ;; Then, ensure all index values were deep copied.
    ;; In this case, the numbers will be EQL to each other.
    (dotimes (i (array-total-size c))
      (assert (eql (row-major-aref c i)
                   (row-major-aref o i))))

    (format t "Passed.~%")
    t))

(defun test-clone-deep-array-shared-simple-array-0 ()
  (let* ((item (cons 1 2))
         ;; Type SIMPLE-ARRAY
         (o (make-array 3 :element-type 'cons
                          :initial-element item))
         ;; All intentions have the same behavior for arrays.
         (c (clone-deep o)))

    (format t "Original | ~S~%" o)
    (format t "Cloned   | ~S~%" c)
    (finish-output)

    ;; Ensure it is newly allocated.
    (assert (not (eq c o)))

    (assert-matching-array-properties c o)

    ;; Then, ensure all index values were deep copied.
    (dotimes (i (array-total-size c))
      (assert (not (eql (row-major-aref c i)
                        (row-major-aref o i)))))

    ;; Also verify the shared structure in copy.
    (dotimes (i (array-total-size c))
      (assert (eql (row-major-aref c i)
                   (row-major-aref c (mod i (array-total-size c))))))

    (format t "Passed.~%")
    t))

(defun test-clone-deep-array-unique-simple-vector-0 ()
  (let* (;; Type SIMPLE-VECTOR
         (o (make-array 3 :initial-element 0))
         ;; All intentions have the same behavior for arrays.
         (c (clone-deep o)))

    (format t "Original | ~S~%" o)
    (format t "Cloned   | ~S~%" c)
    (finish-output)

    ;; Ensure it is newly allocated.
    (assert (not (eq c o)))

    (assert-matching-array-properties c o)

    ;; Then, ensure all index values were deep copied.
    ;; In this case, the numbers (which are identity copied)
    ;; are checked against the original).
    (dotimes (i (array-total-size c))
      (assert (eql (row-major-aref c i)
                   (row-major-aref o i))))

    (format t "Passed.~%")
    t))

(defun test-clone-deep-array-shared-simple-vector-0 ()
  (let* ((item (cons 1 2))
         ;; Type SIMPLE-VECTOR
         (o (make-array 3 :initial-element item))
         ;; All intentions have the same behavior for arrays.
         (c (clone-deep o)))

    (format t "Original | ~S~%" o)
    (format t "Cloned   | ~S~%" c)
    (finish-output)

    ;; Ensure it is newly allocated.
    (assert (not (eq c o)))

    (assert-matching-array-properties c o)

    ;; Then, ensure all index values were deep copied.
    (dotimes (i (array-total-size c))
      (assert (not (eql (row-major-aref c i)
                        (row-major-aref o i)))))

    ;; Verify the shared structure in copy.
    (dotimes (i (array-total-size c))
      (assert (eql (row-major-aref c i)
                   (row-major-aref c (mod i (array-total-size c))))))

    (format t "Passed.~%")
    t))

(defun test-clone-deep-array-unique-vector-0 ()
  (let* (;; Type VECTOR
         (o (make-array 3 :adjustable t
                          :initial-element 0))
         ;; All intentions have the same behavior for arrays.
         (c (clone-deep o)))

    (format t "Original | ~S~%" o)
    (format t "Cloned   | ~S~%" c)
    (finish-output)

    ;; Ensure it is newly allocated.
    (assert (not (eq c o)))

    (assert-matching-array-properties c o)

    ;; Then, ensure all index values were deep copied.
    ;; NOTE: The numbers are identity cloned, so we can check against
    ;; the original.
    (dotimes (i (array-total-size c))
      (assert (eql (row-major-aref c i)
                   (row-major-aref o i))))

    (format t "Passed.~%")
    t))

(defun test-clone-deep-array-shared-vector-0 ()
  (let* ((item (cons 1 2))
         ;; Type VECTOR
         (o (make-array 3 :adjustable t
                          :initial-element item))
         ;; All intentions have the same behavior for arrays.
         (c (clone-deep o)))

    (format t "Original | ~S~%" o)
    (format t "Cloned   | ~S~%" c)
    (finish-output)

    ;; Ensure it is newly allocated.
    (assert (not (eq c o)))

    (assert-matching-array-properties c o)

    ;; Then, ensure all index values were deep copied.
    (dotimes (i (array-total-size c))
      (assert (not (eql (row-major-aref c i)
                        (row-major-aref o i)))))

    ;; Verify the shared structure in copy.
    (dotimes (i (array-total-size c))
      (assert (eql (row-major-aref c i)
                   (row-major-aref c (mod i (array-total-size c))))))

    (format t "Passed.~%")
    t))

(defun test-clone-deep-array-unique-array-0 ()
  (let* (;; Type ARRAY
         (o (make-array '(3 4) :adjustable t
                               :initial-element 0))
         ;; All intentions have the same behavior for arrays.
         (c (clone-deep o)))

    (format t "Original | ~S~%" o)
    (format t "Cloned   | ~S~%" c)
    (finish-output)

    ;; Ensure it is newly allocated.
    (assert (not (eq c o)))

    (assert-matching-array-properties c o)

    ;; Then, ensure all index values were deep copied.
    ;; NOTE: They are numbers which are identity copied, so we
    ;; can check them against the original.
    (dotimes (i (array-total-size c))
      (assert (eql (row-major-aref c i)
                   (row-major-aref o i))))

    (format t "Passed.~%")
    t))

(defun test-clone-deep-array-shared-array-0 ()
  (let* ((item (cons 1 2))
         ;; Type ARRAY
         (o (make-array '(3 4) :adjustable t
                               :initial-element item))
         ;; All intentions have the same behavior for arrays.
         (c (clone-deep o)))

    (format t "Original | ~S~%" o)
    (format t "Cloned   | ~S~%" c)
    (finish-output)

    ;; Ensure it is newly allocated.
    (assert (not (eq c o)))

    (assert-matching-array-properties c o)

    ;; Then, ensure all index values were shallow copied.
    (dotimes (i (array-total-size c))
      (assert (not (eql (row-major-aref c i)
                        (row-major-aref o i)))))

    ;; Verify the shared structure in copy.
    (dotimes (i (array-total-size c))
      (assert (eql (row-major-aref c i)
                   (row-major-aref c (mod i (array-total-size c))))))

    (format t "Passed.~%")
    t))

(defun test-clone-deep-array-simple-base-string-0 ()
  (let* (;; Type SIMPLE-BASE-STRING
         (o (make-array 3 :element-type 'base-char
                          :initial-element #\a))
         ;; All intentions have the same behavior for arrays.
         (c (clone-deep o)))

    (format t "Original | ~S~%" o)
    (format t "Cloned   | ~S~%" c)
    (finish-output)

    ;; Ensure it is newly allocated.
    (assert (not (eq c o)))

    (assert-matching-array-properties c o)

    ;; Then, ensure all index values were deep copied.
    ;; NOTE: Characters are identity copied, so we can just check
    ;; against the original.
    (dotimes (i (array-total-size c))
      (assert (eql (row-major-aref c i)
                   (row-major-aref o i))))

    (format t "Passed.~%")
    t))

(defun test-clone-deep-array-base-string-0 ()
  (let* (;; Type BASE-STRING
         (o (make-array 3 :element-type 'base-char
                          :adjustable t
                          :initial-element #\a))
         ;; All intentions have the same behavior for arrays.
         (c (clone-deep o)))

    (format t "Original | ~S~%" o)
    (format t "Cloned   | ~S~%" c)
    (finish-output)

    ;; Ensure it is newly allocated.
    (assert (not (eq c o)))

    (assert-matching-array-properties c o)

    ;; Then, ensure all index values were shallow copied.
    ;; NOTE: We're checking against characters which are identity
    ;; copied, so we can look at the original.
    (dotimes (i (array-total-size c))
      (assert (eql (row-major-aref c i)
                   (row-major-aref o i))))

    (format t "Passed.~%")
    t))

;;; ------------------------------------------------------------
;; clone-deep hash-tables.
;;
;; NOTE: The graphs only use arrays, cons cells, and identity-clone values.
;;; ------------------------------------------------------------

(defun test-clone-deep-hash-table-0 ()
  "Can an empty hash table be deep copied and preserve its properties?"
  (let* ((o (u:dict))
         (c (clone-deep o)))

    (format t "Original | ~S~%" o)
    (format t "Cloned   | ~S~%" c)
    (finish-output)

    (assert (not (eq c o)))
    (assert-matching-hash-table-properties c o)

    (format t "Passed.~%")
    t))

(defun test-clone-deep-hash-table-1 ()
  "keys and values share no structure and are identity-policy style objects."
  (let* ((k0 0)
         (v0 1)
         (k1 #\a)
         (v1 #\b)
         (k2 'foo)
         (v2 'bar)
         (k3 (lambda () 100))
         (v3 (lambda () 200))
         (k4 #P"/tmp/foo.txt")
         (v4 #P"/tmp/bar.txt")
         (o (u:dict #'eql k0 v0 k1 v1 k2 v2 k3 v3 k4 v4))
         (c (clone-deep o)))

    (format t "Original | ~S~%" o)
    (format t "Cloned   | ~S~%" c)
    (finish-output)

    ;; Ensure it is newly allocated
    (assert (not (eq c o)))
    (assert-matching-hash-table-properties c o)

    ;; Ensure the keys values are what they should be.  These tests are a
    ;; little odd, but transitively, they do the intention of equality of the
    ;; source keys and values between themselves and their initial values in
    ;; the LET bindings. It is written like this because the hash tables can
    ;; iterate over the key/value pairs in any order.
    (let ((all-keys (list k0 k1 k2 k3 k4))
          (all-values (list v0 v1 v2 v3 v4)))
      (u:do-hash (k v o)
        (assert (some (u:curry 'eql k) all-keys))
        (assert (some (u:curry 'eql v) all-values)))

      (u:do-hash (k v c)
        (assert (some (u:curry 'eql k) all-keys))
        (assert (some (u:curry 'eql v) all-values)))

      (format t "Passed.~%")
      t)))

;; KEEP GOING (add more tests for sure).

;; TODO: Broken (because not converted to deep clone semantics).
(defun test-clone-deep-hash-table-2 ()
  (let* ((key (list 1 2 3))
         (value (list 4 5 6))
         (o (u:dict #'equal key value))
         (eql-map (make-eql-map :stats-alloc (u:dict #'equal)))
         (c (clone-deep o eql-map)))

    (format t "Original | ~S~%" o)
    (format t "Cloned   | ~S~%" c)
    (finish-output)

    (assert (not (eq c o)))
    (assert-matching-hash-table-properties c o)

    ;; Ensure the values exist in the clone and the expected shared structure
    ;; too.
    (assert (u:href c key))
    (assert (equal (u:href c key) value))
    (let ((o-key nil)
          (o-value nil)
          (c-key nil)
          (c-value nil))

      (u:do-hash (k v o)
        (push k o-key)
        (push v o-value))

      (u:do-hash (k v c)
        (push k c-key)
        (push v c-value))

      ;; The keys must be identical in the shallow copy.
      (assert (eq (first o-key) (first c-key)))
      ;; The values must be identical in the shallow copy.
      (assert (eq (first o-value) (first c-value)))

      (format t "Passed.~%")
      t)))


;;; ------------------------------------------------------------
;; clone-deep complex combinations of types and self-referential structure
;; including cycles.
;;
;; NOTE: The graphs are allowed to use any one of the following types:
;;
;; numbers, characters, symbols, functions, pathnames, cons cells, arrays (of
;; any type), and hash tables.
;;
;; It is intended to ensure that complex graphs of various objects that refer
;; to each other can be correctly deep cloned.
;;; ------------------------------------------------------------



;;; ------------------------------------------------------------
;; Aggregate deep cloning testing code.
;;; ------------------------------------------------------------

(defun test-clone-deep ()
  (format t "Deep clone tests.~%")

  ;; identity values that are always themselves.
  (test-clone-deep-function-0)
  (test-clone-deep-character-0)
  (test-clone-deep-pathname-0)
  (test-clone-deep-symbol-0)
  (test-clone-deep-integer-0)

  ;; The graphs are only cons cells and identity-like values.
  (test-clone-deep-cons-0)
  (test-clone-deep-cons-1)
  (test-clone-deep-cons-2)
  (test-clone-deep-cons-3)
  (test-clone-deep-cons-4)
  (test-clone-deep-cons-5)
  (test-clone-deep-cons-6)

  ;; The arrays may hold references to themselves, to nested arrays, to cons
  ;; cells, and to identity-like values.
  (test-clone-deep-array-simple-string-0)
  (test-clone-deep-array-simple-bit-vector-0)
  (test-clone-deep-array-bit-vector-0)
  (test-clone-deep-array-unique-simple-array-0)
  (test-clone-deep-array-shared-simple-array-0)
  (test-clone-deep-array-unique-simple-vector-0)
  (test-clone-deep-array-shared-simple-vector-0)
  (test-clone-deep-array-unique-vector-0)
  (test-clone-deep-array-shared-vector-0)
  (test-clone-deep-array-unique-array-0)
  (test-clone-deep-array-shared-array-0)
  (test-clone-deep-array-simple-base-string-0)
  (test-clone-deep-array-base-string-0)
  ;; TODO: Do more of the array ones related to graph/self-referential
  ;; structure.

  ;; These hash tables may use anything applicable as keys and values,
  ;; including self-referential structure and cycles.
  (test-clone-deep-hash-table-0)
  (test-clone-deep-hash-table-1)

  (format t "Deep clone tests passed!~%")
  t)


;;; ---------------------------------------------------------------------------
;; NEW TYPE cloning tests.
;;
;; Herein we construct a couple new types and make shallow and deep clones of
;; them to ensure things work out.
;;; ---------------------------------------------------------------------------


;;; ------------------------------------------------------------
;; Aggregate NEW TYPE cloning testing code.
;;; ------------------------------------------------------------

(defun test-clone-new-type ()
  (format t "New Type clone tests.~%")

  (format t "New Type clone tests passed!~%")
  t)


;;; ---------------------------------------------------------------------------
;; Entry point for entire clone system test.
;;; ---------------------------------------------------------------------------

(defun test-clone ()
  (test-clone-shallow)
  (test-clone-deep)
  (test-clone-new-type))
