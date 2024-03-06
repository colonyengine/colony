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

(defun test-cons-graph ()
  (let* ((*print-circle* t)
         (dsl0 '((a :l (:v 1))
                 (a :r b :r (:v 100))
                 (b :l a)
                 (:roots a)))
         (dsl0-graph (cons-graph dsl0)))
    (flet ((emit-graph (desc dsl graph)
             (format t "~A forms:~% ~{~S~% ~}~%" desc dsl)
             (format t "~A graph roots:~% ~{~S ~}~%" desc graph)))
      (emit-graph "DSL0" dsl0 dsl0-graph)
      )))

;;; ------------------------------------------------------------
;; Testing the EQL-MAP API
;;; ------------------------------------------------------------

;; TODO: This actually doesn't yet smoke test the EQL-MAP object. Tsk Tsk!
;; TODO: Write actual unit tests for the EQL-MAP interface itself.
(defun test-eql-map ()
  "This should smoke test the EQL-MAP."
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
    (eql-map-dump eql-map)
    t))

;;; ---------------------------------------------------------------------------
;; Helper functions for the tests.
;;; ---------------------------------------------------------------------------

(defun id-type (val)
  "Return two values. The first value is VAL and the second value is the type
of VAL."
  (values val (type-of val)))

(defun map-type-of (&rest objects)
  "Return N values of the TYPE-OF each object in OBJECTS in left to right
order."
  (apply #'values (mapcar #'type-of objects)))

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
                            `(:move-original (,o-type 1))))

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
                            `(:move-original (,o-type 1))))

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
     (eql-map-stats-match-p eql-map `(:move-original (,o-type 1))))

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
                            `(:move-original (,o-type 1))))

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
                            `(:move-original (,o-type 1))))

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
                            `(:move-original (,v0-type 1) (,v1-type 1))
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
                              `(:move-original (,o-type 2))
                              `(:allocation (,o-type 1))))

      (format t "Passed (with expected surprise due to cycle).~%")
      t)))

;;; ------------------------------------------------------------
;; clone-shallow cons of LIST intention
;;; ------------------------------------------------------------

(defun tree-unshared-cons-p (t0 t1 &key (test #'eql))
  "Return two values: The first and second values are T and :ok if none of the
cons cells are shared between the cons _tree_ T0 and T1. If there are shared
cons cells or a difference in the stree structure is found, the two values are
then NIL and either :shared or :difference to indicate, respectively a shared
cons cell or that the tree structures were different. There can be no shared
structure or cycles in the cons cells."

  (labels ((helper (tree0 tree1)
             (cond
               (;; handle non-cons leaves
                (and (not (consp tree0))
                     (not (consp tree1)))
                (if (funcall test tree0 tree1)
                    (values t :ok)
                    (return-from helper
                      (values nil :difference))))
               ;; handle when cons cells are shared and recurse of not.
               ((and (consp tree0)
                     (consp tree1))
                (if (eq tree0 tree1)
                    (return-from helper
                      (values nil :shared))
                    (progn
                      (helper (car tree0) (car tree1))
                      (helper (cdr tree0) (cdr tree1)))))
               ;; some other problem, must be different.
               (t
                (return-from helper
                  (values nil :difference))))))
    (helper t0 t1)))


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
                            `(:move-original (,v0-type 1)
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
                            `(:move-original (,v0-type 2) ;; shared structure
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
      ;; But we choose that the cdr is pointing into the cloned list structure.
      (assert (eq (cdr c) c))

      (eql-map-dump-stats eql-map)
      (validate-eql-map-stats
       (eql-map-stats-match-p eql-map
                              ;; The car of the clone is a move of the original
                              ;; cons cell.
                              `(:move-original (,o-type 1))
                              ;; BUT the cdr of the clone is a move-clone
                              ;; to preserve the shared list structure.
                              `(:move-clone (,o-type 1))
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
                              `(:move-original (,v0-type 1)
                                               (,v1-type 1)
                                               (,v2-type 1)
                                               (,v3-type 1))
                              ;; Preserving the shared structure of the cycle
                              ;; causes this :move-clone to exist.
                              `(:move-clone (,o-type 1))
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
                              `(:move-original (,v0-type 1)
                                               (,v1-type 1)
                                               (,v2-type 1)
                                               (,v3-type 1))
                              ;; Preserving the shared structure of the cycle
                              ;; causes this :move-clone to exist.
                              `(:move-clone (,o-type 1))
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
             (v4 v4-type (id-type nil))
             (v5 v5-type (id-type 3))
             (v6 v6-type (id-type nil))
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

    ;; TODO: FIXME! There should be 1 NULL in move original for the first time
    ;; we see it, and then 2 NULL in move-clone, why isn't that the case?
    (eql-map-dump-stats eql-map)
    (validate-eql-map-stats
     (eql-map-stats-match-p eql-map
                            `(:move-original (,v0-type 1)
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
             (v2 v2-type (id-type (cons v0 v1)))
             (v3 v3-type (id-type 'a))
             (v4 v4-type (id-type 1))
             (v5 v5-type (id-type nil))
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
                            `(:move-original (,v0-type 1)
                                             (,v1-type 1)
                                             (,v3-type 1)
                                             (,v4-type 1)
                                             (,v5-type 1)
                                             (,v6-type 1)
                                             (,list-end-type 1))
                            ;; v2 is shared once, so there should be one move
                            ;; of its previously cloned data.
                            `(:move-clone (,v2-type 1))
                            ;; Only 7 because v2 is a cloned cons cell that was
                            ;; shared!
                            `(:allocation (,o-type 7))))

    (format t "Passed.~%")
    t))

(defun test-clone-shallow-alist-2 ()
  "Handle a single cons cell alist that references itself in both car and cdr."
  (u:mvlet ((*print-circle* t)
            ;; The two nils here are wiped out before cloning.
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
                              ;; The cdr of the cons cell references itself
                              ;; and we arbitrarily decide to honor that
                              ;; cycle and preserve it in the clone.
                              `(:move-clone (,o-type 1))
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
                              `(:move-original (,v0-type 1)
                                               (,v1-type 1))
                              ;; We arbitrarily decide to honor the cycle in
                              ;; the list structure.
                              `(:move-clone (,o-type 1))
                              `(:allocation (,o-type 2))))

      (format t "Passed.~%")
      t)))

(defun test-clone-shallow-alist-4 ()
  "An alist with additional non-kv cells entries in it and no cycles."
  (u:mvlet* ((v0 v0-type (id-type nil))
             (v1 v1-type (id-type 2))
             (v2 v2-type (id-type 'foo))
             (v3 v3-type (id-type 3))
             (v4 v4-type (id-type nil))
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
                            `(:move-original (,v0-type 1)
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
                              `(:move-original (,v0-type 1)
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
                              `(:move-original (,v0-type 1)
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
                              `(:move-original (,v0-type 2))
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
                              `(:move-original (,v0-type 1)
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
                              ;; One reference for the car, and one for the cdr
                              ;; of the newly cloned cons cell so it can
                              ;; reference itself.
                              `(:move-clone (,o-type 2))
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
                                `(:move-original (,v0-type 1)
                                                 (,v1-type 1)
                                                 (,v2-type 1)
                                                 (,v3-type 1)
                                                 (,v4-type 1)
                                                 (,v5-type 1))
                                ;; The obm cell was cloned and also referenced
                                ;; by two things. The second time it was
                                ;; referenced is counted as the first time
                                ;; here.
                                `(:move-clone (,o-type 1))
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
                                ;; All of the shared structure moves...
                                `(:move-clone (,l0-type 6))
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

(defun test-clone-shallow-array-simple-array-single-float-0 ()
  (u:mvlet* ((v0 v0-type (id-type 1f0))
             (num-elems 8)
             ;; Type (SIMPLE-ARRAY SINGLE-FLOAT (*))
             (o o-type (id-type (make-array num-elems
                                            :element-type 'single-float
                                            :initial-element v0)))
             ;; All intentions have the same behavior for arrays.
             (c eql-map (clone-shallow o (make-eql-map-with-stats))))

    (format t "Original | ~S~%" o)
    (format t "Cloned   | ~S~%" c)
    (finish-output)

    ;; Ensure it is newly allocated.
    (assert (not (eq c o)))

    (assert-matching-array-properties c o)

    ;; Then, ensure all index values were deep copied.
    ;; In this case, the characters will be = to each other.
    (dotimes (i (array-total-size c))
      (assert (= (row-major-aref c i)
                  (row-major-aref o i))))

    (eql-map-dump-stats eql-map)
    (validate-eql-map-stats
     (eql-map-stats-match-p eql-map
                            `(:move-original (,v0-type ,num-elems))
                            `(:allocation (,o-type 1))
                            `(:array-clone-speed-fast (,o-type 1))))

    (format t "Passed.~%")
    t))

(defun test-clone-shallow-array-simple-array-unsigned-byte-8-0 ()
  (u:mvlet* ((num-elems 8)
             ;; Type (SIMPLE-ARRAY (UNSIGNED-BYTE 8) (*))
             (o o-type (id-type (make-array num-elems
                                            :element-type '(unsigned-byte 8)
                                            :initial-element 0)))
             ;; All intentions have the same behavior for arrays.
             (c eql-map (clone-shallow o (make-eql-map-with-stats))))

    (format t "Original | ~S~%" o)
    (format t "Cloned   | ~S~%" c)
    (finish-output)

    ;; Ensure it is newly allocated.
    (assert (not (eq c o)))

    (assert-matching-array-properties c o)

    ;; Then, ensure all index values were deep copied.
    ;; In this case, the characters will be = to each other.
    (dotimes (i (array-total-size c))
      (assert (= (row-major-aref c i)
                  (row-major-aref o i))))

    (eql-map-dump-stats eql-map)
    (validate-eql-map-stats
     (eql-map-stats-match-p eql-map
                            `(:move-original ((unsigned-byte 8) ,num-elems))
                            `(:allocation (,o-type 1))
                            `(:array-clone-speed-fast (,o-type 1))))

    (format t "Passed.~%")
    t))

(defun test-clone-shallow-array-simple-array-signed-byte-8-0 ()
  (u:mvlet* ((num-elems 8)
             ;; Type (SIMPLE-ARRAY (SIGNED-BYTE 8) (*))
             (o o-type (id-type (make-array num-elems
                                            :element-type '(signed-byte 8)
                                            :initial-element 0)))
             ;; All intentions have the same behavior for arrays.
             (c eql-map (clone-shallow o (make-eql-map-with-stats))))

    (format t "Original | ~S~%" o)
    (format t "Cloned   | ~S~%" c)
    (finish-output)

    ;; Ensure it is newly allocated.
    (assert (not (eq c o)))

    (assert-matching-array-properties c o)

    ;; Then, ensure all index values were deep copied.
    ;; In this case, the characters will be = to each other.
    (dotimes (i (array-total-size c))
      (assert (= (row-major-aref c i)
                  (row-major-aref o i))))

    (eql-map-dump-stats eql-map)
    (validate-eql-map-stats
     (eql-map-stats-match-p eql-map
                            `(:move-original ((signed-byte 8) ,num-elems))
                            `(:allocation (,o-type 1))
                            `(:array-clone-speed-fast (,o-type 1))))

    (format t "Passed.~%")
    t))

(defun test-clone-shallow-array-simple-array-unsigned-byte-16-0 ()
  (u:mvlet* ((num-elems 8)
             ;; Type (SIMPLE-ARRAY (UNSIGNED-BYTE 16) (*))
             (o o-type (id-type (make-array num-elems
                                            :element-type '(unsigned-byte 16)
                                            :initial-element 0)))
             ;; All intentions have the same behavior for arrays.
             (c eql-map (clone-shallow o (make-eql-map-with-stats))))

    (format t "Original | ~S~%" o)
    (format t "Cloned   | ~S~%" c)
    (finish-output)

    ;; Ensure it is newly allocated.
    (assert (not (eq c o)))

    (assert-matching-array-properties c o)

    ;; Then, ensure all index values were deep copied.
    ;; In this case, the characters will be = to each other.
    (dotimes (i (array-total-size c))
      (assert (= (row-major-aref c i)
                  (row-major-aref o i))))

    (eql-map-dump-stats eql-map)
    (validate-eql-map-stats
     (eql-map-stats-match-p eql-map
                            `(:move-original ((unsigned-byte 16) ,num-elems))
                            `(:allocation (,o-type 1))
                            `(:array-clone-speed-fast (,o-type 1))))

    (format t "Passed.~%")
    t))

(defun test-clone-shallow-array-simple-array-signed-byte-16-0 ()
  (u:mvlet* ((num-elems 8)
             ;; Type (SIMPLE-ARRAY (SIGNED-BYTE 16) (*))
             (o o-type (id-type (make-array num-elems
                                            :element-type '(signed-byte 16)
                                            :initial-element 0)))
             ;; All intentions have the same behavior for arrays.
             (c eql-map (clone-shallow o (make-eql-map-with-stats))))

    (format t "Original | ~S~%" o)
    (format t "Cloned   | ~S~%" c)
    (finish-output)

    ;; Ensure it is newly allocated.
    (assert (not (eq c o)))

    (assert-matching-array-properties c o)

    ;; Then, ensure all index values were deep copied.
    ;; In this case, the characters will be = to each other.
    (dotimes (i (array-total-size c))
      (assert (= (row-major-aref c i)
                  (row-major-aref o i))))

    (eql-map-dump-stats eql-map)
    (validate-eql-map-stats
     (eql-map-stats-match-p eql-map
                            `(:move-original ((signed-byte 16) ,num-elems))
                            `(:allocation (,o-type 1))
                            `(:array-clone-speed-fast (,o-type 1))))

    (format t "Passed.~%")
    t))

(defun test-clone-shallow-array-simple-array-unsigned-byte-32-0 ()
  (u:mvlet* ((num-elems 8)
             ;; Type (SIMPLE-ARRAY (UNSIGNED-BYTE 32) (*))
             (o o-type (id-type (make-array num-elems
                                            :element-type '(unsigned-byte 32)
                                            :initial-element 0)))
             ;; All intentions have the same behavior for arrays.
             (c eql-map (clone-shallow o (make-eql-map-with-stats))))

    (format t "Original | ~S~%" o)
    (format t "Cloned   | ~S~%" c)
    (finish-output)

    ;; Ensure it is newly allocated.
    (assert (not (eq c o)))

    (assert-matching-array-properties c o)

    ;; Then, ensure all index values were deep copied.
    ;; In this case, the characters will be = to each other.
    (dotimes (i (array-total-size c))
      (assert (= (row-major-aref c i)
                  (row-major-aref o i))))

    (eql-map-dump-stats eql-map)
    (validate-eql-map-stats
     (eql-map-stats-match-p eql-map
                            `(:move-original ((unsigned-byte 32) ,num-elems))
                            `(:allocation (,o-type 1))
                            `(:array-clone-speed-fast (,o-type 1))))

    (format t "Passed.~%")
    t))

(defun test-clone-shallow-array-simple-array-signed-byte-32-0 ()
  (u:mvlet* ((num-elems 8)
             ;; Type (SIMPLE-ARRAY (SIGNED-BYTE 32) (*))
             (o o-type (id-type (make-array num-elems
                                            :element-type '(signed-byte 32)
                                            :initial-element 0)))
             ;; All intentions have the same behavior for arrays.
             (c eql-map (clone-shallow o (make-eql-map-with-stats))))

    (format t "Original | ~S~%" o)
    (format t "Cloned   | ~S~%" c)
    (finish-output)

    ;; Ensure it is newly allocated.
    (assert (not (eq c o)))

    (assert-matching-array-properties c o)

    ;; Then, ensure all index values were deep copied.
    ;; In this case, the characters will be = to each other.
    (dotimes (i (array-total-size c))
      (assert (= (row-major-aref c i)
                  (row-major-aref o i))))

    (eql-map-dump-stats eql-map)
    (validate-eql-map-stats
     (eql-map-stats-match-p eql-map
                            `(:move-original ((signed-byte 32) ,num-elems))
                            `(:allocation (,o-type 1))
                            `(:array-clone-speed-fast (,o-type 1))))

    (format t "Passed.~%")
    t))

(defun test-clone-shallow-array-simple-array-unsigned-byte-64-0 ()
  (u:mvlet* ((num-elems 8)
             ;; Type (SIMPLE-ARRAY (UNSIGNED-BYTE 64) (*))
             (o o-type (id-type (make-array num-elems
                                            :element-type '(unsigned-byte 64)
                                            :initial-element 0)))
             ;; All intentions have the same behavior for arrays.
             (c eql-map (clone-shallow o (make-eql-map-with-stats))))

    (format t "Original | ~S~%" o)
    (format t "Cloned   | ~S~%" c)
    (finish-output)

    ;; Ensure it is newly allocated.
    (assert (not (eq c o)))

    (assert-matching-array-properties c o)

    ;; Then, ensure all index values were deep copied.
    ;; In this case, the characters will be = to each other.
    (dotimes (i (array-total-size c))
      (assert (= (row-major-aref c i)
                  (row-major-aref o i))))

    (eql-map-dump-stats eql-map)
    (validate-eql-map-stats
     (eql-map-stats-match-p eql-map
                            `(:move-original ((unsigned-byte 64) ,num-elems))
                            `(:allocation (,o-type 1))
                            `(:array-clone-speed-fast (,o-type 1))))

    (format t "Passed.~%")
    t))

(defun test-clone-shallow-array-simple-array-signed-byte-64-0 ()
  (u:mvlet* ((num-elems 8)
             ;; Type (SIMPLE-ARRAY (SIGNED-BYTE 64) (*))
             (o o-type (id-type (make-array num-elems
                                            :element-type '(signed-byte 64)
                                            :initial-element 0)))
             ;; All intentions have the same behavior for arrays.
             (c eql-map (clone-shallow o (make-eql-map-with-stats))))

    (format t "Original | ~S~%" o)
    (format t "Cloned   | ~S~%" c)
    (finish-output)

    ;; Ensure it is newly allocated.
    (assert (not (eq c o)))

    (assert-matching-array-properties c o)

    ;; Then, ensure all index values were deep copied.
    ;; In this case, the characters will be = to each other.
    (dotimes (i (array-total-size c))
      (assert (= (row-major-aref c i)
                  (row-major-aref o i))))

    (eql-map-dump-stats eql-map)
    (validate-eql-map-stats
     (eql-map-stats-match-p eql-map
                            `(:move-original ((signed-byte 64) ,num-elems))
                            `(:allocation (,o-type 1))
                            `(:array-clone-speed-fast (,o-type 1))))

    (format t "Passed.~%")
    t))

(defun test-clone-shallow-array-simple-array-double-float-0 ()
  (u:mvlet* ((v0 v0-type (id-type 1d0))
             (num-elems 8)
             ;; Type (SIMPLE-ARRAY DOUBLE-FLOAT (*))
             (o o-type (id-type (make-array num-elems
                                            :element-type 'double-float
                                            :initial-element v0)))
             ;; All intentions have the same behavior for arrays.
             (c eql-map (clone-shallow o (make-eql-map-with-stats))))

    (format t "Original | ~S~%" o)
    (format t "Cloned   | ~S~%" c)
    (finish-output)

    ;; Ensure it is newly allocated.
    (assert (not (eq c o)))

    (assert-matching-array-properties c o)

    ;; Then, ensure all index values were deep copied.
    ;; In this case, the characters will be = to each other.
    (dotimes (i (array-total-size c))
      (assert (= (row-major-aref c i)
                  (row-major-aref o i))))

    (eql-map-dump-stats eql-map)
    (validate-eql-map-stats
     (eql-map-stats-match-p eql-map
                            `(:move-original (,v0-type ,num-elems))
                            `(:allocation (,o-type 1))
                            `(:array-clone-speed-fast (,o-type 1))))

    (format t "Passed.~%")
    t))

(defun test-clone-shallow-array-simple-string-0 ()
  (u:mvlet* ((v0 v0-type (id-type #\a))
             (num-elems 8)
             ;; Type SIMPLE-STRING aka (SIMPLE-ARRAY CHARACTER *)
             (o o-type (id-type
                        (make-sequence 'simple-string num-elems
                                       :initial-element v0)))
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
                            `(:move-original (,v0-type ,num-elems))
                            `(:allocation (,o-type 1))
                            `(:array-clone-speed-slow (,o-type 1))))

    (format t "Passed.~%")
    t))

(defun test-clone-shallow-array-simple-bit-vector-0 ()
  (u:mvlet* ((v0 v0-type (id-type 0))
             (num-elems 8)
             ;; Type SIMPLE-BIT-VECTOR
             (o o-type (id-type (make-sequence '(vector bit) num-elems
                                               :initial-element v0)))
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
                            `(:move-original (,v0-type ,num-elems))
                            `(:allocation (,o-type 1))
                            `(:array-clone-speed-fast (,o-type 1))))

    (format t "Passed.~%")
    t))

(defun test-clone-shallow-array-bit-vector-0 ()
  (u:mvlet* ((v0 v0-type (id-type 0))
             (num-elems 8)
             ;; Type BIT-VECTOR
             (o o-type (id-type (make-array num-elems :element-type 'bit
                                                      :adjustable t
                                                      :initial-element v0)))
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
                            `(:move-original (,v0-type ,num-elems))
                            `(:allocation (,o-type 1))
                            `(:array-clone-speed-fast (,o-type 1))))

    (format t "Passed.~%")
    t))

(defun test-clone-shallow-array-unique-simple-array-0 ()
  (u:mvlet* (;; Type SIMPLE-ARRAY
             (num-elems 8)
             (o o-type (id-type (make-array num-elems
                                            :element-type '(unsigned-byte 8)
                                            :initial-element 0)))
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
                            `(:move-original ((unsigned-byte 8) ,num-elems))
                            `(:allocation (,o-type 1))
                            `(:array-clone-speed-fast (,o-type 1))))

    (format t "Passed.~%")
    t))

(defun test-clone-shallow-array-shared-simple-array-0 ()
  (u:mvlet* ((v0 v0-type (id-type (cons 1 2)))
             (num-elems 4)
             ;; Type SIMPLE-ARRAY
             (o o-type (id-type (make-array num-elems :element-type 'cons
                                                      :initial-element v0)))
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

    ;; Verify the shared structure in copy.
    (dotimes (i (array-total-size c))
      (assert (eql (row-major-aref c i)
                   (row-major-aref o (mod i (array-total-size c))))))

    (eql-map-dump-stats eql-map)
    (validate-eql-map-stats
     (eql-map-stats-match-p eql-map
                            ;; Since we never copied any of the elements in the
                            ;; shallow copy, we don't know they are actually
                            ;; shared references, so we can only blindy record
                            ;; them in the :move-original domain.
                            `(:move-original (,v0-type ,num-elems))
                            `(:allocation (,o-type 1))
                            `(:array-clone-speed-slow (,o-type 1))))

    (format t "Passed.~%")
    t))

(defun test-clone-shallow-array-unique-simple-vector-0 ()
  (u:mvlet* ((v0 v0-type (id-type 0))
             (num-elems 8)
             ;; Type SIMPLE-VECTOR
             (o o-type (id-type (make-array num-elems :initial-element v0)))
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
                            `(:move-original (,v0-type ,num-elems))
                            `(:allocation (,o-type 1))
                            `(:array-clone-speed-slow (,o-type 1))))

    (format t "Passed.~%")
    t))

(defun test-clone-shallow-array-shared-simple-vector-0 ()
  (u:mvlet* ((v0 v0-type (id-type (cons 1 2)))
             (num-elems 4)
             ;; Type SIMPLE-VECTOR
             (o o-type (id-type (make-array num-elems :initial-element v0)))
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

    ;; Verify the shared structure in copy.
    (dotimes (i (array-total-size c))
      (assert (eql (row-major-aref c i)
                   (row-major-aref o (mod i (array-total-size c))))))

    (eql-map-dump-stats eql-map)
    (validate-eql-map-stats
     (eql-map-stats-match-p eql-map
                            ;; We are blind to the fact there is shared
                            ;; structure in this shallow clone, so we record
                            ;; these into the :move-original domain.
                            `(:move-original (,v0-type ,num-elems))
                            `(:allocation (,o-type 1))
                            `(:array-clone-speed-slow (,o-type 1))))

    (format t "Passed.~%")
    t))

(defun test-clone-shallow-array-unique-vector-0 ()
  (u:mvlet* ((v0 v0-type (id-type 0))
             (num-elems 8)
             ;; Type VECTOR
             (o o-type (id-type (make-array num-elems :adjustable t
                                                      :initial-element v0)))
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
                            `(:move-original (,v0-type ,num-elems))
                            `(:allocation (,o-type 1))
                            `(:array-clone-speed-slow (,o-type 1))))

    (format t "Passed.~%")
    t))

(defun test-clone-shallow-array-shared-vector-0 ()
  (u:mvlet* ((v0 v0-type (id-type (cons 1 2)))
             (num-elems 4)
             ;; Type VECTOR
             (o o-type (id-type (make-array num-elems :adjustable t
                                                      :initial-element v0)))
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

    ;; Verify the shared structure in copy.
    (dotimes (i (array-total-size c))
      (assert (eql (row-major-aref c i)
                   (row-major-aref o (mod i (array-total-size c))))))

    (eql-map-dump-stats eql-map)
    (validate-eql-map-stats
     (eql-map-stats-match-p eql-map
                            ;; We are blind to the shared structure in the
                            ;; shallow copy and can only record this into the
                            ;; :move-original domain.
                            `(:move-original (,v0-type ,num-elems))
                            `(:allocation (,o-type 1))
                            `(:array-clone-speed-slow (,o-type 1))))

    (format t "Passed.~%")
    t))

(defun test-clone-shallow-array-unique-array-0 ()
  (u:mvlet* ((v0 v0-type (id-type 0))
             (dim '(3 4))
             ;; Type ARRAY
             (o o-type (id-type (make-array dim :adjustable t
                                                :initial-element v0)))
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
                            `(:move-original (,v0-type ,(array-total-size c)))
                            `(:allocation (,o-type 1))
                            `(:array-clone-speed-slow (,o-type 1))))

    (format t "Passed.~%")
    t))

(defun test-clone-shallow-array-shared-array-0 ()
  (u:mvlet* ((v0 v0-type (id-type (cons 1 2)))
             (dim '(3 4))
             ;; Type ARRAY
             (o o-type (id-type (make-array dim :adjustable t
                                                :initial-element v0)))
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

    ;; Verify the shared structure in copy.
    (dotimes (i (array-total-size c))
      (assert (eql (row-major-aref c i)
                   (row-major-aref o (mod i (array-total-size c))))))

    (eql-map-dump-stats eql-map)
    (validate-eql-map-stats
     (eql-map-stats-match-p eql-map
                            ;; We are blind to the shared structure in the
                            ;; shallow clone and can only record these into the
                            ;; :move-original domain..
                            `(:move-original (,v0-type ,(array-total-size c)))
                            `(:allocation (,o-type 1))
                            `(:array-clone-speed-slow (,o-type 1))))

    (format t "Passed.~%")
    t))

(defun test-clone-shallow-array-simple-base-string-0 ()
  (u:mvlet* ((v0 v0-type (id-type #\a))
             (num-elems 8)
             ;; Type SIMPLE-BASE-STRING
             (o o-type (id-type (make-array num-elems :element-type 'base-char
                                                      :initial-element v0)))
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
                            `(:move-original (,v0-type ,num-elems))
                            `(:allocation (,o-type 1))
                            `(:array-clone-speed-slow (,o-type 1))))

    (format t "Passed.~%")
    t))

(defun test-clone-shallow-array-base-string-0 ()
  (u:mvlet* ((v0 v0-type (id-type #\a))
             (num-elems 8)
             ;; Type BASE-STRING
             (o o-type (id-type (make-array num-elems :element-type 'base-char
                                                      :adjustable t
                                                      :initial-element v0)))
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
                            `(:move-original (,v0-type ,num-elems))
                            `(:allocation (,o-type 1))
                            `(:array-clone-speed-slow (,o-type 1))))

    (format t "Passed.~%")
    t))

(defun test-clone-shallow-array-unique-any-0 ()
  (u:mvlet* ((v0 v0-type (id-type nil))
             (v1 v1-type (id-type (cons nil nil)))
             (v2 v2-type (id-type 1))
             (v3 v3-type (id-type "hello"))
             (v4 v4-type (id-type 'a))
             (v5 v5-type (id-type 23.5))
             (v6 v6-type (id-type #\a))
             (v7 v7-type (id-type (u:dict #'equal)))
             (v8 v8-type (id-type #P"/tmp/foo.txt"))
             (contents (list v0 v1 v2 v3 v4 v5 v6 v7 v8))
             (o o-type (id-type (make-array (length contents)
                                            :initial-contents contents)))
             ;; All intentions have the same behavior for arrays.
             (c eql-map (clone-shallow o (make-eql-map-with-stats))))

    (format t "Original | ~S~%" o)
    (format t "Cloned   | ~S~%" c)
    (finish-output)

    ;; Ensure it is newly allocated.
    (assert (not (eq c o)))

    (assert-matching-array-properties c o)

    ;; Then, ensure all index values were shallow copied.
    (dotimes (i (length c))
      (assert (eql (aref c i) (aref o i))))

    (eql-map-dump-stats eql-map)
    (validate-eql-map-stats
     (eql-map-stats-match-p eql-map
                            `(:move-original (,v0-type 1)
                                             (,v1-type 1)
                                             (,v2-type 1)
                                             (,v3-type 1)
                                             (,v4-type 1)
                                             (,v5-type 1)
                                             (,v6-type 1)
                                             (,v7-type 1)
                                             (,v8-type 1))
                            `(:allocation (,o-type 1))
                            `(:array-clone-speed-slow (,o-type 1))))

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

;; another helper to compare shallow cloned hash tables contents.
(defun ht-shallow-kv-valid-p (original-ht cloned-ht all-keys all-values)
  "Compare the ORIGINAL-HT with its shallow cloned CLONED-HT copy and ensure
that ALL-KEYS are present and ALL-VALUES are present and that each key maps
properly to each value. Also check that they keys and values were shallow
copied. Return T if this is the case and NIL otherwise."
  ;; All-keys and all-values must be present in the original.
  (u:do-hash (k v original-ht)
    (unless (some (u:curry 'eql k) all-keys)
      (return-from ht-shallow-kv-valid-p nil))
    (unless (some (u:curry 'eql v) all-values)
      (return-from ht-shallow-kv-valid-p nil)))

  ;; All keys and all values must be present in the clone.
  (u:do-hash (k v cloned-ht)
    (unless (some (u:curry 'eql k) all-keys)
      (return-from ht-shallow-kv-valid-p nil))
    (unless (some (u:curry 'eql v) all-values)
      (return-from ht-shallow-kv-valid-p nil)))

  ;; Cross validate that each key in the original maps to the same value for
  ;; that key in the clone and vice versa. This ensures that both hash tables
  ;; have the same keys and the same values and each keys map to the same
  ;; values in each table.
  (u:do-hash (k v original-ht)
    (let ((cv (u:href cloned-ht k)))
      (unless (eql cv v)
        (return-from ht-shallow-kv-valid-p nil))))
  (u:do-hash (k v cloned-ht)
    (let ((ov (u:href original-ht k)))
      (unless (eql ov v)
        (return-from ht-shallow-kv-valid-p nil))))
  t)

(defun test-clone-shallow-hash-table-0 ()
  (u:mvlet* ((o o-type (id-type (u:dict)))
             (c eql-map (clone-shallow o (make-eql-map-with-stats))))

    (format t "Original | ~S~%" o)
    (format t "Cloned   | ~S~%" c)
    (finish-output)

    (assert (not (eq c o)))

    (assert-matching-hash-table-properties c o)

    ;; Ensure the key/values pairs are correct in the clone.
    ;; NOTE: There should be none. :)
    (assert (ht-shallow-kv-valid-p o c nil nil))

    (eql-map-dump-stats eql-map)
    (validate-eql-map-stats
     (eql-map-stats-match-p eql-map
                            `(:allocation (,o-type 1))))

    (format t "Passed.~%")
    t))

(defun test-clone-shallow-hash-table-1 ()
  (u:mvlet* ((k0 k0-type (id-type 'foo))
             (v0 v0-type (id-type 'bar))
             (o o-type (id-type (u:dict #'eq k0 v0)))
             (c eql-map (clone-shallow o (make-eql-map-with-stats))))

    (format t "Original | ~S~%" o)
    (format t "Cloned   | ~S~%" c)
    (finish-output)

    ;; Ensure it is newly allocated
    (assert (not (eq c o)))

    (assert-matching-hash-table-properties c o)

    ;; Ensure the key/values pairs are correct in the clone.
    (assert (ht-shallow-kv-valid-p o c (list k0) (list v0)))

    (eql-map-dump-stats eql-map)
    (validate-eql-map-stats
     (eql-map-stats-match-p eql-map
                            `(:move-original (,k0-type 1)
                                             (,v0-type 1))
                            `(:allocation (,o-type 1))))

    (format t "Passed.~%")
    t))

(defun test-clone-shallow-hash-table-2 ()
  (u:mvlet* ((k0 k0-type (id-type (list 1 2 3)))
             (v0 v0-type (id-type (list 4 5 6)))
             (o o-type (id-type (u:dict #'equal k0 v0)))
             (c eql-map (clone-shallow o (make-eql-map-with-stats))))

    (format t "Original | ~S~%" o)
    (format t "Cloned   | ~S~%" c)
    (finish-output)

    (assert (not (eq c o)))

    (assert-matching-hash-table-properties c o)

    (assert (u:href c k0))
    (assert (equal (u:href c k0) v0))

    ;; Ensure the key/values pairs are correct in the clone.
    (assert (ht-shallow-kv-valid-p o c (list k0) (list v0)))

    (eql-map-dump-stats eql-map)
    (validate-eql-map-stats
     (eql-map-stats-match-p eql-map
                            `(:move-original (,k0-type 1)
                                             (,v0-type 1))
                            `(:allocation (,o-type 1))))
    (format t "Passed.~%")
    t))

(defun test-clone-shallow-hash-table-3 ()
  (u:mvlet* ((k0 k0-type (id-type 'foo))
             (v0 v0-type (id-type 23))
             (k1 k1-type (id-type #\a))
             (v1 v1-type (id-type 'bar))
             (k2 k2-type (id-type #P"/tmp/thing.txt"))
             (v2 v2-type (id-type 12.5))
             (k3 k3-type (id-type "foobar"))
             (v3 v3-type (id-type "stuff"))
             (all-keys (list k0 k1 k2 k3))
             (all-values (list v0 v1 v2 v3))
             (o o-type (id-type (u:dict #'eql k0 v0 k1 v1 k2 v2 k3 v3)))
             (c eql-map (clone-shallow o (make-eql-map-with-stats))))

    (format t "Original | ~S~%" o)
    (format t "Cloned   | ~S~%" c)
    (finish-output)

    ;; Ensure it is newly allocated
    (assert (not (eq c o)))

    (assert-matching-hash-table-properties c o)

    ;; Ensure the key/values pairs are correct in the clone.
    (assert (ht-shallow-kv-valid-p o c all-keys all-values))

    (eql-map-dump-stats eql-map)
    (validate-eql-map-stats
     (eql-map-stats-match-p eql-map
                            `(:move-original (,k0-type 1)
                                             (,k1-type 1)
                                             (,k2-type 1)
                                             (,k3-type 1)
                                             (,v0-type 1)
                                             (,v1-type 1)
                                             (,v2-type 1)
                                             (,v3-type 1))
                            `(:allocation (,o-type 1))))

    (format t "Passed.~%")
    t))



;;; ------------------------------------------------------------
;; Aggregate shallow cloning testing code.
;;; ------------------------------------------------------------

(defun test-clone-shallow ()
  (format t "Shallow clone tests.~%")

  (test-eql-map)

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

  (test-clone-shallow-array-simple-array-single-float-0)
  (test-clone-shallow-array-simple-array-unsigned-byte-8-0)
  (test-clone-shallow-array-simple-array-signed-byte-8-0)
  (test-clone-shallow-array-simple-array-unsigned-byte-16-0)
  (test-clone-shallow-array-simple-array-signed-byte-16-0)
  (test-clone-shallow-array-simple-array-unsigned-byte-32-0)
  (test-clone-shallow-array-simple-array-signed-byte-32-0)
  (test-clone-shallow-array-simple-array-unsigned-byte-64-0)
  (test-clone-shallow-array-simple-array-double-float-0)
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
  (test-clone-shallow-hash-table-3)

  (format t "Shallow clone tests passed!~%")

  t)

;;; ---------------------------------------------------------------------------
;; DEEP cloning tests.
;;; ---------------------------------------------------------------------------

;;; ------------------------------------------------------------
;; clone-deep functions of ANY intention
;;; ------------------------------------------------------------

(defun test-clone-deep-function-0 ()
  (u:mvlet* (;; Type FUNCTION
             (o o-type (id-type #'cl:identity))
             (c eql-map (clone-deep o (make-eql-map-with-stats))))

    (format t "Original | ~S~%" o)
    (format t "Cloned   | ~S~%" c)
    (finish-output)

    ;; Should return itself.
    (assert (eq o c))

    (eql-map-dump-stats eql-map)
    (validate-eql-map-stats
     (eql-map-stats-match-p eql-map
                            `(:move-original (,o-type 1))))

    (format t "Passed.~%")
    t))

;;; ------------------------------------------------------------
;; clone-deep character of ANY intention
;;; ------------------------------------------------------------

(defun test-clone-deep-character-0 ()
  (u:mvlet* (;; Type CHARACTER
             (o o-type (id-type #\a))
             (c eql-map (clone-deep o (make-eql-map-with-stats))))

    (format t "Original | ~S~%" o)
    (format t "Cloned   | ~S~%" c)
    (finish-output)

    ;; Should return itself.
    (assert (eql o c))

    (eql-map-dump-stats eql-map)
    (validate-eql-map-stats
     (eql-map-stats-match-p eql-map
                            `(:move-original (,o-type 1))))

    (format t "Passed.~%")
    t))

;;; ------------------------------------------------------------
;; clone-deep pathname of ANY intention
;;; ------------------------------------------------------------

(defun test-clone-deep-pathname-0 ()
  (u:mvlet* (;; Type PATHNAME
             (o o-type (id-type #P"/tmp/foo.txt"))
             (c eql-map (clone-deep o (make-eql-map-with-stats))))

    (format t "Original | ~S~%" o)
    (format t "Cloned   | ~S~%" c)
    (finish-output)

    ;; Should return itself.
    (assert (equal o c))

    (eql-map-dump-stats eql-map)
    (validate-eql-map-stats
     (eql-map-stats-match-p eql-map
                            `(:move-original (,o-type 1))))

    (format t "Passed.~%")
    t))

;;; ------------------------------------------------------------
;; clone-deep symbol of ANY intention
;;; ------------------------------------------------------------

(defun test-clone-deep-symbol-0 ()
  (u:mvlet* (;; Type SYMBOL
             (o o-type (id-type 'a))
             (c eql-map (clone-deep o (make-eql-map-with-stats))))

    (format t "Original | ~S~%" o)
    (format t "Cloned   | ~S~%" c)
    (finish-output)

    ;; Should return itself.
    (assert (eq o c))

    (eql-map-dump-stats eql-map)
    (validate-eql-map-stats
     (eql-map-stats-match-p eql-map
                            `(:move-original (,o-type 1))))

    (format t "Passed.~%")
    t))

;;; ------------------------------------------------------------
;; clone-deep integer of ANY intention
;;; ------------------------------------------------------------

(defun test-clone-deep-integer-0 ()
  (u:mvlet* (;; Type INTEGER
             (o o-type (id-type 42))
             (c eql-map (clone-deep o (make-eql-map-with-stats))))

    (format t "Original | ~S~%" o)
    (format t "Cloned   | ~S~%" c)
    (finish-output)

    ;; Should return itself.
    (assert (eql o c))

    (eql-map-dump-stats eql-map)
    (validate-eql-map-stats
     (eql-map-stats-match-p eql-map
                            `(:move-original (,o-type 1))))

    (format t "Passed.~%")
    t))

;;; ------------------------------------------------------------
;; clone-deep cons.
;;
;; NOTE: The graphs only use cons cells and identity-clone values.
;;; ------------------------------------------------------------

(defun test-clone-deep-cons-0 ()
  (u:mvlet* ((v0 v0-type (id-type nil))
             (v1 v1-type (id-type nil))
             (o o-type (id-type (cons v0 v1)))
             (c eql-map (clone-deep o (make-eql-map-with-stats))))

    (format t "Original | ~S~%" o)
    (format t "Cloned   | ~S~%" c)
    (finish-output)

    ;; This part must be cloned.
    (assert (not (eq o c)))

    ;; but the values of the car and cdr must still be eq/eql to original
    ;; since they are identity clone objects.
    (assert (eql (car c) (car o)))
    (assert (eql (cdr c) (cdr o)))

    (eql-map-dump-stats eql-map)
    (validate-eql-map-stats
     (eql-map-stats-match-p eql-map
                            `(:move-original (,v0-type 1)
                                             (,v1-type 1))
                            `(:allocation (,o-type 1))))
    (format t "Passed.~%")
    t))

(defun test-clone-deep-cons-1 ()
  (u:mvlet* ((v0 v0-type (id-type 1))
             (v1 v1-type (id-type 2))
             (o o-type (id-type (cons v0 v1)))
             (c eql-map (clone-deep o (make-eql-map-with-stats))))

    (format t "Original | ~S~%" o)
    (format t "Cloned   | ~S~%" c)
    (finish-output)

    ;; This part must be cloned.
    (assert (not (eq o c)))

    ;; but the values of the car and cdr must still be eq/eql to original
    ;; since they are identity clone objects.
    (assert (eql (car c) (car o)))
    (assert (eql (cdr c) (cdr o)))

    (eql-map-dump-stats eql-map)
    (validate-eql-map-stats
     (eql-map-stats-match-p eql-map
                            `(:move-original (,v0-type 1)
                                             (,v1-type 1))
                            `(:allocation (,o-type 1))))
    (format t "Passed.~%")
    t))

(defun test-clone-deep-cons-2 ()
  (u:mvlet* ((*print-circle* t)
             (graph (cons-graph
                     '((:roots o)
                       (o :r o)
                       (o :l o))))
             (o (get-roots graph 'o))
             (o-type (type-of o))
             (c eql-map (clone-deep o (make-eql-map-with-stats))))

    (format t "Original | ~S~%" o)
    (format t "Cloned   | ~S~%" c)
    (finish-output)

    ;; Ensure new memory
    (assert (not (eq o c)))

    ;; Ensure structure is deep cloned.
    (assert (eq (car c) c))
    (assert (eq (cdr c) c))

    (eql-map-dump-stats eql-map)
    (validate-eql-map-stats
     (eql-map-stats-match-p eql-map
                            ;; o's move of the cloned copy reference to itself!
                            `(:move-clone (,o-type 2))
                            `(:allocation (,o-type 1))))

    (format t "Passed.~%")
    t))

(defun test-clone-deep-cons-3 ()
  (u:mvlet* ((*print-circle* t)
             (v0 v0-type (id-type 1))
             (v1 v1-type (id-type #\a))
             (v2 v2-type (id-type #'+))
             (v3 v3-type (id-type '+))
             (list-end list-end-type (id-type nil))
             (graph (cons-graph
                     `((:roots n0 n1 n2 n3)
                       (n0 :r n1 :r n2 :r n3 :r (:v ,list-end))
                       (n0 :l (:v ,v0))
                       (n1 :l (:v ,v1))
                       (n2 :l (:v ,v2))
                       (n3 :l (:v ,v3)))))
             (n0 n1 n2 n3
                 (get-roots graph 'n0 'n1 'n2 'n3))
             (n0-type n1-type n2-type n3-type
                      (map-type-of n0 n1 n2 n3))
             (c eql-map (clone-deep n0 (make-eql-map-with-stats))))

    (format t "Original | ~S~%" n0)
    (format t "Cloned   | ~S~%" c)
    (finish-output)

    ;; Ensure new memory
    (assert (not (eq n0 c)))

    ;; Ensure that each cons cell was cloned.
    (loop :for o-cell :on n0
          :for c-cell :on c
          :do (assert (not (eq o-cell c-cell))))

    (eql-map-dump-stats eql-map)
    (validate-eql-map-stats
     (eql-map-stats-match-p eql-map
                            `(:move-original (,v0-type 1)
                                             (,v1-type 1)
                                             (,v2-type 1)
                                             (,v3-type 1)
                                             (,list-end-type 1))
                            `(:allocation (,n0-type 1)
                                          (,n1-type 1)
                                          (,n2-type 1)
                                          (,n3-type 1))))
    (format t "Passed.~%")
    t))

(defun test-clone-deep-cons-4 ()
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
                 (get-roots graph 'n0 'n1 'n2 'n3))
             (n0-type n1-type n2-type n3-type
                      (map-type-of n0 n1 n2 n3))
             (c eql-map (clone-deep n0 (make-eql-map-with-stats))))

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

      (eql-map-dump-stats eql-map)
      (validate-eql-map-stats
       (eql-map-stats-match-p eql-map
                              `(:move-clone (,n0-type 2) ;; n0 used twice.
                                            (,n1-type 1)
                                            (,n2-type 1)
                                            (,n3-type 1))
                              `(:allocation (,n0-type 1)
                                            (,n1-type 1)
                                            (,n2-type 1)
                                            (,n3-type 1))))

      (format t "Passed.~%")
      t)))

(defun test-clone-deep-cons-5 ()
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
                 (get-roots graph 'n0 'n1 'n2 'n3))
             (n0-type n1-type n2-type n3-type
                      (map-type-of n0 n1 n2 n3))
             (c eql-map (clone-deep n0 (make-eql-map-with-stats))))

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

      (eql-map-dump-stats eql-map)
      (validate-eql-map-stats
       (eql-map-stats-match-p eql-map
                              `(:move-clone (,n0-type 2) ;; n0 used twice.
                                            (,n1-type 1)
                                            (,n2-type 1)
                                            (,n3-type 1))
                              `(:allocation (,n0-type 1)
                                            (,n1-type 1)
                                            (,n2-type 1)
                                            (,n3-type 1))))

      (format t "Passed.~%")
      t)))

(defun test-clone-deep-cons-6 ()
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
                 (get-roots graph 'n0 'n1 'n2 'n3))
             (n0-type n1-type n2-type n3-type
                      (map-type-of n0 n1 n2 n3))
             (c eql-map (clone-deep n0 (make-eql-map-with-stats))))

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

      (eql-map-dump-stats eql-map)
      (validate-eql-map-stats
       (eql-map-stats-match-p eql-map
                              `(:move-clone (,n0-type 2) ;; used twice
                                            (,n1-type 1)
                                            (,n2-type 1)
                                            (,n3-type 1))
                              `(:allocation (,n0-type 1)
                                            (,n1-type 1)
                                            (,n2-type 1)
                                            (,n3-type 1))))

      (format t "Passed.~%")
      t)))

(defun test-clone-deep-cons-7 ()
  "Irreducible loop with shared structure and cycles."
  (u:mvlet* ((*print-circle* t)
             (v0 v0-type (id-type 1))
             (graph (cons-graph
                     `((:roots n0 n1 n2)
                       (n0 :l n1)
                       (n0 :r n2)
                       (n1 :l (:v ,v0))
                       (n1 :r n2)
                       (n2 :l n1)
                       (n2 :r n1))))
             (n0 n1 n2
                 (get-roots graph 'n0 'n1 'n2))
             (n0-type n1-type n2-type
                      (map-type-of n0 n1 n2))
             (c eql-map (clone-deep n0 (make-eql-map-with-stats))))

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

      (assert (eql (car c1) v0))
      (assert (eq (cdr c1) c2))

      (assert (eq (car c2) c1))
      (assert (eq (cdr c2) c1))

      (eql-map-dump-stats eql-map)
      (validate-eql-map-stats
       (eql-map-stats-match-p eql-map
                              `(:move-original (,v0-type 1))
                              `(:move-clone (,n1-type 2) ;; shared structure.
                                            (,n2-type 1))
                              `(:allocation (,n0-type 1)
                                            (,n1-type 1)
                                            (,n2-type 1))))

      (format t "Passed.~%")
      t)))

(defun test-clone-deep-cons-8 ()
  "A list of list of lists."
  (u:mvlet* ((v0 v0-type (id-type 0))
             (v1 v1-type (id-type 'a))
             (v2 v2-type (id-type 1))
             (v3 v3-type (id-type 'b))
             (v4 v4-type (id-type 2))
             (v5 v5-type (id-type 'c))
             (v6 v6-type (id-type 3))
             (v7 v7-type (id-type 'd))
             (v8 v8-type (id-type 4))
             (v9 v9-type (id-type 'e))
             (v10 v10-type (id-type 5))
             (v11 v11-type (id-type 'f))
             (v12 v12-type (id-type 6))
             (v13 v13-type (id-type 'g))
             (v14 v14-type (id-type 7))
             (v15 v15-type (id-type 'h))
             (v16 v16-type (id-type 8))
             (v17 v17-type (id-type 'i))
             (v18 v18-type (id-type 9))
             (v19 v19-type (id-type 'j))
             (list-end-type (type-of nil))
             ;; NOTE: This next list has 35 cons cells.
             (o o-type (id-type `(((,v0 ,v1) (,v2 ,v3))
                                  ((,v4 ,v5) (,v6 ,v7))
                                  ((,v8 ,v9) (,v10 ,v11))
                                  ((,v12 ,v13) (,v14 ,v15))
                                  ((,v16 ,v17) (,v18 ,v19)))))
             (c eql-map (clone-deep o (make-eql-map-with-stats))))

    (format t "Original | ~S~%" o)
    (format t "Cloned   | ~S~%" c)
    (finish-output)

    ;; Validate that the leaves are the same acording to EQL, no cons cells
    ;; are shared, and the tree structure is identical.
    (assert (tree-unshared-cons-p o c))

    (eql-map-dump-stats eql-map)
    (validate-eql-map-stats
     (eql-map-stats-match-p eql-map
                            `(:move-original (,v0-type 1)
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
                                             (,v16-type 1)
                                             (,v17-type 1)
                                             (,v18-type 1)
                                             (,v19-type 1)
                                             ;; NIL at end of each list.
                                             (,list-end-type 16))
                            `(:allocation (,o-type 35))))

    (format t "Passed.~%")
    t))


;;; ------------------------------------------------------------
;; clone-deep arrays.
;;
;; NOTE: The graphs only use arrays, cons cells, and identity-clone values.
;;; ------------------------------------------------------------

(defun test-clone-deep-array-simple-array-single-float-0 ()
  (u:mvlet* ((v0 v0-type (id-type 1f0))
             (num-elems 8)
             ;; Type (SIMPLE-ARRAY SINGLE-FLOAT (*))
             (o o-type (id-type (make-array num-elems
                                            :element-type 'single-float
                                            :initial-element v0)))
             ;; All intentions have the same behavior for arrays.
             (c eql-map (clone-deep o (make-eql-map-with-stats))))

    (format t "Original | ~S~%" o)
    (format t "Cloned   | ~S~%" c)
    (finish-output)

    ;; Ensure it is newly allocated.
    (assert (not (eq c o)))

    (assert-matching-array-properties c o)

    ;; Then, ensure all index values were deep copied.
    ;; In this case, the characters will be = to each other.
    (dotimes (i (array-total-size c))
      (assert (= (row-major-aref c i)
                  (row-major-aref o i))))

    (eql-map-dump-stats eql-map)
    (validate-eql-map-stats
     (eql-map-stats-match-p eql-map
                            `(:move-original (,v0-type ,num-elems))
                            `(:allocation (,o-type 1))
                            `(:array-clone-speed-fast (,o-type 1))))

    (format t "Passed.~%")
    t))

(defun test-clone-deep-array-simple-array-unsigned-byte-8-0 ()
  (u:mvlet* ((num-elems 8)
             ;; Type (SIMPLE-ARRAY (UNSIGNED-BYTE 8) (*))
             (o o-type (id-type (make-array num-elems
                                            :element-type '(unsigned-byte 8)
                                            :initial-element 0)))
             ;; All intentions have the same behavior for arrays.
             (c eql-map (clone-deep o (make-eql-map-with-stats))))

    (format t "Original | ~S~%" o)
    (format t "Cloned   | ~S~%" c)
    (finish-output)

    ;; Ensure it is newly allocated.
    (assert (not (eq c o)))

    (assert-matching-array-properties c o)

    ;; Then, ensure all index values were deep copied.
    ;; In this case, the characters will be = to each other.
    (dotimes (i (array-total-size c))
      (assert (= (row-major-aref c i)
                  (row-major-aref o i))))

    (eql-map-dump-stats eql-map)
    (validate-eql-map-stats
     (eql-map-stats-match-p eql-map
                            `(:move-original ((unsigned-byte 8) ,num-elems))
                            `(:allocation (,o-type 1))
                            `(:array-clone-speed-fast (,o-type 1))))

    (format t "Passed.~%")
    t))

(defun test-clone-deep-array-simple-array-signed-byte-8-0 ()
  (u:mvlet* ((num-elems 8)
             ;; Type (SIMPLE-ARRAY (SIGNED-BYTE 8) (*))
             (o o-type (id-type (make-array num-elems
                                            :element-type '(signed-byte 8)
                                            :initial-element 0)))
             ;; All intentions have the same behavior for arrays.
             (c eql-map (clone-deep o (make-eql-map-with-stats))))

    (format t "Original | ~S~%" o)
    (format t "Cloned   | ~S~%" c)
    (finish-output)

    ;; Ensure it is newly allocated.
    (assert (not (eq c o)))

    (assert-matching-array-properties c o)

    ;; Then, ensure all index values were deep copied.
    ;; In this case, the characters will be = to each other.
    (dotimes (i (array-total-size c))
      (assert (= (row-major-aref c i)
                  (row-major-aref o i))))

    (eql-map-dump-stats eql-map)
    (validate-eql-map-stats
     (eql-map-stats-match-p eql-map
                            `(:move-original ((signed-byte 8) ,num-elems))
                            `(:allocation (,o-type 1))
                            `(:array-clone-speed-fast (,o-type 1))))

    (format t "Passed.~%")
    t))


(defun test-clone-deep-array-simple-array-unsigned-byte-16-0 ()
  (u:mvlet* ((num-elems 8)
             ;; Type (SIMPLE-ARRAY (UNSIGNED-BYTE 16) (*))
             (o o-type (id-type (make-array num-elems
                                            :element-type '(unsigned-byte 16)
                                            :initial-element 0)))
             ;; All intentions have the same behavior for arrays.
             (c eql-map (clone-deep o (make-eql-map-with-stats))))

    (format t "Original | ~S~%" o)
    (format t "Cloned   | ~S~%" c)
    (finish-output)

    ;; Ensure it is newly allocated.
    (assert (not (eq c o)))

    (assert-matching-array-properties c o)

    ;; Then, ensure all index values were deep copied.
    ;; In this case, the characters will be = to each other.
    (dotimes (i (array-total-size c))
      (assert (= (row-major-aref c i)
                  (row-major-aref o i))))

    (eql-map-dump-stats eql-map)
    (validate-eql-map-stats
     (eql-map-stats-match-p eql-map
                            `(:move-original ((unsigned-byte 16) ,num-elems))
                            `(:allocation (,o-type 1))
                            `(:array-clone-speed-fast (,o-type 1))))

    (format t "Passed.~%")
    t))

(defun test-clone-deep-array-simple-array-signed-byte-16-0 ()
  (u:mvlet* ((num-elems 8)
             ;; Type (SIMPLE-ARRAY (SIGNED-BYTE 16) (*))
             (o o-type (id-type (make-array num-elems
                                            :element-type '(signed-byte 16)
                                            :initial-element 0)))
             ;; All intentions have the same behavior for arrays.
             (c eql-map (clone-deep o (make-eql-map-with-stats))))

    (format t "Original | ~S~%" o)
    (format t "Cloned   | ~S~%" c)
    (finish-output)

    ;; Ensure it is newly allocated.
    (assert (not (eq c o)))

    (assert-matching-array-properties c o)

    ;; Then, ensure all index values were deep copied.
    ;; In this case, the characters will be = to each other.
    (dotimes (i (array-total-size c))
      (assert (= (row-major-aref c i)
                  (row-major-aref o i))))

    (eql-map-dump-stats eql-map)
    (validate-eql-map-stats
     (eql-map-stats-match-p eql-map
                            `(:move-original ((signed-byte 16) ,num-elems))
                            `(:allocation (,o-type 1))
                            `(:array-clone-speed-fast (,o-type 1))))

    (format t "Passed.~%")
    t))


(defun test-clone-deep-array-simple-array-unsigned-byte-32-0 ()
  (u:mvlet* ((num-elems 8)
             ;; Type (SIMPLE-ARRAY (UNSIGNED-BYTE 32) (*))
             (o o-type (id-type (make-array num-elems
                                            :element-type '(unsigned-byte 32)
                                            :initial-element 0)))
             ;; All intentions have the same behavior for arrays.
             (c eql-map (clone-deep o (make-eql-map-with-stats))))

    (format t "Original | ~S~%" o)
    (format t "Cloned   | ~S~%" c)
    (finish-output)

    ;; Ensure it is newly allocated.
    (assert (not (eq c o)))

    (assert-matching-array-properties c o)

    ;; Then, ensure all index values were deep copied.
    ;; In this case, the characters will be = to each other.
    (dotimes (i (array-total-size c))
      (assert (= (row-major-aref c i)
                  (row-major-aref o i))))

    (eql-map-dump-stats eql-map)
    (validate-eql-map-stats
     (eql-map-stats-match-p eql-map
                            `(:move-original ((unsigned-byte 32) ,num-elems))
                            `(:allocation (,o-type 1))
                            `(:array-clone-speed-fast (,o-type 1))))

    (format t "Passed.~%")
    t))

(defun test-clone-deep-array-simple-array-signed-byte-32-0 ()
  (u:mvlet* ((num-elems 8)
             ;; Type (SIMPLE-ARRAY (SIGNED-BYTE 32) (*))
             (o o-type (id-type (make-array num-elems
                                            :element-type '(signed-byte 32)
                                            :initial-element 0)))
             ;; All intentions have the same behavior for arrays.
             (c eql-map (clone-deep o (make-eql-map-with-stats))))

    (format t "Original | ~S~%" o)
    (format t "Cloned   | ~S~%" c)
    (finish-output)

    ;; Ensure it is newly allocated.
    (assert (not (eq c o)))

    (assert-matching-array-properties c o)

    ;; Then, ensure all index values were deep copied.
    ;; In this case, the characters will be = to each other.
    (dotimes (i (array-total-size c))
      (assert (= (row-major-aref c i)
                  (row-major-aref o i))))

    (eql-map-dump-stats eql-map)
    (validate-eql-map-stats
     (eql-map-stats-match-p eql-map
                            `(:move-original ((signed-byte 32) ,num-elems))
                            `(:allocation (,o-type 1))
                            `(:array-clone-speed-fast (,o-type 1))))

    (format t "Passed.~%")
    t))

(defun test-clone-deep-array-simple-array-unsigned-byte-64-0 ()
  (u:mvlet* ((num-elems 8)
             ;; Type (SIMPLE-ARRAY (UNSIGNED-BYTE 64) (*))
             (o o-type (id-type (make-array num-elems
                                            :element-type '(unsigned-byte 64)
                                            :initial-element 0)))
             ;; All intentions have the same behavior for arrays.
             (c eql-map (clone-deep o (make-eql-map-with-stats))))

    (format t "Original | ~S~%" o)
    (format t "Cloned   | ~S~%" c)
    (finish-output)

    ;; Ensure it is newly allocated.
    (assert (not (eq c o)))

    (assert-matching-array-properties c o)

    ;; Then, ensure all index values were deep copied.
    ;; In this case, the characters will be = to each other.
    (dotimes (i (array-total-size c))
      (assert (= (row-major-aref c i)
                  (row-major-aref o i))))

    (eql-map-dump-stats eql-map)
    (validate-eql-map-stats
     (eql-map-stats-match-p eql-map
                            `(:move-original ((unsigned-byte 64) ,num-elems))
                            `(:allocation (,o-type 1))
                            `(:array-clone-speed-fast (,o-type 1))))

    (format t "Passed.~%")
    t))

(defun test-clone-deep-array-simple-array-signed-byte-64-0 ()
  (u:mvlet* ((num-elems 8)
             ;; Type (SIMPLE-ARRAY (SIGNED-BYTE 64) (*))
             (o o-type (id-type (make-array num-elems
                                            :element-type '(signed-byte 64)
                                            :initial-element 0)))
             ;; All intentions have the same behavior for arrays.
             (c eql-map (clone-deep o (make-eql-map-with-stats))))

    (format t "Original | ~S~%" o)
    (format t "Cloned   | ~S~%" c)
    (finish-output)

    ;; Ensure it is newly allocated.
    (assert (not (eq c o)))

    (assert-matching-array-properties c o)

    ;; Then, ensure all index values were deep copied.
    ;; In this case, the characters will be = to each other.
    (dotimes (i (array-total-size c))
      (assert (= (row-major-aref c i)
                  (row-major-aref o i))))

    (eql-map-dump-stats eql-map)
    (validate-eql-map-stats
     (eql-map-stats-match-p eql-map
                            `(:move-original ((signed-byte 64) ,num-elems))
                            `(:allocation (,o-type 1))
                            `(:array-clone-speed-fast (,o-type 1))))

    (format t "Passed.~%")
    t))

(defun test-clone-deep-array-simple-array-double-float-0 ()
  (u:mvlet* ((v0 v0-type (id-type 1d0))
             (num-elems 8)
             ;; Type (SIMPLE-ARRAY DOUBLE-FLOAT (*))
             (o o-type (id-type (make-array num-elems
                                            :element-type 'double-float
                                            :initial-element v0)))
             ;; All intentions have the same behavior for arrays.
             (c eql-map (clone-deep o (make-eql-map-with-stats))))

    (format t "Original | ~S~%" o)
    (format t "Cloned   | ~S~%" c)
    (finish-output)

    ;; Ensure it is newly allocated.
    (assert (not (eq c o)))

    (assert-matching-array-properties c o)

    ;; Then, ensure all index values were deep copied.
    ;; In this case, the characters will be = to each other.
    (dotimes (i (array-total-size c))
      (assert (= (row-major-aref c i)
                  (row-major-aref o i))))

    (eql-map-dump-stats eql-map)
    (validate-eql-map-stats
     (eql-map-stats-match-p eql-map
                            `(:move-original (,v0-type ,num-elems))
                            `(:allocation (,o-type 1))
                            `(:array-clone-speed-fast (,o-type 1))))

    (format t "Passed.~%")
    t))

(defun test-clone-deep-array-simple-string-0 ()
  (u:mvlet* ((v0 v0-type (id-type #\a))
             (num-elems 8)
             ;; Type SIMPLE-STRING aka (SIMPLE-ARRAY CHARACTER *)
             (o o-type (id-type (make-sequence 'simple-string num-elems
                                               :initial-element v0)))
             ;; All intentions have the same behavior for arrays.
             (c eql-map (clone-deep o (make-eql-map-with-stats))))

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

    (eql-map-dump-stats eql-map)
    (validate-eql-map-stats
     (eql-map-stats-match-p eql-map
                            `(:move-original (,v0-type ,num-elems))
                            `(:allocation (,o-type 1))
                            `(:array-clone-speed-slow (,o-type 1))))

    (format t "Passed.~%")
    t))

(defun test-clone-deep-array-simple-bit-vector-0 ()
  (u:mvlet* ((v0 v0-type (id-type 0))
             (num-elems 8)
             ;; Type SIMPLE-BIT-VECTOR
             (o o-type (id-type (make-sequence '(vector bit) num-elems
                                               :initial-element v0)))
             ;; All intentions have the same behavior for arrays.
             (c eql-map (clone-deep o (make-eql-map-with-stats))))

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

    (eql-map-dump-stats eql-map)
    (validate-eql-map-stats
     (eql-map-stats-match-p eql-map
                            `(:move-original (,v0-type ,num-elems))
                            `(:allocation (,o-type 1))
                            `(:array-clone-speed-fast (,o-type 1))))

    (format t "Passed.~%")
    t))

(defun test-clone-deep-array-bit-vector-0 ()
  (u:mvlet* ((v0 v0-type (id-type 0))
             (num-elems 8)
             ;; Type BIT-VECTOR
             (o o-type (id-type (make-array num-elems  :element-type 'bit
                                                       :adjustable t
                                                       :initial-element v0)))
             ;; All intentions have the same behavior for arrays.
             (c eql-map (clone-deep o (make-eql-map-with-stats))))

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

    (eql-map-dump-stats eql-map)
    (validate-eql-map-stats
     (eql-map-stats-match-p eql-map
                            `(:move-original (,v0-type ,num-elems))
                            `(:allocation (,o-type 1))
                            `(:array-clone-speed-fast (,o-type 1))))

    (format t "Passed.~%")
    t))

(defun test-clone-deep-array-unique-simple-array-0 ()
  (u:mvlet* ((v0 v0-type (id-type 23.5))
             (num-elems 8)
             ;; Type SIMPLE-ARRAY
             (o o-type (id-type (make-array num-elems
                                            :element-type 'single-float
                                            :initial-element v0)))
             ;; All intentions have the same behavior for arrays.
             (c eql-map (clone-deep o (make-eql-map-with-stats))))

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

    (eql-map-dump-stats eql-map)
    (validate-eql-map-stats
     (eql-map-stats-match-p eql-map
                            `(:move-original (,v0-type ,num-elems))
                            `(:allocation (,o-type 1))
                            `(:array-clone-speed-fast (,o-type 1))))

    (format t "Passed.~%")
    t))

(defun test-clone-deep-array-shared-simple-array-0 ()
  (u:mvlet* ((v0 v0-type (id-type 1))
             (v1 v1-type (id-type 2))
             (v2 v2-type (id-type (cons v0 v1)))
             (dims '(3 4))
             ;; Type (SIMPLE-ARRAY T (*))
             (o o-type (id-type (make-array dims
                                            :element-type 'cons
                                            :initial-element v2)))
             ;; All intentions have the same behavior for arrays.
             (c eql-map (clone-deep o (make-eql-map-with-stats))))

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

    (eql-map-dump-stats eql-map)
    (validate-eql-map-stats
     (eql-map-stats-match-p eql-map
                            `(:move-original (,v0-type 1)
                                             (,v1-type 1))
                            ;; v2's clone is copied 11 additional times into
                            ;; the array.
                            `(:move-clone (,v2-type 11))
                            `(:allocation (,v2-type 1)
                                          (,o-type 1))
                            `(:array-clone-speed-slow (,o-type 1))))

    (format t "Passed.~%")
    t))

(defun test-clone-deep-array-unique-simple-vector-0 ()
  (u:mvlet* ((v0 v0-type (id-type 0))
             (num-elems 8)
             ;; Type SIMPLE-VECTOR
             (o o-type (id-type (make-array num-elems :initial-element v0)))
             ;; All intentions have the same behavior for arrays.
             (c eql-map (clone-deep o (make-eql-map-with-stats))))

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

    (eql-map-dump-stats eql-map)
    (validate-eql-map-stats
     (eql-map-stats-match-p eql-map
                            `(:move-original (,v0-type ,num-elems))
                            `(:allocation (,o-type 1))
                            `(:array-clone-speed-slow (,o-type 1))))

    (format t "Passed.~%")
    t))

(defun test-clone-deep-array-shared-simple-vector-0 ()
  (u:mvlet* ((v0 v0-type (id-type 1))
             (v1 v1-type (id-type 2))
             (v2 v2-type (id-type (cons v0 v1)))
             (num-elems 4)
             ;; Type SIMPLE-VECTOR
             (o o-type (id-type (make-array num-elems :initial-element v2)))
             ;; All intentions have the same behavior for arrays.
             (c eql-map (clone-deep o (make-eql-map-with-stats))))

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

    (eql-map-dump-stats eql-map)
    (validate-eql-map-stats
     (eql-map-stats-match-p eql-map
                            `(:move-original (,v0-type 1)
                                             (,v1-type 1))
                            ;; The moved clone of v2
                            `(:move-clone (,v2-type 3))
                            `(:allocation (,o-type 1)
                                          ;; The allocation of v2
                                          (,v2-type 1))
                            `(:array-clone-speed-slow (,o-type 1))))

    (format t "Passed.~%")
    t))

(defun test-clone-deep-array-unique-vector-0 ()
  (u:mvlet* ((v0 v0-type (id-type 0))
             (num-elems 4)
             ;; Type VECTOR
             (o o-type (id-type (make-array num-elems :adjustable t
                                                      :initial-element v0)))
             ;; All intentions have the same behavior for arrays.
             (c eql-map (clone-deep o (make-eql-map-with-stats))))

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

    (eql-map-dump-stats eql-map)
    (validate-eql-map-stats
     (eql-map-stats-match-p eql-map
                            `(:move-original (,v0-type ,num-elems))
                            `(:allocation (,o-type 1))
                            `(:array-clone-speed-slow (,o-type 1))))

    (format t "Passed.~%")
    t))

(defun test-clone-deep-array-shared-vector-0 ()
  (u:mvlet* ((v0 v0-type (id-type 1))
             (v1 v1-type (id-type 2))
             (v2 v2-type (id-type (cons v0 v1)))
             (num-elems 4)
             ;; Type VECTOR
             (o o-type (id-type (make-array num-elems :adjustable t
                                                      :initial-element v2)))
             ;; All intentions have the same behavior for arrays.
             (c eql-map (clone-deep o (make-eql-map-with-stats))))

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

    (eql-map-dump-stats eql-map)
    (validate-eql-map-stats
     (eql-map-stats-match-p eql-map
                            `(:move-original (,v0-type 1)
                                             (,v1-type 1))
                            `(:move-clone (,v2-type 3))
                            `(:allocation (,o-type 1)
                                          (,v2-type 1))
                            `(:array-clone-speed-slow (,o-type 1))))

    (format t "Passed.~%")
    t))

(defun test-clone-deep-array-unique-array-0 ()
  (u:mvlet* ((v0 v0-type (id-type 0))
             (dims '(3 4))
             ;; Type ARRAY
             (o o-type (id-type (make-array dims :adjustable t
                                                 :initial-element v0)))
             ;; All intentions have the same behavior for arrays.
             (c eql-map (clone-deep o (make-eql-map-with-stats))))

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

    (eql-map-dump-stats eql-map)
    (validate-eql-map-stats
     (eql-map-stats-match-p eql-map
                            `(:move-original (,v0-type ,(array-total-size o)))
                            `(:allocation (,o-type 1))
                            `(:array-clone-speed-slow (,o-type 1))))

    (format t "Passed.~%")
    t))

(defun test-clone-deep-array-shared-array-0 ()
  (u:mvlet* ((v0 v0-type (id-type 1))
             (v1 v1-type (id-type 2))
             (v2 v2-type (id-type (cons 1 2)))
             (dims '(3 4))
             ;; Type ARRAY
             (o o-type (id-type (make-array dims :adjustable t
                                                 :initial-element v2)))
             ;; All intentions have the same behavior for arrays.
             (c eql-map (clone-deep o (make-eql-map-with-stats))))

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

    (eql-map-dump-stats eql-map)
    (validate-eql-map-stats
     (eql-map-stats-match-p eql-map
                            `(:move-original (,v0-type 1)
                                             (,v1-type 1))
                            ;; the clone of v2 is copied this many times. 1-
                            ;; because we only count the additional time beyond
                            ;; the first one for moving the clone.
                            `(:move-clone
                              (,v2-type ,(1- (array-total-size o))))
                            `(:allocation (,o-type 1)
                                          (,v2-type 1))
                            `(:array-clone-speed-slow (,o-type 1))))

    (format t "Passed.~%")
    t))

(defun test-clone-deep-array-simple-base-string-0 ()
  (u:mvlet* ((v0 v0-type (id-type #\a))
             (num-elems 8)
             ;; Type SIMPLE-BASE-STRING
             (o o-type (id-type (make-array num-elems :element-type 'base-char
                                                      :initial-element v0)))
             ;; All intentions have the same behavior for arrays.
             (c eql-map (clone-deep o (make-eql-map-with-stats))))

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

    (eql-map-dump-stats eql-map)
    (validate-eql-map-stats
     (eql-map-stats-match-p eql-map
                            `(:move-original (,v0-type ,num-elems))
                            `(:allocation (,o-type 1))
                            `(:array-clone-speed-slow (,o-type 1))))

    (format t "Passed.~%")
    t))

(defun test-clone-deep-array-base-string-0 ()
  (u:mvlet* ((v0 v0-type (id-type #\a))
             (num-elems 8)
             ;; Type BASE-STRING
             (o o-type (id-type (make-array num-elems :element-type 'base-char
                                                      :adjustable t
                                                      :initial-element v0)))
             ;; All intentions have the same behavior for arrays.
             (c eql-map (clone-deep o (make-eql-map-with-stats))))

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

    (eql-map-dump-stats eql-map)
    (validate-eql-map-stats
     (eql-map-stats-match-p eql-map
                            `(:move-original (,v0-type ,num-elems))
                            `(:allocation (,o-type 1))
                            `(:array-clone-speed-slow (,o-type 1))))

    (format t "Passed.~%")
    t))

;;; ------------------------------------------------------------
;; clone-deep hash-tables.
;;
;; NOTE: The graphs only use arrays, cons cells, and identity-clone values.
;;; ------------------------------------------------------------

(defun test-clone-deep-hash-table-0 ()
  "Can an empty hash table be deep copied and preserve its properties?"
  (u:mvlet* ((o o-type (id-type (u:dict)))
             (c eql-map (clone-deep o (make-eql-map-with-stats))))

    (format t "Original | ~S~%" o)
    (format t "Cloned   | ~S~%" c)
    (finish-output)

    (assert (not (eq c o)))

    (assert-matching-hash-table-properties c o)

    (eql-map-dump-stats eql-map)
    (validate-eql-map-stats
     (eql-map-stats-match-p eql-map
                            `(:allocation (,o-type 1))))

    (format t "Passed.~%")
    t))

(defun test-clone-deep-hash-table-1 ()
  "keys and values share no structure and are identity-policy style objects."
  (u:mvlet* ((k0 k0-type (id-type 0))
             (v0 v0-type (id-type 1))
             (k1 k1-type (id-type #\a))
             (v1 v1-type (id-type #\b))
             (k2 k2-type (id-type 'foo))
             (v2 v2-type (id-type 'bar))
             (k3 k3-type (id-type (lambda () 100)))
             (v3 v3-type (id-type (lambda () 200)))
             (k4 k4-type (id-type #P"/tmp/foo.txt"))
             (v4 v4-type (id-type #P"/tmp/bar.txt"))
             (o o-type (id-type (u:dict #'eql k0 v0 k1 v1 k2 v2 k3 v3 k4 v4)))
             (c eql-map (clone-deep o (make-eql-map-with-stats))))

    (format t "Original | ~S~%" o)
    (format t "Cloned   | ~S~%" c)
    (finish-output)

    ;; Ensure it is newly allocated
    (assert (not (eq c o)))
    (assert-matching-hash-table-properties c o)

    ;; TODO: I need to write a structure checker on deep cloned key/value
    ;; pairs. This is not obvious...

    (eql-map-dump-stats eql-map)
    (validate-eql-map-stats
     (eql-map-stats-match-p eql-map
                            `(:move-original (,k0-type 1)
                                             (,k1-type 1)
                                             (,k2-type 1)
                                             (,k3-type 1)
                                             (,k4-type 1)
                                             (,v0-type 1)
                                             (,v1-type 1)
                                             (,v2-type 1)
                                             (,v3-type 1)
                                             (,v4-type 1))
                            `(:allocation (,o-type 1))))

    (format t "Passed.~%")
    t))

(defun test-clone-deep-hash-table-2 ()
  "keys and values share no structure and are not identity-policy style values"
  (u:mvlet* ((e0 e0-type (id-type 42))
             (k0-elems 8)
             (k0 k0-type (id-type (make-array k0-elems :initial-element e0)))
             (v0 v0-type (id-type "Hello World"))

             (e1 e1-type (id-type 10))
             (e2 e2-type (id-type 20))
             (e3 e3-type (id-type 30))
             (k1 k1-type (id-type (list e1 e2 e3)))
             (v1 v1-type (id-type (u:dict #'eq)))

             (e4 e4-type (id-type 100))
             (e5 e5-type (id-type 200))
             (k2 k2-type (id-type (u:dict #'eql)))
             (v2 v2-type (id-type (cons e4 e5)))

             (e6 e6-type (id-type #'+))
             (e7 e7-type (id-type #'-))
             (e8 e8-type (id-type #'*))
             (e9 e9-type (id-type #'/))
             (k3 k3-type (id-type (list e6 e7)))
             (v3 v3-type (id-type (list e8 e9)))

             (e10 e10-type (id-type (u:dict #'equal)))
             (k4 k4-type (id-type #*11011))
             (v4 v4-type (id-type (list e10)))

             (list-end-type (type-of nil))
             (o o-type (id-type (u:dict #'equal
                                        k0 v0 k1 v1 k2 v2 k3 v3 k4 v4)))
             (c eql-map (clone-deep o (make-eql-map-with-stats))))

    (format t "Original | ~S~%" o)
    (u:do-hash (k v o)
      (format t "         > ~S -> ~S~%" k v))
    (format t "Cloned   | ~S~%" c)
    (u:do-hash (k v c)
      (format t "         > ~S -> ~S~%" k v))
    (finish-output)

    ;; Ensure it is newly allocated
    (assert (not (eq c o)))
    (assert-matching-hash-table-properties c o)

    ;; TODO: I need to write a better structure checker on deep cloned
    ;; key/value pairs. This is not obvious...
    (u:do-hash (k v o)
      (assert (not (eq k v))))
    (u:do-hash (k v c)
      (assert (not (eq k v))))

    (eql-map-dump-stats eql-map)
    (validate-eql-map-stats
     (eql-map-stats-match-p eql-map
                            `(:move-original (,e0-type ,k0-elems) ;; (int 0 xx)
                                             (standard-char 11) ;; std-char
                                             (,e1-type 1) ;; (int 0 xx)
                                             (,e2-type 1) ;; (int 0 xx)
                                             (,e3-type 1) ;; (int 0 xx)
                                             (,e4-type 1) ;; (int 0 xx)
                                             (,e5-type 1) ;; (int 0 xx)
                                             (,e6-type 1) ;; COMPILED-FUNCTION
                                             (,e7-type 1) ;; COMPILED-FUNCTION
                                             (,e8-type 1) ;; COMPILED-FUNCTION
                                             (,e9-type 1) ;; COMPILED-FUNCTION
                                             (bit 5) ;; #* bit vector
                                             (,list-end-type 4))
                            `(:allocation (,v1-type 1) ;; HASH-TABLE
                                          (,k2-type 1) ;; HASH-TABLE
                                          (,e10-type 1) ;; HASH-TABLE
                                          (,k1-type 3) ;; CONS
                                          (,v2-type 1) ;; CONS
                                          (,k3-type 2) ;; CONS
                                          (,v3-type 2) ;; CONS
                                          (,v4-type 1) ;; CONS
                                          (,k0-type 1) ;; (SIMPLE-VECTOR 8)
                                          (,v0-type 1) ;; (S-ARRAY CHAR (11))
                                          (,k4-type 1) ;; (S-BIT-VECTOR 5)
                                          (,o-type 1)) ;; HASH-TABLE
                            `(:array-clone-speed-fast (,k4-type 1))
                            `(:array-clone-speed-slow (,k0-type 1)
                                                      (,v0-type 1))))

    (format t "Passed.~%")
    t))

(defun test-clone-deep-hash-table-3 ()
  "Key and Value are the identical list causing shared structure in the hash
table."
  (u:mvlet* ((e0 e0-type (id-type 0))
             (e1 e1-type (id-type 10))
             (list-end-type (type-of nil))
             (kv0 kv0-type (id-type (list e0 e1)))
             (o o-type (id-type (u:dict #'eql kv0 kv0)))
             (c eql-map (clone-deep o (make-eql-map-with-stats))))

    (format t "Original | ~S~%" o)
    (format t "Cloned   | ~S~%" c)
    (finish-output)

    ;; Ensure it is newly allocated
    (assert (not (eq c o)))
    (assert-matching-hash-table-properties c o)

    ;; TODO: I need to write a structure checker on deep cloned key/value
    ;; pairs. This is not obvious...
    ;; NOTE: This only works with ONE key/value pair and that's ok for now.
    (loop :for (ok . ov) :in (u:hash->alist o)
          :for (ck . cv) :in (u:hash->alist c)
          :do (assert (eq ok ov))
              (assert (eq ck cv))
              (assert (not (eq ok ck)))
              (assert (not (eq ov cv))))

    (eql-map-dump-stats eql-map)
    (validate-eql-map-stats
     (eql-map-stats-match-p eql-map
                            `(:move-original (,e0-type 1)
                                             (,e1-type 1)
                                             (,list-end-type 1))
                            `(:move-clone (,kv0-type 1))
                            `(:allocation (,kv0-type 2)
                                          (,o-type 1))))

    (format t "Passed.~%")
    t)
  )

;; TODO: We can always add more tests.



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

  (test-eql-map)

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
  (test-clone-deep-cons-7)
  (test-clone-deep-cons-8)

  ;; The arrays may hold references to themselves, to nested arrays, to cons
  ;; cells, and to identity-like values.
  (test-clone-deep-array-simple-array-single-float-0)
  (test-clone-deep-array-simple-array-unsigned-byte-8-0)
  (test-clone-deep-array-simple-array-signed-byte-8-0)
  (test-clone-deep-array-simple-array-unsigned-byte-16-0)
  (test-clone-deep-array-simple-array-signed-byte-16-0)
  (test-clone-deep-array-simple-array-unsigned-byte-32-0)
  (test-clone-deep-array-simple-array-signed-byte-32-0)
  (test-clone-deep-array-simple-array-unsigned-byte-64-0)
  (test-clone-deep-array-simple-array-signed-byte-64-0)
  (test-clone-deep-array-simple-array-double-float-0)
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
  (test-clone-deep-hash-table-2)
  (test-clone-deep-hash-table-3)

  (format t "Deep clone tests passed!~%")
  t)


;;; ---------------------------------------------------------------------------
;; NEW TYPE cloning tests.
;;
;; Herein we construct a couple new types and make shallow and deep clones of
;; them to ensure things work out.
;;; ---------------------------------------------------------------------------

;; %bar class and with clone API

(defclass %foo ()
  ((%junk :accessor %junk
          :initarg :%junk
          :initform 0)))

(defmethod allocatablep ((object %foo))
  t)

(defmethod clone-allocate ((object %foo) eql-map)
  (eql-map-record eql-map object :allocation)
  (make-instance '%foo))

(defmethod clone-object progn ((cloned-object %foo)
                               (original-object %foo)
                               (policy deep-clone)
                               (intention graph-intention)
                               (last-known-intention no-specific-intention)
                               eql-map &key)

  (setf (%junk cloned-object)
        (clone-deep (%junk original-object) eql-map))
  cloned-object)

;; %foo class and with clone API

(defclass %bar (%foo)
  ((%feh :accessor %feh
         :initarg :%feh
         :initform 0)))

(defmethod allocatablep ((object %bar))
  t)

(defmethod clone-allocate ((object %bar) eql-map)
  (eql-map-record eql-map object :allocation)
  (make-instance '%bar))

(defmethod clone-object progn ((cloned-object %bar)
                               (original-object %bar)
                               (policy deep-clone)
                               (intention graph-intention)
                               (last-known-intention no-specific-intention)
                               eql-map &key)

  (setf (%feh cloned-object)
        (clone-deep (%feh original-object) eql-map))
  cloned-object)

;; The test that uses %foo and %bar

(defun test-clone-deep-new-type-%foo/%bar-0 ()
  (u:mvlet* ((e0 e0-type (id-type 42))
             (e1 e1-type (id-type 55))
             (%b %b-type (id-type (make-instance '%bar :%junk e0 :%feh e1)))
             (%c eql-map (clone-deep %b (make-eql-map-with-stats))))

    (format t "Original | ~S~%" %b)
    (format t "Cloned   | ~S~%" %c)
    (finish-output)

    (assert (not (eq %b %c)))
    (assert (= e0 (%junk %b) (%junk %c)))
    (assert (= e1 (%feh %b) (%feh %c)))

    (eql-map-dump-stats eql-map)
    (validate-eql-map-stats
     (eql-map-stats-match-p eql-map
                            `(:move-original (,e0-type 1)
                                             (,e1-type 1))
                            `(:allocation (,%b-type 1))))

    (format t "Passed!~%")
    t))

;;; ------------------------------------------------------------
;; Aggregate NEW TYPE cloning testing code.
;;; ------------------------------------------------------------

(defun test-clone-new-type ()
  (format t "New Type clone tests.~%")

  (test-clone-deep-new-type-%foo/%bar-0)

  (format t "New Type clone tests passed!~%")
  t)

;;; ---------------------------------------------------------------------------
;; Entry point for entire clone system test.
;;; ---------------------------------------------------------------------------

(defun test-clone ()
  (test-clone-shallow)
  (test-clone-deep)
  (test-clone-new-type))
