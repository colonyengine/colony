(in-package #:virality.clone)

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

;;; ------------------------------------------------------------
;; clone-shallow of CONS intention
;;; ------------------------------------------------------------

(defun test-clone-shallow-cons-0 ()
  (let* ((o (cons 1 (make-hash-table)))
         (c (clone-shallow-cons o)))

    (format t "Original: ~S~%" o)
    (format t "Cloned: ~S~%" c)

    ;; This part must be cloned.
    (assert (not (eq o c)))

    ;; but the values of the car and cdr must still be eq/eql to original.
    (assert (eql (car c) (car o)))
    (assert (eq (cdr c) (cdr o)))

    (format t "Passed.~%")
    t))

(defun test-clone-shallow-cons-1 ()
  (let* ((*print-circle* t)
         (o (cons nil nil)))
    ;; Both car and cdr point to o.
    (setf (car o) o
          (cdr o) o)

    (let ((c (clone-shallow-cons o)))
      (format t "Original: ~S~%" o)
      (format t "Cloned: ~S~%" c)
      (finish-output)

      ;; The shallow clone created a new memory allocation.
      (assert (not (eq o c)))

      ;; But the very surprising thing is that the car and cdr were shallow
      ;; copied. They still point to the original o! This means that o and
      ;; c cannot be EQUAL to each other when cycles are present and a shallow
      ;; copy happens.
      (assert (eq (car c) o))
      (assert (eq (cdr c) o))

      (format t "Passed (with expected surprise due to cycle).~%")
      t)))

;;; ------------------------------------------------------------
;; clone-shallow of LIST intention
;;; ------------------------------------------------------------

(defun test-clone-shallow-list-0 ()
  "Test the usual case of a proper list with interesting stuff in it."
  (let* ((o (list 'a 1 (cons #\a #\b) (make-hash-table) (list 10 20 30)))
         (c (clone-shallow-list o)))
    (format t "Original: ~S~%" o)
    (format t "Cloned: ~S~%" c)

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

    (format t "Changed Original: ~S~%" o)
    (format t "Changed Cloned: ~S~%" c)
    (finish-output)

    ;; Check certain constraints after the changes.

    (assert (not (equal o c)))
    (assert (not (eq (nth 0 o) (nth 0 c))))
    (assert (eql (nth 1 o) 10))
    (assert (eql (nth 1 c) 100))
    (assert (eq (nth 2 o) (nth 2 c)))
    (assert (eq (nth 3 o) (nth 3 c)))
    (assert (eq (nth 4 o) (nth 4 o)))

    (format t "Passed.~%")
    t))

(defun test-clone-shallow-list-1 ()
  "Handle entries in the list which are identical."
  (let* ((*print-circle* t)
         (v (cons 1 2))
         (o (list (cons 'a 'b) v v (cons 'c 'd)))
         (c (clone-shallow-list o)))
    (format t "Original: ~S~%" o)
    (format t "Cloned: ~S~%" c)
    (finish-output)

    (assert (equal o c))
    (assert (every #'consp o))
    (assert (every #'consp c))

    ;; The list structure cons cells must be different.
    (loop :for o-cell :on o
          :for c-cell :on c
          :do (assert (not (eq o-cell c-cell))))

    ;; But the actual entries in the lists were shallow copied.
    (loop :for a :in o
          :for b :in c
          :do (assert (eq a b))) ;; NOTE: expecting all cons cells in list.

    (format t "Passed.~%")
    t))

(defun test-clone-shallow-list-2 ()
  "Handle a cons cell that references itself in both car and cdr."
  (let* ((*print-circle* t)
         (o (cons nil nil)))
    ;; A single cons cell representing a list that has both itself as the first
    ;; element and a cycle for the rest of the list.
    (setf (car o) o
          (cdr o) o)

    (let ((c (clone-shallow-list o)))
      (format t "Original: ~S~%" o)
      (format t "Cloned: ~S~%" c)
      (finish-output)

      (assert (eq o (car o)))
      (assert (eq o (cdr o)))

      ;; The clone must have produced a new starting cons cell.
      (assert (not (eq c o)))
      ;; The car should have been shallow copied to original list.
      (assert (eq (car c) o))
      ;; But notice the cdr is pointing into the cloned list structure.
      (assert (eq (cdr c) c))

      (format t "Passed with expected surprise in list structure.~%")
      t)))

(defun test-clone-shallow-list-3 ()
  "Cdr of last cons cell points to head of list cons in a cycle."
  (let* ((*print-circle* t)
         (o (list 1 2 3 nil)))
    ;; Last cons cell cdr points to head of list in a cycle.
    (setf (cdddr o) o)
    (format t "Original: ~S~%" o)
    (finish-output)

    (let ((c (clone-shallow-list o)))
      (format t "Cloned: ~S~%" c)
      (finish-output)

      (assert (eq o (cdddr o)))
      (assert (eq c (cdddr c)))
      (assert (not (eq o c)))
      (assert (not (eq (cdddr o) (cdddr c))))

      (format t "Passed.~%")
      t)))

(defun test-clone-shallow-list-4 ()
  "Cdr of last cons cell points to middle of list cons in a cycle."
  (let* ((*print-circle* t)
         (o (list 1 2 3 nil)))
    ;; Last cons cell cdr points to middle of list in a cycle.
    (setf (cdddr o) (cdr o))
    (format t "Original: ~S~%" o)
    (finish-output)

    (let ((c (clone-shallow-list o)))
      (format t "Cloned: ~S~%" c)
      (finish-output)

      (assert (eq (cdr o) (cdddr o)))
      (assert (eq (cdr c) (cdddr c)))
      (assert (not (eq o c)))
      (assert (not (eq (cdddr o) (cdddr c))))
      (assert (not (eq (cdr o) (cdr c))))

      (format t "Passed.~%")
      t)))


;;; ------------------------------------------------------------
;; clone-shallow of ALIST intention
;;; ------------------------------------------------------------

(defun test-clone-shallow-alist-0 ()
  "Test a basic well formatted alist in the common form."
  (let* ((o (list (cons 'a #\a)
                  (cons 'b 'foo)
                  (cons 'c 3)
                  (cons 'd (make-hash-table))
                  (cons 'e (list 1 2 3))
                  (cons #\a #\b)
                  (cons (cons 1 2) (make-array 4))
                  (cons (make-hash-table) (lambda (x) x))))
         (c (clone-shallow-alist o)))
    (format t "Original: ~S~%" o)
    (format t "Cloned: ~S~%" c)

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

    (format t "Passed.~%")
    t))

(defun test-clone-shallow-alist-1 ()
  "Test reconstruction of shared reference kv cons cells."
  (let* ((v (cons (make-array 4) #\b))
         (o (list (cons 'a 1) v v (cons #\a (make-hash-table))))
         (c (clone-shallow-alist o)))
    (format t "Original: ~S~%" o)
    (format t "Cloned: ~S~%" c)
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

    (format t "Passed.~%")
    t))


;; 1. cyclic list structure in the alist.
(defun test-clone-shallow-alist-2 ()
  "Handle an alist that references itself in both car and cdr."
  (let* ((*print-circle* t)
         (o (cons nil nil)))

    ;; A single cons cell representing an alist that has both itself as the
    ;; first element and a cycle for the rest of the list.
    (setf (car o) o
          (cdr o) o)

    (let ((c (clone-shallow-alist o)))
      (format t "Original: ~S~%" o)
      (format t "Cloned: ~S~%" c)
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

      (format t "Passed.~%")
      t)))

(defun test-clone-shallow-alist-3 ()
  "An alist with one proper cons entry, but then a cycle to itself."
  (let ((*print-circle* t)
        (o (list (cons 1 2))))
    (setf (cdr o) o)
    (let ((c (clone-shallow-alist o)))
      (format t "Original: ~S~%" o)
      (format t "Cloned: ~S~%" c)
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

      (format t "Passed.~%")
      t)))

;; 2. not quite an alist, but still proper list.
(defun test-clone-shallow-alist-4 ()
  "An alist with additional non-kv cells entries in it and no cycles."
  (let* ((o (list (cons 1 2) 'foo (cons 3 4) 42))
         (c (clone-shallow-alist o)))
    (format t "Original: ~S~%" o)
    (format t "Cloned: ~S~%" c)
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

    (format t "Passed.~%")
    t))

(defun test-clone-shallow-alist-5 ()
  "An alist with additional non-kv cells entries it ending improperly."
  (let ((o (list (cons 1 2) 'foo 42)))
    (setf (cdr (cddr o)) 77)
    (let ((c (clone-shallow-alist o)))
      (format t "Original: ~S~%" o)
      (format t "Cloned: ~S~%" c)
      (finish-output)

      ;; Check that the list structure was cloned.
      (loop :for o-cell :on o
            :for c-cell :on c
            :do (assert (not (eq o-cell c-cell))))

      ;; check contents of first element cons cell.
      (assert (and (eql (caar o) (caar c))))
      (assert (and (eql (cdar o) (cdar c))))

      ;; check second element
      (assert (eq (nth 1 o) (nth 1 c)))

      ;; check third element improper cons cell.
      (assert (eql (car (cddr o)) (car (cddr c))))
      (assert (eql (cdr (cddr o)) (cdr (cddr c))))

      (format t "Passed.~%")
      t)))


;;; ------------------------------------------------------------
;; clone-shallow of GRAPH intention
;;; ------------------------------------------------------------

(defun test-clone-shallow-graph-0 ()
  "Handle a cons cell that references two distinct atomic-like things."
  (let* ((*print-circle* t)
         (graph (cons-graph
                  `((o :l (:v 100))
                    (o :r (:v 200)))))
         (o (get-roots graph 'o)))

    (let ((c (clone-shallow-graph o)))
      (format t "Original: ~S~%" o)
      (format t "Cloned: ~S~%" c)
      (finish-output)

      (assert (eql (car o) 100))
      (assert (eql (cdr o) 200))

      ;; The clone must have produced a new starting cons cell.
      (assert (not (eq c o)))

      (setf (car c) 300
            (cdr c) 400)
      (format t "Modified Cloned: ~S~%" c)
      ;; make sure didn't mess with original.
      (assert (eql (car o) 100))
      (assert (eql (cdr o) 200))
      ;; Ensure the change took in the clone.
      (assert (eql (car c) 300))
      (assert (eql (cdr c) 400))

      (format t "Passed.~%")
      t)))

(defun test-clone-shallow-graph-1 ()
  "Handle a cons cell whose car/cdr point to the same atomic thing."
  (let* ((*print-circle* t)
         (v (make-hash-table))
         (graph (cons-graph
                  `((o :l (:v ,v))
                    (o :r (:v ,v)))))
         (o (get-roots graph 'o)))

    (let ((c (clone-shallow-graph o)))
      (format t "Original: ~S~%" o)
      (format t "Cloned: ~S~%" c)
      (finish-output)

      ;; Make sure original is still sane.
      (assert (eq (car o) v))
      (assert (eq (cdr o) v))

      ;; The clone must have produced a new starting cons cell.
      (assert (not (eq c o)))
      ;; The car should have preserved the graph structure and point to the NEW
      ;; starting cons cell.
      (assert (eq (car c) v))
      ;; Same with the cdr
      (assert (eq (cdr c) v))

      (format t "Passed.~%")
      t)))

(defun test-clone-shallow-graph-2 ()
  "Handle a small tree made with cons cells."
  (u:mvlet* ((*print-circle* t)
             (graph (cons-graph
                      `((:roots n0 n1 n2 n3)
                        (n0 :l n1)
                        (n0 :r n2)
                        (n1 :l (:v 1))
                        (n1 :r (:v 2))
                        (n2 :l n3)
                        (n2 :r (:v nil))
                        (n3 :l (:v 3))
                        (n3 :r (:v 4)))))
             (n0 n1 n2 n3 (get-roots graph 'n0 'n1 'n2 'n3)))

    (let* ((c (clone-shallow-graph n0))
           (c0 c)
           (c1 (car c0))
           (c2 (cdr c0))
           (c3 (car c2))
           (vlc1 (car c1))
           (vrc1 (cdr c1))
           (vlc3 (car c3))
           (vrc3 (cdr c3)))
      (format t "Original: ~S~%" n0)
      (format t "Cloned: ~S~%" c)
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

      (format t "Passed.~%")
      t)))

(defun test-clone-shallow-graph-3 ()
  "Handle a cons cell that references itself in both car and cdr."
  (let* ((*print-circle* t)
         (graph (cons-graph
                  `((:roots o)
                    (o :l o)
                    (o :r o))))
         (o (get-roots graph 'o)))

    (let ((c (clone-shallow-graph o)))
      (format t "Original: ~S~%" o)
      (format t "Cloned: ~S~%" c)
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

      (format t "Passed.~%")
      t)))


(defun test-clone-shallow-graph-4 ()
  "Handle a directed acyclic graph with shared structure."
  (flet ((neq (x y)
           (not (eq x y))))
    (u:mvlet* ((*print-circle* t)
               (val 1)
               (sym 'a)
               (path #P "/tmp/foo.txt")
               (str "foo")
               (ht (make-hash-table))
               (ch #\a)
               (graph (cons-graph
                        `((:roots o oml omr obl obm obr)
                          (o :l oml :l obl :l (:v ,val))
                          (o :r omr :r obr :r (:v ,path))
                          (oml :r obm)
                          (omr :l obm)
                          (obl :r (:v ,sym))
                          (obm :l (:v ,str))
                          (obm :r (:v ,ch))
                          (obr :l (:v ,ht)))))
               (obl obm obr oml omr o
                    (get-roots graph 'obl 'obm 'obr 'oml 'omr 'o))
               (c (clone-shallow-graph o)))

      (format t "Original: ~S~%" o)
      (format t "Cloned: ~S~%" c)
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
               (c (clone-shallow-graph l0)))

      (format t "Original: ~S~%" l0)
      (format t "Cloned: ~S~%" c)
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

        (format t "Passed.~%")

        t))))


;;; ------------------------------------------------------------
;; Higher level testing code.
;;; ------------------------------------------------------------

(defun test-clone-shallow ()
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

  t)

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
