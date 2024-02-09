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
    (eql-map-mark-transition eql-map o0 42 (make-graph-intention) )
    (eql-map-mark-visited eql-map o1)
    (eql-map-mark-visited eql-map o2)
    (eql-map-mark-visited eql-map o3)
    (eql-map-mark-visited eql-map o4)
    (eql-map-mark-transition eql-map o4 (make-hash-table)
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
;; clone-shallow of TREE intention
;;; ------------------------------------------------------------
