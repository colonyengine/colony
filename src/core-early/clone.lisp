(in-package #:virality.clone)

;;;; Creation of CLONE-POLICIES
(defun make-identity-clone ()
  (make-instance 'identity-clone))

(defun make-shallow-clone ()
  (make-instance 'shallow-clone))

(defun make-shallow-clone-cons ()
  (make-instance 'shallow-clone-cons))

(defun make-shallow-clone-list ()
  (make-instance 'shallow-clone-list))

(defun make-shallow-clone-alist ()
  (make-instance 'shallow-clone-alist))

(defun make-shallow-clone-tree ()
  (make-instance 'shallow-clone-tree))

(defun make-deep-clone ()
  (make-instance 'deep-clone))

;; These are NOT to be exported. They basically represent an easy way to not
;; have memory churn in heavy copy situations.
(defparameter *identity* (make-identity-clone))
(defparameter *shallow* (make-shallow-clone))
(defparameter *shallow-cons* (make-shallow-clone-cons))
(defparameter *shallow-list* (make-shallow-clone-list))
(defparameter *shallow-alist* (make-shallow-clone-alist))
(defparameter *shallow-tree* (make-shallow-clone-tree))
(defparameter *deep* (make-deep-clone))

;; TODO: This interface and the functions below are sort of ugly...

;; Shortcut API for very common cloning policies.
(defun clone-identity (object &optional (eql-map nil eql-map-supp-p))
  "Perform an identity clone of the OBJECT. The clone is a nop and the OBJECT
is returned."
  (clone object *identity* (if eql-map-supp-p eql-map (make-eql-map))))

(defun clone-shallow (object &optional (eql-map nil eql-map-supp-p))
  "Perform a deep clone of the OBJECT and return a copy. Note that lists are
copied as if by COPY-LIST in a shallow copy."
  (clone object *shallow* (if eql-map-supp-p eql-map (make-eql-map))))

(defun clone-shallow-cons (object &optional (eql-map nil eql-map-supp-p))
  "Perform a deep clone of the OBJECT and return a copy. Note that lists only
have their very first cons cell shallow copied and that's it!"
  (clone object *shallow-cons* (if eql-map-supp-p eql-map (make-eql-map))))

(defun clone-shallow-list (object &optional (eql-map nil eql-map-supp-p))
  "Perform a deep clone of the OBJECT and return a copy. Note that lists only
have their toplevel cons cells shallow copied!"
  (clone object *shallow-list* (if eql-map-supp-p eql-map (make-eql-map))))

(defun clone-shallow-alist (object &optional (eql-map nil eql-map-supp-p))
  "Perform a deep clone of the OBJECT and return a copy. Note that lists are
treated as alists and only the alist structure is shallow copied!"
  (clone object *shallow-alist* (if eql-map-supp-p eql-map (make-eql-map))))

(defun clone-shallow-tree (object &optional (eql-map nil eql-map-supp-p))
  "Perform a deep clone of the OBJECT and return a copy. Note that lists are
treated as trees and only the entire tree structure is shallow copied which
will produce a new tree whose non-cons car values are simply copied!"
  (clone object *shallow-tree* (if eql-map-supp-p eql-map (make-eql-map))))

(defun clone-deep (object &optional (eql-map nil eql-map-supp-p))
  "Perform a deep clone of the OBJECT and return a copy."
  (clone object *deep* (if eql-map-supp-p eql-map (make-eql-map))))

;;; -------------------------------
;; The EQL-MAP API
;;; -------------------------------

(defun make-eql-map ()
  (make-instance 'eql-map))

(defun eql-map-initialize (eql-map)
  (clrhash (transition-table eql-map)))

(defun eql-map-ref (eql-map original-object)
  (u:href (transition-table eql-map) original-object))

(defun (setf eql-map-ref) (cloned-object eql-map original-object)
  (setf (u:href (transition-table eql-map) original-object) cloned-object))

(defun eql-map-transition (eql-map original-object)
  "If the ORIGINAL-OBJECT does not have a mapping in EQL-MAP, then return two
values, the ORIGINAL-OBJECT and NIL. If it does have a transition, return two
values: the object to which it transitioned and T."
  (u:mvlet ((object-trfm object-trfm-present-p
                         (eql-map-ref eql-map original-object)))
    (if object-trfm-present-p
        (values object-trfm t)
        (values original-object nil))))

;; TODO: This is very terrible, only for debugging at this time.
(defun dump-eql-map (eql-map)
  (format t "eql-map is:~%")
  (maphash
   (lambda (k v)
     (format t " ")
     (print-unreadable-object (k t :type t :identity t)
       (format t "~A" k))
     (format t " -> ")
     (print-unreadable-object (v t :type t :identity t)
       (format t "~A" v))
     (format t "~%"))
   (transition-table eql-map)))

;;; The CLONE API and CLONE-OBJECT API methods.  CLONE is the entry point to
;;; clone an object and must allocate the memory for the new object, if
;;; appropriate, it wll then call CLONE-OBJECT to complete the cloning process.
;;
;;; NOTE: In most places where it is intended to so an identity clone, I've not
;;; actually written the CLONE call and instead just used the raw value.  This
;;; is identical behavior and faster because CLONE isn't being resolved and
;;; used like cl:identity. However, we include it for when you DO need it.

;;; -------------------------------
;; The default IDENTITY-CLONE policy for any object is to return exactly itself
;; with no new memory allocation and no copying of information and no recursive
;; copying. This is the base cloning method for "by value"-like things such as
;; symbols, characters, functions/closures, numbers, and atomic things which
;; are not collections.
;;; -------------------------------
(defmethod clone (object (policy identity-clone) eql-map &key)
  (values object eql-map))

;; If it is a clone of an unknown type, return the transition if there is one.
(defmethod clone (object (policy allocating-clone) eql-map &key)
  (values (eql-map-transition eql-map object) eql-map))

;;; -------------------------------
;; Cloning a pathname
;;; -------------------------------

;; Since you can't seem to modify a pathname once constructed, we ignore
;; and just treat them as purely atomic and they return simply themselves.

;;; -------------------------------
;; Cloning a cons cell (or a list).
;;; -------------------------------
(defmethod clone ((object cons) (policy allocating-clone) eql-map &key)
  (let ((cloned-object (cons nil nil)))
    (setf (eql-map-ref eql-map object) cloned-object)
    (values (clone-object cloned-object object policy eql-map) eql-map)))

;; shallow-clone-cons shallow clones the SINGLE cons cell given to it with
;; no recursion or remapping what-so-ever.
(defmethod clone-object progn ((cloned-object cons)
                               (original-object cons)
                               (policy shallow-clone-cons)
                               eql-map
                               &key)

  (destructuring-bind (l . r) original-object
    (setf (car cloned-object) l
          (cdr cloned-object) r))
  cloned-object)


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

;; shallow-clone-list clones the toplevel list structure ONLY.
(defmethod clone-object progn ((cloned-object cons)
                               (original-object cons)
                               (policy shallow-clone-list)
                               eql-map
                               &key)
  ;; We use a visited table because we're walking the _original_ list
  ;; structure. We can't use the eql-map to also represent this visiting
  ;; information because the eql-map is only representing structure, not if
  ;; we've processed something already.
  (let ((visited-cells (make-hash-table :test #'eq)))
    ;; We've technically visited the start of the list already.
    (setf (u:href visited-cells original-object) t)

    (destructuring-bind (l . r) original-object
      ;; We always shallow copy the car no matter what it was.
      ;; NOTE: if the car points back into the list structure...too bad use
      ;; deep copy for such a thing.
      (setf (car cloned-object) l)

      ;; If the cdr isn't a cons (hence an improper list), the answer is easy.
      (unless (consp r)
        (setf (cdr cloned-object) r)
        (return-from clone-object cloned-object))

      ;; But if it was a cons, then the answer is much harder since the cdr can
      ;; represent: a complete proper list, a currently proper list that then
      ;; turns into an improper list, or a cycle at some point (including
      ;; immediately).

      (loop :with end = cloned-object
            :for original-cell :on r
            :do
               (if (u:href visited-cells original-cell)
                   ;; Base case of handling the cycle. We catch it right away.
                   (progn
                     (setf (cdr end)
                           (eql-map-transition eql-map original-cell))
                     (return))

                   ;; else the original-cell hasn't been encountered before in
                   ;; the list traversal, so copy it and continue (but beware
                   ;; the cdr might be an improper list!).
                   (let* ((new-cell (cons nil nil))
                          (l-original (car original-cell))
                          (r-original (cdr original-cell))
                          (list-continues-p (consp r-original)))

                     ;; visit the current original-cell we are about to
                     ;; processing.
                     (setf (u:href visited-cells original-cell) t)

                     ;; Insert the cons clone into the eql-map.
                     (setf (eql-map-ref eql-map original-cell) new-cell)

                     ;; The car of the new-cell is an easy fixup since we're
                     ;; doing a shallow copy. If the car points into the
                     ;; original list structure, you're going to have a bad
                     ;; time because you should use deep copy for that.
                     (setf (car new-cell) l-original)

                     ;; But for the cdr, we could discover that we have an
                     ;; improper list.
                     (if list-continues-p
                         (setf (cdr new-cell) nil)
                         (setf (cdr new-cell) (eql-map-transition eql-map
                                                                  r-original)))
                     (setf (cdr end) new-cell
                           end new-cell))))

      cloned-object)))

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


;; Do shallow-clone-alist. Shallow copy the list structure and the consp
;; in the car spot of each list cons cell when available. We do it manually
;; as opposed to recursive calls cause this needs to be efficient.
(defmethod clone-object progn ((cloned-object cons)
                               (original-object cons)
                               (policy shallow-clone-alist)
                               eql-map
                               &key)

  (flet ((clone-kv-cell (original-kv-cell)
           ;; Clone a key/value cell from the alist.
           (multiple-value-bind (maybe-cloned-kv-cell present-p)
               (eql-map-transition eql-map original-kv-cell)
             ;; However, check to see if the one we're cloning doesn't already
             ;; have a transition, if so, use that instead!  This covers cases
             ;; where an alist reused the exact kv cons cell multiple times in
             ;; the list. We replicate that identical structure in the clone.
             (if present-p
                 maybe-cloned-kv-cell
                 (destructuring-bind (l . r) original-kv-cell
                   ;; We specifically preserve the original contents in the
                   ;; shallow clone of the cons cell. If there is
                   ;; self-referential information here, there will be a
                   ;; surprise in the clone.
                   (let ((cloned-kv-cell (cons l r)))
                     ;; Update the eql-map table.
                     (setf (eql-map-ref eql-map original-kv-cell)
                           cloned-kv-cell)
                     cloned-kv-cell))))))

    (let ((visited-cells (make-hash-table :test #'eq)))
      ;; We've technically visited the start of the list already.
      (setf (u:href visited-cells original-object) t)

      (destructuring-bind (l . r) original-object
        ;; Handle first entry in the alist.
        (setf (car cloned-object)
              (if (consp l)
                  ;; We're told it is an alist and should be a key/value cons
                  ;; cell, so clone it.
                  (clone-kv-cell l)
                  ;; oops! not a kv-cell (and not really an alist), but just
                  ;; shallow copy it and keep going. Note: if it is
                  ;; self-referential here, there will be surprises since it'll
                  ;; point to the original alist structures.
                  l))

        ;; If the cdr if the list structure holding the kv cons cells isn't a
        ;; cons (hence an improper list), the answer is easy.
        (unless (consp r)
          (setf (cdr cloned-object) r)
          (return-from clone-object cloned-object))

        ;; But if it was a cons, then the answer is much harder since the cdr
        ;; can represent: a complete proper list, a currently proper list that
        ;; then turns into an improper list, or a cycle at some point
        ;; (including immediately).

        (loop :with end = cloned-object
              :for original-cell :on r
              :do
                 (if (u:href visited-cells original-cell)
                     ;; Base case of handling the cycle. We catch it right
                     ;; away.
                     (progn
                       (setf (cdr end)
                             (eql-map-transition eql-map original-cell))
                       (return))

                     ;; else the original-cell hasn't been encountered before
                     ;; in the list traversal, so copy it and continue (but
                     ;; beware the cdr might be an improper list!).
                     (let* ((new-cell (cons nil nil))
                            (l-original (car original-cell))
                            (r-original (cdr original-cell))
                            (list-continues-p (consp r-original)))

                       ;; visit the current original-cell we are about to
                       ;; processing.
                       (setf (u:href visited-cells original-cell) t)

                       ;; Insert the list structure cons clone into the
                       ;; eql-map.
                       (setf (eql-map-ref eql-map original-cell) new-cell)

                       ;; The car of the new-cell is intended to be a kv cons
                       ;; cell, so clone it as such. If somehow it isn't then
                       ;; just copy it over.  If the car points into the
                       ;; original list structure, you're going to have a bad
                       ;; time because you should use deep copy for that.
                       (setf (car new-cell)
                             (if (consp l-original)
                                 (clone-kv-cell l-original)
                                 l-original))

                       ;; But for the cdr, we could discover that we have an
                       ;; improper list.
                       (if list-continues-p
                           (setf (cdr new-cell) nil)
                           (setf (cdr new-cell)
                                 (eql-map-transition eql-map r-original)))
                       (setf (cdr end) new-cell
                             end new-cell))))
        cloned-object))))

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
      t))))


;;; KEEP GOING -------------------------------------------------------------

;; Do shallow-clone-tree (technically a graph due to common literal
;; elimination, so we'll have to process the entire thing as a graph). Due to
;; this we must treat both car AND cdr to be part of the tree structure.
(defmethod clone-object progn ((cloned-object cons)
                               (original-object cons)
                               (policy shallow-clone-tree)
                               eql-map
                               &key)

  )




#|
;; Deep clone.
(defmethod clone-object progn ((cloned-object cons)
                               (original-object cons)
                               (policy deep-clone)
                               &key)
  (destructuring-bind (l . r) original-object
    ;; We always deep copy the car.
    (setf (car cloned-object) (clone l policy))
    (if (consp r)
        ;; If we're in a list, manually copy the rest of it here in a deep
        ;; copying manner. Much faster than iterative CLONE calls on the cdr.
        (loop :with end = cloned-object
              :for cell :on r
              :for v = (cdr cell)
              :do (let ((new-cons (cons (clone (car cell) policy)
                                        (if (consp v)
                                            nil
                                            (clone v policy)))))
                    (setf (cdr end) new-cons
                          end new-cons)))
        ;; else, we just deep copy what we already found for the cdr!
        (setf (cdr cloned-object) (clone r policy)))
    cloned-object))

;;; -------------------------------
;; Cloning an array of any kind. The type of the array (simple or not, etc)
;; should be fully reconstructed by this method.
;;
;; TODO: Because this is somewhat generic, it may suffer from the EQUAL
;; problem a little bit if two elements have a reference to the same entity
;; during a deep copy. This is probably fixable in most of the terrible
;; cases if we carefully use EQ to keep track of stuff we've duplicated
;; from the original.
;;; -------------------------------
(defmethod clone ((object array) (policy allocating-clone) &key)
  (multiple-value-bind (displaced-to displaced-index-offset)
      (array-displacement object)
    (unless (and (null displaced-to)
                 (eql  displaced-index-offset 0))
      (error "Cloning displaced arrays is not yet supported.")))

  (let ((cloned-object
          (make-array (array-dimensions object)
                      :element-type (array-element-type object)
                      :adjustable (adjustable-array-p object))))
    (clone-object cloned-object object policy)))

;; Shallow clone
(defmethod clone-object progn ((cloned-object array) (original-object array)
                               (policy shallow-clone) &key)
  (dotimes (index (array-total-size original-object))
    (setf (row-major-aref cloned-object index)
          (row-major-aref original-object index)))
  cloned-object)

;; Deep clone
(defmethod clone-object progn ((cloned-object array) (original-object array)
                               (policy deep-clone) &key)
  (dotimes (index (array-total-size original-object))
    (setf (row-major-aref cloned-object index)
          (clone (row-major-aref original-object index) policy)))
  cloned-object)

;;; -------------------------------
;; Cloning a hash table
;;
;; TODO: Because this is somewhat generic, it may suffer from the EQUAL
;; problem a little bit if two elements have a reference to the same entity
;; during a deep copy. This is probably fixable in most of the terrible
;; cases if we carefully use EQ to keep track of stuff we've duplicated
;; from the original.
;;; -------------------------------
(defmethod clone ((object hash-table) (policy allocating-clone) &key)
  (let ((cloned-object
          (make-hash-table
           :test (hash-table-test object)
           :size (hash-table-size object)
           :rehash-size (hash-table-rehash-size object)
           :rehash-threshold (hash-table-rehash-threshold object))))
    (clone-object cloned-object object policy)))

;; Shallow clone
(defmethod clone-object progn ((cloned-object hash-table)
                               (original-object hash-table)
                               (policy shallow-clone)
                               &key)
  (u:do-hash (key value original-object)
    (setf (u:href key cloned-object)
          value))
  cloned-object)

;; Deep clone
(defmethod clone-object progn ((cloned-object hash-table)
                               (original-object hash-table)
                               (policy deep-clone)
                               &key)
  (u:do-hash (key value original-object)
    (setf (u:href (clone key policy) cloned-object)
          (clone value policy)))
  cloned-object)


;;; -------------------------------
;; Tests
;;; -------------------------------

;; Functions (expected identity clone)
(defun test-clone/function/shallow ()
  (let* (;; Type Function
         (val #'cl:identity)
         (cval (clone-shallow val)))
    (assert (eq cval val))))

(defun test-clone/function/deep ()
  (let* (;; Type Function
         (val #'cl:identity)
         (cval (clone-deep val)))
    (assert (eq cval val))))

;; CHaracters
(defun test-clone/character/shallow ()
  (let* (;; Type Character
         (val #\A)
         (cval (clone-shallow val)))
    (assert (eql cval val))))

(defun test-clone/character/deep ()
  (let* (;; Type Character
         (val #\A)
         (cval (clone-deep val)))
    (assert (eql cval val))))

;; Pathnames
;; NOTE: For now, we treat them as atoms with no structure.
(defun test-clone/pathname/shallow ()
  (let* (;; Type Pathname
         (val #P"/tmp/foo.txt")
         (cval (clone-shallow val)))
    (assert (equal cval val))))

;; TODO: Can actually be copied, implement me.
;; NOTE: For now, we treat them as atoms with no structure.
(defun test-clone/pathname/deep ()
  (let* (;; Type Pathname
         (val #P"/tmp/foo.txt")
         (cval (clone-deep val)))
    (assert (equal cval val))))

;; Sumbols
(defun test-clone/symbol/shallow ()
  (let* (;; Type Symbol
         (val 'foobar)
         (cval (clone-shallow val)))
    (assert (eq cval val))))

(defun test-clone/symbol/deep ()
  (let* (;; Type Symbol
         (val 'foobar)
         (cval (clone-deep val)))
    (assert (eq cval val))))

;; Simple Strings
(defun test-clone/simple-string/shallow ()
  (let* (;; Type SIMPLE-STRING aka (SIMPLE-ARRAY CHARACTER *)
         (val (make-sequence 'simple-string 8 :initial-element #\a))
         (cval (clone-shallow val)))

    (assert (not (eq cval val)))))

(defun test-clone/simple-string/deep ()
  (let* (;; Type SIMPLE-STRING aka (SIMPLE-ARRAY CHARACTER *)
         (val (make-sequence 'simple-string 8 :initial-element #\a))
         (cval (clone-deep val)))
    (assert (not (eq cval val)))))

;; Simple Bit Vectors
(defun test-clone/simple-bit-vector/shallow ()
  (let* (;; Type SIMPLE-BIT-VECTOR
         (val (make-sequence '(vector bit) 8 :initial-element 0))
         (cval (clone-shallow val)))
    (assert (not (eq cval val)))))

(defun test-clone/simple-bit-vector/deep ()
  (let* (;; Type SIMPLE-BIT-VECTOR
         (val (make-sequence '(vector bit) 8 :initial-element 0))
         (cval (clone-deep val)))
    (assert (not (eq cval val)))))

;; Bit Vectors
(defun test-clone/bit-vector/shallow ()
  (let* (;; Type BIT-VECTOR
         (val (make-array 8 :element-type 'bit
                            :adjustable t
                            :initial-element 0))
         (cval (clone-shallow val)))
    (assert (not (eq cval val)))))

(defun test-clone/bit-vector/deep ()
  (let* (;; Type BIT-VECTOR
         (val (make-array 8 :element-type 'bit
                            :adjustable t
                            :initial-element 0))
         (cval (clone-deep val)))
    (assert (not (eq cval val)))))

;; Simple Array, elements not shared.
(defun test-clone/simple-array-unique/shallow ()
  (let* (;; Type SIMPLE-ARRAY
         (val (make-array 3 :element-type '(unsigned-byte 8)
                            :initial-element 0))
         (cval (clone-shallow val)))
    (assert (not (eq cval val)))))

(defun test-clone/simple-array-unique/deep ()
  (let* (;; Type SIMPLE-ARRAY
         (val (make-array 3 :element-type '(unsigned-byte 8)
                            :initial-element 0))
         (cval (clone-deep val)))
    (assert (not (eq cval val)))))

;; Simple Array, elements shared.
(defun test-clone/simple-array-shared/shallow ()
  (let* ((item (cons 1 2))
         ;; Type SIMPLE-ARRAY
         (val (make-array 3 :element-type 'cons
                            :initial-element item))
         (cval (clone-shallow val)))
    (assert (and (eq (aref val 0) (aref val 1))
                 (not (eq cval val))
                 (eq (aref cval 0) (aref cval 1))))))

(defun test-clone/simple-array-shared/deep ()
  (let* ((item (cons 1 2))
         ;; Type SIMPLE-ARRAY
         (val (make-array 3 :element-type 'cons
                            :initial-element item))
         (cval (clone-deep val)))

    (assert (and (eq (aref val 0) (aref val 1))
                 (not (eq cval val))
                 (eq (aref cval 0) (aref cval 1))))))

;; KEEP GOING



(defun test-clone ()
  (test-clone/function/shallow)
  (test-clone/function/deep)

  (test-clone/character/shallow)
  (test-clone/character/deep)

  (test-clone/pathname/shallow)
  (test-clone/pathname/deep)

  (test-clone/symbol/shallow)
  (test-clone/symbol/deep)

  (test-clone/simple-string/shallow)
  (test-clone/simple-string/deep)

  (test-clone/simple-bit-vector/shallow)
  (test-clone/simple-bit-vector/deep)

  (test-clone/bit-vector/shallow)
  (test-clone/bit-vector/deep)

  (test-clone/simple-array-unique/shallow)
  (test-clone/simple-array-unique/deep)

  (test-clone/simple-array-shared/shallow)
  ;; TODO: This one fails, need to go up to EQL in structure preservation.
  ;;(test-clone/simple-array-shared/deep)

  t)


#++(let (


         ;; Type SIMPLE-VECTOR, elements not shared.
         (val-simple-vector-unique
           (make-array 8 :initial-element 0))

         ;; Type SIMPLE-VECTOR, elements shared.
         (val-simple-vector-shared
           (let ((v (cons 1 2)))
             (make-array 8 :initial-element v)))

         ;; Type VECTOR, elements unique
         (val-vector-unique
           (make-array 8 :adjustable t :initial-element nil))

         ;; Type VECTOR, elements shared
         (val-vector-shared
           (let ((v (cons 1 2)))
             (make-array 8 :adjustable t :initial-element v)))

         ;; Type ARRAY, elements not shared.
         (val-array-unique
           (make-array '(8 8) :adjustable t :initial-element 0))

         ;; Type ARRAY, elements shared.
         (val-array-unique
           (let ((v (cons 1 2)))
             (make-array '(8 8) :adjustable t :initial-element v)))

         ;; Type SIMPLE-BASE-STRING
         (val-simple-base-string
           (make-array 8 :element-type 'base-char
                         :initial-element #\a))

         ;; Type BASE-STRING
         (val-base-string
           (make-array 8 :element-type 'base-char :adjustable t
                         :initial-element #\a))

         )

     )
|#
