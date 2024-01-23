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

(defparameter *identity* (make-identity-clone))
(defparameter *shallow* (make-shallow-clone))
(defparameter *shallow-cons* (make-shallow-clone-cons))
(defparameter *shallow-list* (make-shallow-clone-list))
(defparameter *shallow-alist* (make-shallow-clone-alist))
(defparameter *shallow-tree* (make-shallow-clone-tree))
(defparameter *deep* (make-deep-clone))

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
  (clone object *shallow-list* (if eql-map-supp-p eql-map (make-eql-map))))

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
  "If the ORIGINAL-OBJECT does not have a mapping in EQL-MAP, then return
ORIGINAL-OBJECT. If it does have a transition, return the object to which it
transitioned."
  (u:mvlet ((object-trfm object-trfm-present-p
                         (eql-map-ref eql-map original-object)))
    (if object-trfm-present-p object-trfm original-object)))

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
  object)

;; If it is a clone of an unknown type, return the transition if there is one.
(defmethod clone (object (policy allocating-clone) eql-map &key)
  (eql-map-transition eql-map object))

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
    (clone-object cloned-object object policy eql-map)))

;; shallow-clone-cons shallow clones the SINGLE cons cell given to it with
;; no recursion what-so-ever.
(defmethod clone-object progn ((cloned-object cons)
                               (original-object cons)
                               (policy shallow-clone-cons)
                               eql-map
                               &key)

  (destructuring-bind (l . r) original-object
    (setf (car cloned-object) (eql-map-transition eql-map l)
          (cdr cloned-object) (eql-map-transition eql-map r)))
  cloned-object)

;; shallow-clone-list clones the toplevel list structure ONLY.
(defmethod clone-object progn ((cloned-object cons)
                               (original-object cons)
                               (policy shallow-clone-list)
                               eql-map
                               &key)
  (destructuring-bind (l . r) original-object
    ;; We always shallow copy the car no matter what it was.
    (setf (car cloned-object) (eql-map-transition eql-map l))

    (if (consp r)
        ;; If we're in a list (or list-like thing, maybe it is an improper
        ;; list), manually copy the rest of it here in a shallow copying
        ;; manner. This is much faster than recursive CLONE calls on the cdr
        ;; and won't blow the stack.
        (loop :with end = cloned-object
              :for original-cell :on r
              :do (let* ((new-cell (cons nil nil))
                         (l-original (car original-cell))
                         (r-original (cdr original-cell))
                         (list-continues-p (consp r-original)))
                    ;; Insert the cons clone into the eql-map.
                    (setf (eql-map-ref eql-map original-cell) new-cell)
                    ;; Set the values of the new cons cell, which might use
                    ;; the transition we just inserted.
                    (setf
                     (car new-cell) (eql-map-transition eql-map l-original)

                     (cdr new-cell) (if list-continues-p
                                        nil
                                        (eql-map-transition eql-map
                                                            r-original)))
                    ;; Extend the end of the shallow copied list and get loop
                    ;; ready for the next cons cell clone.
                    (setf (cdr end) new-cell
                          end new-cell)))

        ;; else, we just keep what we already found for the cdr!
        (setf (cdr cloned-object) (eql-map-transition eql-map r)))
    cloned-object))

;;; KEEP GOING -------------------------------------------------------------

;; Do shallow-clone-alist
;; Do shallow-clone-tree




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
