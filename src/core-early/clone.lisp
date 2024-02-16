(in-package #:virality.clone)

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

;;; -------------------------------
;; The EQL-MAP API
;;; -------------------------------

(defun make-eql-map-entry (&rest args)
  (apply #'make-instance 'eql-map-entry args))

(defun make-eql-map ()
  (make-instance 'eql-map))

(defun eql-map-initialize (eql-map)
  (clrhash (entry-table eql-map)))

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

#++(defun eql-map-transition-p (eql-map original-object)
     "Return two values. The first value is T if there is a transition TARGET and
NIL if there is none. The second value is T if the ORIGINAL-OBJECT had already
been visited and NIL otherwise."
     (u:if-let ((eql-map-entry (eql-map-ref eql-map original-object)))
       (values (transition-p eql-map-entry) t)
       (values nil nil)))

#++(defun eql-map-target (eql-map original-object)
     "Return two values. The first value is the TARGET of the transition or NIL if
there is no transition. The second value is T if there was a transition and NIL
otherwise.  Beware that (values NIL T) means there is a transition who target
is the value NIL."
     (let ((eql-map-entry (eql-map-ref eql-map original-object)))
       (u:if-let ((transition-p eql-map-entry))
         (values (target eql-map-entry) t)
         (values nil nil))))

(defun eql-map-mark-target (eql-map original-object cloned-object intent)
  "Visit the ORIGINAL-OBJECT (if not already visited), the mark the transition
as present and store the CLONED-OBJECT as the TARGET and store the INTENT into
the EQL-MAP-ENTRY.  Return the CLONED-OBJECT."
  (let ((eql-map-entry (eql-map-mark-visited eql-map original-object)))
    (setf (transition-p eql-map-entry) t
          (target eql-map-entry) cloned-object
          (intent eql-map-entry) intent)
    cloned-object))



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
       (format strm "~%  v:")
       (print-unreadable-object (eql-map-entry strm :type t :identity t)
         (format strm "~%    o:  ~S~%    tp: ~S~%    t:  ~S~%    i:  ~S"
                 (safe-slot-value eql-map-entry '%origin #'origin)
                 (safe-slot-value eql-map-entry '%transition-p #'transition-p)
                 (safe-slot-value eql-map-entry '%target #'target)
                 (safe-slot-value eql-map-entry '%intent #'intent)))
       (format strm "~%"))
     (entry-table eql-map))))

;;; The CLONE API and CLONE-OBJECT API methods.  CLONE is the entry point to
;;; clone an object and must allocate the memory for the new object, if
;;; appropriate, it wll then call CLONE-OBJECT to complete the cloning process.
;;
;;; NOTE: In most places where it is intended to so an identity clone, I've not
;;; actually written the CLONE call and instead just used the raw value.  This
;;; is identical behavior and faster because CLONE isn't being resolved and
;;; used like cl:identity. However, we include it for when you DO need it.

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


;;; -------------------------------
;; Cloning low level atomic non collection-like things.
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
(defmethod clone (object (policy identity-clone) intention eql-map &key)
  (let ((eql-map-entry (eql-map-mark-visited eql-map object)))
    (if (transition-p eql-map-entry)
        (values (target eql-map-entry) eql-map)
        (values (eql-map-mark-target eql-map object object intention)
                eql-map))))

;;; -------------------------------
;; Cloning a cons cell
;;; -------------------------------
(defmethod clone ((object cons) (policy allocating-clone) intention eql-map
                  &key)
  ;; Note: we may have already visited it once in some other control path.
  (let ((eql-map-entry (eql-map-mark-visited eql-map object)))
    (if (transition-p eql-map-entry)
        ;; We already have a transition...
        (if (intention= intention (intent eql-map-entry))
            ;; then good to go, we can just reuse what we have!
            (values (target eql-map-entry) eql-map)
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
        (let ((cloned-object (cons nil nil)))
          (eql-map-mark-target eql-map object cloned-object intention)
          (values (clone-object cloned-object object policy intention
                                (make-no-specific-intention) eql-map)
                  eql-map)))))

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
    (setf (car cloned-object) l)

    ;; If the cdr isn't a cons (hence an improper list), the answer is easy.
    (unless (consp r)
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
                        (progn (setf (cdr end) (target eql-map-entry))
                               (return))
                        (error "Unsupported list intention pair: ~A and ~A"
                               intention (intent eql-map-entry)))

                    ;; If not a transition, then original-cell hasn't been
                    ;; cloned (or transitioned) before in the list traversal,
                    ;; so copy it and continue (but beware the cdr might be an
                    ;; improper list!).
                    (let* ((new-cell (cons nil nil))
                           (l-original (car original-cell))
                           (r-original (cdr original-cell))
                           (list-continues-p (consp r-original)))

                      ;; The car of the new-cell is an easy fixup since we're
                      ;; doing a shallow copy. If the car points into the
                      ;; original list structure, you're going to have a bad
                      ;; time because you should use deep copy for that.
                      (setf (car new-cell) l-original)

                      ;; For the cdr, we could discover that we have an
                      ;; improper list.
                      (unless list-continues-p
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
               ;; Otherwise, just shallow clone the non-cons cell.
               maybe-kv-cell)))

    (destructuring-bind (l . r) original-object
      ;; We always shallow copy the car no matter what it was.
      ;; NOTE: if the car points back into the list structure...too bad use
      ;; deep copy for such a thing.
      (setf (car cloned-object)
            (clone-maybe-kv-cell l))

      ;; If the cdr isn't a cons (hence an improper list), the answer is easy.
      (unless (consp r)
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
                          (progn (setf (cdr end) (target eql-map-entry))
                                 (return))
                          (error "Unsupported list intention pair: ~A and ~A"
                                 intention (intent eql-map-entry)))

                      ;; If not a transition, then original-cell hasn't been
                      ;; cloned (or transitioned) before in the list traversal,
                      ;; so copy it and continue (but beware the cdr might be
                      ;; an improper list!).
                      (let* ((new-cell (cons nil nil))
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



;;; KEEP GOING -------------------------------------------------------------
;;; This is almost done, just writing unit tests.

;; Do shallow-clone-tree (technically a graph due to common literal coalescing,
;; so we'll have to process the entire thing as a graph). Due to this we must
;; treat both car AND cdr, when they are to a cons, to be part of the tree
;; structure  and hence reference the eql-map and cloned.
(defmethod clone-object progn ((cloned-object cons)
                               (original-object cons)
                               (policy shallow-clone)
                               (intention graph-intention)
                               (last-known-intention no-specific-intention)
                               eql-map
                               &key)

  ;; This function not only explores the child nodes, but also clones the grpah
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
                     (let ((new-child (cons nil nil)))
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
                     (funcall setter
                              (target parent-eql-map-entry)
                              child))

                 ;; NOTE: This case catches certain situations like a graph
                 ;; consisting of a single cons cell pointing to itself, or
                 ;; when we're attempting to clone the graph and there are
                 ;; multiple roots into it. This ensure in those cases that the
                 ;; graph edge in the cloned graph is present.
                 ;;
                 ;; If the child had been explored already, then update cloned
                 ;; parents's car/cdr to the cloned target to preserve the
                 ;; graph structure of the clone and we're done with the
                 ;; car/cdr edge. The BFS graph is keeping track of NODES that
                 ;; have been explored, not edges! So we ensure it is
                 ;; preserved.
                 (funcall setter
                          (target parent-eql-map-entry)
                          (target child-eql-map-entry))))))

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
                  ;;(format t "Processing: ~S~%" n)
                  ;; Process CAR
                  ;;(format t " car: ~S~%" nl)
                  (explore-child-node n nl #'rplaca q)

                  ;; Process CDR
                  ;;(format t " cdr: ~S~%" nr)
                  (explore-child-node n nr #'rplacd q)))))

  ;; Then we return the root to the newly cloned graph.
  cloned-object)



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
