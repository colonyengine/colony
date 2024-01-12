(in-package #:virality.attribute-bag)

;;; ------------------------------------------------------------------------
;; Attribute Value API
;; An API to get and set attribute-values instances in the attribute-bag.
;;; ------------------------------------------------------------------------

;; Construct an attribute-value.
(defun make-attribute-value (&rest init-args)
  (apply #'make-instance 'attribute-value init-args))

(defun semantic-value-bound-p (attr-value)
  "Return true if the semantic value in the ATTR-VALUE is bound.
NIL otherwise."
  (slot-boundp attr-value '%semantic))

(defun computed-value-bound-p (attr-value)
  "Return true if the computed value in the ATTR-VALUE is bound.
NIL otherwise."
  (slot-boundp attr-value '%computed))

#|
;; TODO: Replace calls with below CLONE code.
;; OK CLONE seems to work.
(defun copy-attribute-value (original-attr-value
                             &key (semantic-copier-func #'identity)
                               (computed-copier-func #'identity))
  "Copy ORIGINAL-ATTR-VALUE by allocating a new attribute-value. Use the
SEMANTIC-COPIER-FUNC on the semantic value and the COMPUTED-COPIER-FUNC on the
computed values when copying them into the new attribute-value. Both of them
default to CL:IDENTITY. Return the copied attribute-value."
  (let ((new-attr-value (make-attribute-value)))
    (when (semantic-value-bound-p original-attr-value)
      (setf (semantic new-attr-value)
            (funcall semantic-copier-func (semantic original-attr-value))))

    (setf (dirty new-attr-value) (dirty original-attr-value))

    (when (computed-value-bound-p original-attr-value)
      (setf (computed new-attr-value)
            (funcall computed-copier-func (computed original-attr-value))))
    new-attr-value))
|#

(defmethod clone:clone ((object attribute-value)
                        (policy clone:allocating-clone)
                        &key (semantic-copier-func #'identity)
                          (computed-copier-func #'identity))
  "Copy ORIGINAL-ATTR-VALUE by allocating a new attribute-value. Use the
SEMANTIC-COPIER-FUNC on the semantic value and the COMPUTED-COPIER-FUNC on the
computed values when copying them into the new attribute-value. Both of them
default to CL:IDENTITY. Return the copied attribute-value."
  (let ((cloned-object (make-attribute-value)))
    (clone:clone-object cloned-object object policy
                        :semantic-copier-func semantic-copier-func
                        :computed-copier-func computed-copier-func)))

;; We only implement deep copy
(defmethod clone:clone-object progn ((cloned-object attribute-value)
                                     (original-object attribute-value)
                                     (policy clone:deep-clone)
                                     &key semantic-copier-func
                                       computed-copier-func)
  (when (semantic-value-bound-p original-object)
    (setf (semantic cloned-object)
          (clone:clone
           (funcall semantic-copier-func (semantic original-object))
           policy)))
  (setf (dirty cloned-object)
        (clone:clone (dirty original-object) policy))
  (when (computed-value-bound-p original-object)
    (setf (computed cloned-object)
          (clone:clone
           (funcall computed-copier-func (computed original-object))
           policy)))
  cloned-object)


;;; ------------------------------------------------------------------------
;; Attribute Bag API
;;; ------------------------------------------------------------------------

(defun dump-attribute-bag (bag)
  (format t "Attribute bag~%")
  (do-attr bag
    (lambda (name av)
      ;; d is not dirty, D is dirty.
      (format t " ~S -> ~:[d~;D~] ~S "
              name
              (dirty av)
              (if (semantic-value-bound-p av)
                  (semantic av)
                  "<UNBOUND>"))
      (if (computed-value-bound-p av)
          (format t "=> ~S~%" (computed av))
          (format t "=|~%")))))

(defmethod overlay ((bag attribute-bag) &rest containers)
  "An item in CONTAINERS can be:
One or more of these overlay policy keywords:
:supersede - Change the overlay policy to :supersede for container items
             processed after this item. Supersede means anytime a name is
             found with an associated value, accept the new value and store
             it as the semantic value in the BAG for the current name.
             This is the default overlay policy if no overlay policy is
             specified.
:once-only - Change the overlay policy to :once-only for container items
             processed after this iterm. Once-only means the FIRST time
             a name has ever been seen to the right of this item, it is stored
             in the BAG, but subsequenct times the name is observed ognore it.
             This memory is preserved across policy changes with :supersede.
:once-only-clear - Change the overlay policy to :once-only and also clear the
                   cache of name that have already been seen before processing
                   the items to the right of this item.

Additionally, CONTAINERS can have multiple sequences of items in it that
specify associations of names to semantic values. These association items are
processed left to right and, according to the policy, will overlay their name
and value into the BAG. Sequences of these items may be denoted by a keyword
symbol indicating its type and then one or more instances of that type. For
example:

:plist AAA ... ZZZ - One or more property lists of the form:
                     (:a 0 :b 2 ... :z 25)
:alist AAA ... ZZZ - One or more association lists of the form:
                     ((:a . 0) (:b . 1) ... (:z . 25))
:alist-big AAA ... ZZZ - One or more association lists of the form:
                         ((:a 0) (:b 1) ... (:z 25))
:hash-table AAA ... ZZZ - One of more hash tables of the form:
                          (u:dict :a 0 ... :z 25) [also regular hash-tables]
:attr-bag AAA ... ZZZ - One or more attribute bag instances.

NOTE: :plist, :alist, :alist-big instance(s) MUST be specified with the keyword
symbol since they are not automatically inferrable. When the container item is
a :hash-table or :attr-bag, it does not have to be qualified with one of the
symbols above and will not disturb the current type if one is found while
processing a :plist, :alist, or :alist-big.

An example is:

 (overlay bag (u:dict :a 10) :plist '(:b 2 :c 3) (u:dict :d 4) '(:e 5 :f 6))

Notice how the hash table is jammed in the middle but doesn't disturb the
fact that :plist is the current-type being parsed.

When attribute bags are overlayed, new ATTRIBUTE-VALUES are always
constructed, but the values in the attribute values are shallow copied.

Computed values in the attribute-value instances are never updated when the
semantic value is updated as a result of this method.

Returns BAG after the overlay procedure is complete.
"
  (let ((supported-types '(:plist :alist :alist-big :hash-table :attr-bag))
        (supported-policies '(:supersede :once-only :once-only-clear))
        (once-only (u:dict (hash-table-test (attributes bag))))
        (current-policy :supersede)
        (current-type nil))

    (labels (;; Given the current policy should I insert this name?
             (allow-insert-p (name)
               (ecase current-policy
                 ;; NOTE: :once-only-clear is handled specially when it is
                 ;; first observed.
                 (:once-only
                     (if (nth-value 1 (u:href once-only name))
                         NIL
                         (setf (u:href once-only name) T)))
                 (:supersede T)))

             (absorb-plist (container)
               ;; NOTE: In a not even arg list, the last sem value will be NIL.
               (loop :for (name semval) :on container by #'cddr
                     :when (allow-insert-p name)
                       :do (setf (sattr bag name) semval)))

             (absorb-alist (container)
               (loop :for (name . semval) :in container
                     :when (allow-insert-p name)
                       :do (setf (sattr bag name) semval)))

             (absorb-alist-big (container)
               (loop :for (name semval) :in container
                     :when (allow-insert-p name)
                       :do (setf (sattr bag name) semval)))

             (absorb-hash-table (container)
               (maphash (lambda (name semval)
                          (when (allow-insert-p name)
                            (setf (sattr bag name) semval)))
                        container))

             ;; TODO: Maybe do a little better here wrt allowing the user to
             ;; specify a copier function for copy-attribute-value.
             (absorb-attr-bag (container)
               (do-attr container
                 (lambda (name av)
                   (when (allow-insert-p name)
                     ;; TODO: Good. This seems to work.
                     (setf (attr bag name)
                           (clone:clone-deep av))
                     ;; TODO: If deep cloning works, remove
                     ;; this ignored form and the
                     ;; COPY-ATTRIBUTE-VALUE fnction.
                     #++(setf (attr bag name)
                              (copy-attribute-value av))))))

             ;; Figure out what to do given what the container actually is when
             ;; it is decidable. This allows us to not necessarily have to
             ;; specify types of things.
             (attempt-absorb-container (container)
               (cond
                 ((hash-table-p container)
                  (let ((old-type current-type))
                    (setf current-type :hash-table)
                    (absorb-hash-table container)
                    (setf current-type old-type)))
                 ((typep container 'attribute-bag)
                  (let ((old-type current-type))
                    (setf current-type :attr-bag)
                    (absorb-attr-bag container)
                    (setf current-type old-type)))
                 ((null current-type)
                  (error "overlay: I cannot choose the right type to absorb!"))
                 (t
                  ;; Sadly there are no ways to typecheck this since names can
                  ;; be more complex forms than just a symbol. So if the caller
                  ;; changes from a pist to an alist without specifying the new
                  ;; type, this will explode somewhere.
                  (ecase current-type
                    (:plist (absorb-plist container))
                    (:alist (absorb-alist container))
                    (:alist-big (absorb-alist-big container)))))))

      ;; Process all of the arguments intentionally from left to right.
      (loop :for container :in containers
            :do (cond
                  ((member container supported-types)
                   (setf current-type container))
                  ((member container supported-policies)
                   (setf current-policy container)
                   (case current-policy
                     (:once-only-clear
                      (clrhash once-only)
                      (setf current-policy :once-only))))
                  (t
                   (attempt-absorb-container container))))

      bag)))

(defun make-attribute-bag (&rest args)
  "Allocate and return a base level attribute-bag. Since attribute-bags are
mostly derived, this is not a commonly used function. Apply the OVERLAY
function to ARGS."
  (apply #'overlay (make-instance 'attribute-bag) args))

;;; ----
;; Contained Attribute Value API
;;; ----

(defmethod attr ((attr-bag attribute-bag) name)
  "Return two values. The first value is the attribute-value instance if it
exists or NIL. The second value is T if the attribute-value existed or NIL if
it didn't."
  (u:href (attributes attr-bag) name))

(defmethod (setf attr) (new-attr-value (attr-bag attribute-bag) name)
  "Associate the NEW-ATTR-VALUE with the NAME in ATTR-BAG. Forget about the old
attribute-value if it existed. Return NEW-ATTR-VALUE."
  (setf (u:href (attributes attr-bag) name) new-attr-value))

(defmethod clear-attrs ((attr-bag attribute-bag))
  "Clear attribute hash table. All attribute-values and their semantic and
computed values are forgotten about. Very destructive."
  (clrhash (attributes attr-bag)))

(defmethod do-attr ((attr-bag attribute-bag) func
                    &key copy-table (copier-func #'identity)
                    &allow-other-keys)
  "Iterate the FUNC over each attribute-value contained in the ATTR-BAG. Return
the ATTR-BAG. If the keyword argument :COPY-TABLE is true, then copy the
internal ATTR-BAF hash table using the value of the keyword argument
:COPIER-FUNC as the copy function for the attribute-values and iterate over
that copy instead.  The :COPIER-FUNC function, which defaults to cl:identity,
processes the attr-value before storage into the copied hash table. Through
this, one could deep copy the attribute-values if desired. The copied table is
lost after this function returns. FUNC is passed the name and the attr-value."
  (let* ((attr-table (attributes attr-bag))
         (attr-table (if copy-table
                         (u:copy-hash-table attr-table :key copier-func)
                         attr-table)))
    (u:do-hash (name attr-value attr-table)
      (funcall func name attr-value)))
  attr-bag)

;;; ----
;; Semantic Attribute API
;;; ----

(defmethod sattr ((attr-bag attribute-bag) name &optional (not-found nil))
  "If there is an attribute-value associated with NAME in the ATTR-BAG, then
return two values. The first is the semantic value of the attribute-value and
the second is the attribute-value itself.  If the attribute value does not
exist, return a values with the first value being NOT-FOUND, which defaults to
NIL, and the second value being NIL."
  (u:mvlet ((attr-value presentp (attr attr-bag name)))
    (if presentp
        (values (semantic attr-value) attr-value)
        (values not-found NIL))))

(defmethod (setf sattr) (new-semantic-value (attr-bag attribute-bag) name)
  "If the attribute-value exists, update the semantic value, forget about the
previous semantic value, and dirty the attribute value. Otherwise, create and
insert an attribute-value associated with NAME into ATTR-BAG, update the
semantic value, and dirty the new attribute-value. Returns the
new-semantic-value. Return the NEW-SEMANTIC-VALUE."
  (u:mvlet ((attr-value presentp (attr attr-bag name)))
    (if presentp
        (setf (dirty attr-value) T
              (semantic attr-value) new-semantic-value)
        (let ((new-attr-value
                (make-attribute-value :semantic new-semantic-value
                                      :dirty T)))
          (setf (attr attr-bag name) new-attr-value)
          new-semantic-value))))

(defmethod do-sattr ((attr-bag attribute-bag) func
                     &key copy-table (copier-func #'identity))

  "Iterate over the attribute-values contained in the ATTR-BAG. For each
attribute-value, FUNC is passed the name and the semantic value of the
attribute-value. Return the ATTR-BAG. If the keyword argument :COPY-TABLE is
true, then copy the internal ATTR-BAF hash table using the value of the keyword
argument :COPIER-FUNC as the copy function for the attribute-values and iterate
over that copy instead.  The :COPIER-FUNC function, which defaults to
cl:identity, processes the attr-value before storage into the copied hash
table. Through this, one could deep copy the attribute-values if desired. The
copied table is lost after this function returns."
  (let* ((attr-table (attributes attr-bag))
         (attr-table (if copy-table
                         (u:copy-hash-table attr-table :key copier-func)
                         attr-table)))
    (u:do-hash (name attr-value attr-table)
      (funcall func name (semantic attr-value))))
  attr-bag)

;;; ----
;; Computed Attribute API
;;; ----

(defmethod cattr ((attr-bag attribute-bag) name &optional (not-found nil))
  "If there is an attribute-value associated with NAME in the ATTR-BAG, then
return two values. The first is the computed value of the attribute-value and
the second is the attribute-value itself.  If the attribute value does not
exist, return a values with the first value being NOT-FOUND, which defaults to
NIL, and the second value being NIL."
  (u:mvlet ((attr-value presentp (attr attr-bag name)))
    (if presentp
        (values (computed attr-value) attr-value)
        (values not-found NIL))))

(defmethod (setf cattr) (new-computed-value (attr-bag attribute-bag) name
                         &optional (default-semval NIL default-semval-supp-p))
  "If the attribute-value associated with NAME in the ATTR-BAG exists, update
the computed value and make the attribute not dirty. Any previous computed
value reference is simply lost. If the attribute-value does not exist, it is an
error to try and set a computed attribute value for an attribute that doesn't
exist--unless the caller specifies the optional DEFAULT-SEMVAL argument which
defines what the semval should be for the new attribute.  If the attribute does
exist, ignore the DEFAULT-SEMVAL and just update the dirty flag and computed
value. Return the NEW-COMPUTED-VALUE."
  (u:mvlet ((attr-value presentp (attr attr-bag name)))
    (if presentp
        (setf (dirty attr-value) NIL
              (computed attr-value) new-computed-value)
        (if default-semval-supp-p
            (progn
              (setf (attr attr-bag name)
                    (make-attribute-value :semantic default-semval
                                          :dirty nil
                                          :computed new-computed-value))
              new-computed-value)
            (error "(setf cattr): There is no attribute-value for name: ~S"
                   name)))))

(defmethod do-cattr ((attr-bag attribute-bag) func
                     &key copy-table (copier-func #'identity)
                     &allow-other-keys)
  "Iterate over the attribute-values contained in the ATTR-BAG. For each
attribute-value, FUNC is passed the name and the computed value of the
attribute-value. Return the ATTR-BAG. If the keyword argument :COPY-TABLE is
true, then copy the internal ATTR-BAF hash table using the value of the keyword
argument :COPIER-FUNC as the copy function for the attribute-values and iterate
over that copy instead.  The :COPIER-FUNC function, which defaults to
cl:identity, processes the attr-value before storage into the copied hash
table. Through this, one could deep copy the attribute-values if desired. The
copied table is lost after this function returns."
  (let* ((attr-table (attributes attr-bag))
         (attr-table (if copy-table
                         (u:copy-hash-table attr-table :key copier-func)
                         attr-table)))
    (u:do-hash (name attr-value attr-table)
      (funcall func name (computed attr-value))))
  attr-bag)
