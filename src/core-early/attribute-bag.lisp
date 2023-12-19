(in-package #:virality.attribute-bag)

;;; ------------------------------------------------------------------------
;; Attribute Value API
;; An API to get and set attribute-values instances in the attribute-bag.
;;; ------------------------------------------------------------------------

;; Construct an attribute-value.
(defun make-attribute-value (&rest init-args)
  (apply #'make-instance 'attribute-value init-args))

(defun semantic-value-bound-p (attr-value)
  (slot-boundp attr-value '%semantic))

(defun computed-value-bound-p (attr-value)
  (slot-boundp attr-value '%computed))

;; Copy an attribute value by allocating a new attribute-value. Use the
;; COPIER-FUNC on the semantic and computed values when copying them into the
;; new attribute-value.
(defun copy-attribute-value (original-attr-value &key (copier-func #'identity))
  (let ((new-attr-value (make-attribute-value)))
    (when (semantic-value-bound-p original-attr-value)
      (setf (semantic new-attr-value)
            (funcall copier-func (semantic original-attr-value))))
    (when (computed-value-bound-p original-attr-value)
      (setf (computed new-attr-value)
            (funcall copier-func (computed original-attr-value))))
    new-attr-value))

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
               (do-attr container (lambda (name av)
                                    (when (allow-insert-p name)
                                      (setf (attr bag name)
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

;; Make a base level attribute-bag. Since attribute-bags are mostly derived,
;; this is not a commonly used function. Uses the overlay argument syntax.
(defun make-attribute-bag (&rest args)
  (apply #'overlay (make-instance 'attribute-bag) args))

;;; ----
;; Contained Attribute Value API
;;; ----

;; Return two values.
;; First Value: the attribute-value instance if it exists or NIL.
;; Second value: T if the attribute-value instance exists or NIL.
(defmethod attr ((attr-bag attribute-bag) name)
  (u:href (attributes attr-bag) name))

;; Associate an attribute-value with a name. Forget about the old one if it
;; existed.
(defmethod (setf attr) (new-attr-value (attr-bag attribute-bag) name)
  (setf (u:href (attributes attr-bag) name) new-attr-value))

;; Wipe out the entire attribute hash table. Very destructive.
(defmethod clear-attrs ((attr-bag attribute-bag))
  (clrhash (attributes attr-bag)))

;; Copy the table only copies the hash table for the purposes of the
;; iteration. The :copier-func function, which defaults to cl:identity,
;; processes the attr-value before storage into the copied hash table. Through
;; this, one could deep copy the attribute-values if desired. FUNC is passed
;; the name and the attr-value. Return attr-bag.
(defmethod do-attr ((attr-bag attribute-bag) func
                    &key copy-table (copier-func #'identity))
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

;; Return two values. The first value is the semantic attribute value for the
;; given name or NIL if there was no attribute with this name. The second value
;; is the attribute-value instance if the attribute-value exists or NIL if it
;; didn't.
(defmethod sattr ((attr-bag attribute-bag) name)
  (u:mvlet ((attr-value presentp (attr attr-bag name)))
    (if presentp
        (values (semantic attr-value) attr-value)
        (values NIL NIL))))

;; If the attribute-value exists, update the semantic value (and just forget
;; about the previous one if it exists). Otherwise, create and insert an
;; attribute-value and then update the semantic value. Returns the
;; new-semantic-value.
(defmethod (setf sattr) (new-semantic-value (attr-bag attribute-bag) name)
  (u:mvlet ((attr-value presentp (attr attr-bag name)))
    (if presentp
        (setf (semantic attr-value) new-semantic-value
              (dirty attr-value) T)
        (let ((new-attr-value
                (make-attribute-value :semantic new-semantic-value
                                      :dirty T)))
          (setf (attr attr-bag name) new-attr-value)
          new-semantic-value))))

(defmethod do-sattr ((attr-bag attribute-bag) func
                     &key copy-table (copier-func #'identity))
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

;; Return two values. The first value is the computed attribute value for the
;; given name or NIL if there was no attribute with this name. The second value
;; is the attribute-value instance if the attribute-value exists or NIL if it
;; didn't.
(defmethod cattr ((attr-bag attribute-bag) name)
  (u:mvlet ((attr-value presentp (attr attr-bag name)))
    (if presentp
        (values (computed attr-value) attr-value)
        (values NIL NIL))))

;; If the attribute-value exists, update the computed value (and just forget
;; about the previous one if it exists). Otherwise, it is an error to try and
;; set a computed attribute value for an attribute that doesn't exist.  Return
;; the new-computed-value.
(defmethod (setf cattr) (new-computed-value (attr-bag attribute-bag) name)
  (u:mvlet ((attr-value presentp (attr attr-bag name)))
    (if presentp
        (setf (computed attr-value) new-computed-value
              (dirty attr-value) NIL)
        (error "(setf cattr): There is no attribute-value for name: ~S"
               name))))

(defmethod do-cattr ((attr-bag attribute-bag) func
                     &key copy-table (copier-func #'identity))
  (let* ((attr-table (attributes attr-bag))
         (attr-table (if copy-table
                         (u:copy-hash-table attr-table :key copier-func)
                         attr-table)))
    (u:do-hash (name attr-value attr-table)
      (funcall func name (computed attr-value))))
  attr-bag)
