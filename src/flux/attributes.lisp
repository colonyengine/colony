(in-package #:virality.attribute-bag)

(defgeneric overlay (policy attribute-bag &rest containers))

;;; This will be the new home of the attributes type so we can manage DSL
;;; attributes in a nicer way. Semantic-attributes are usually written by a
;;; human and read from a DSL.

(defclass attribute-value ()
  (;; The original value of this attribute.
   (%semantic :accessor semantic
              :initarg :semantic)
   ;; A (once or more) post processed evaluation of the semantic value.
   (%computed :accessor computed
              :initarg :computed)))

(defclass attribute-bag ()
  (;; Key is an 'attr-name' which is a form comparable with EQUAL.
   ;; Value is an attribute-value instance.
   (%attributes :reader attributes
                :initarg :attributes
                :initform (u:dict #'equal))))

;;; ------------------------------------------------------------------------
;; Attribute Bag Construction/Merging/Overlay API
;;; ------------------------------------------------------------------------

(defun dump-attribute-bag (bag)
  (format t "Attribute bag:~%")
  (maphash (lambda (name av)
             (format t " name: ~S~%" name)
             (format t "  attr semantic value is ~S, computed value is ~S~%"
                     (if (slot-boundp av '%semantic)
                         (semantic av)
                         "<UNBOUND>")
                     (if (slot-boundp av '%computed)
                         (computed av)
                         "<UNBOUND>")))
           (attributes bag)))

(defmethod overlay (policy (bag attribute-bag) &rest containers)
  "POLICY can be :once-only or :supersede. If :once-only it means that the
FIRST time a name is found in the ARGS list, it is placed into the newly
created attr-bag (even if it already exists in the attr-bag), but if it is
encountered again, it is ignored. If :supersede, it means the name/value is
ALWAYS written into the bag no matter how many times it is encountered in
ARGS. Except for attribute-bags in the merging, all values are assumed to be
semantic values only.

ARGS can be a list of one or more:
:plist AAA ... ZZZ of the form AAA is (:a 0 :b 2 ... :z 25)
:alist AAA ... ZZZ of the form AAA is ((:a . 0) (:b . 1) ... (:z . 25))
:alist-big AAA ... ZZZ of the form AAA is ((:a 0) (:b 1) ... (:z 25))
:hash-table AAA ... ZZZ of the form AAA is (u:dict :a 0 ... :z 25)
:attr-bag AAA ... ZZZ
OR
AAA ... ZZZ if the type of these are discoverable without specification,
then they are accepted whole sale.
and they always apply in left to right order.

When attribute bags are overlayed, new attribute-values are always
constructed, but the values in the attribute values are shallow copied.

Returns BAG after the overlay procedure is complete.
"
  (let ((supported-types '(:plist :alist :alist-big :hash-table :attr-bag))
        (once-only (u:dict (hash-table-test (attributes bag))))
        (current-type nil))

    (labels (;; Given the current policy should I insert this name?
             (insert-p (name)
               (ecase policy
                 (:once-only
                     (if (nth-value 1 (u:href once-only name))
                         NIL
                         (setf (u:href once-only name) T)))
                 (:supersede T)))

             (absorb-plist (container)
               ;; NOTE: In a not even arg list, the last sem value will be NIL.
               (loop :for (name semval) :on container by #'cddr
                     :when (insert-p name)
                       :do (setf (sattr bag name) semval)))

             (absorb-alist (container)
               (loop :for (name . semval) :in container
                     :when (insert-p name)
                       :do (setf (sattr bag name) semval)))

             (absorb-alist-big (container)
               (loop :for (name semval) :in container
                     :when (insert-p name)
                       :do (setf (sattr bag name) semval)))

             (absorb-hash-table (container)
               (maphash (lambda (name semval)
                          (when (insert-p name)
                            (setf (sattr bag name) semval)))
                        container))

             ;; TODO: Maybe do a little better here wrt allowing the user to
             ;; specify a copier function for copy-attribute-value.
             (absorb-attr-bag (container)
               (do-attr container (lambda (name av)
                                    (when (insert-p name)
                                      (setf (attr bag name)
                                            (copy-attribute-value av))))))

             ;; Figure out what to do given what the ag actually is when it is
             ;; decidable.
             (attempt-absorb-container (container)
               (cond
                 ((hash-table-p container)
                  (setf current-type :hash-table)
                  (absorb-hash-table container))
                 ((typep container 'attribute-bag)
                  (setf current-type :attr-bag)
                  (absorb-attr-bag container))
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

      ;; Process all of the arguments.
      ;;
      ;; TODO: I bet I could accept :supersede and :once-only in here too and
      ;; change the policy dynamically while we're at it. I'll leave this
      ;; comment here until I discover I need it and am reading this again.
      ;; But one would change the :do to a cond and chec a supported-policies
      ;; that doesn't exist yet, etc, etc. There may be question of forgetting
      ;; the past or not in the once-only hash table, etc, etc.
      (loop :for container :in containers
            :do (if (member container supported-types)
                    (setf current-type container)
                    (attempt-absorb-container container)))

      bag)))

;; Make a base level attribute-bag. Since attribute-bags are mostly derived,
;; this is not a commonly used function. Uses the overlay argument syntax.
(defun make-attribute-bag (policy &rest args)
  (apply #'overlay policy (make-instance 'attribute-bag) args))

;;; ------------------------------------------------------------------------
;; Attribute Value API
;; An API to get and set attribute-values instances in the attribute-bag.
;;; ------------------------------------------------------------------------

;; Construct an attribute-value.
(defun make-attribute-value (&rest init-args)
  (apply #'make-instance 'attribute-value init-args))

;; Copy an attribute value by allocating a new attribute-value. Use the
;; COPIER-FUNC on the semantic and computed values when copying them into the
;; new attribute-value.
(defun copy-attribute-value (original-attr-value &key (copier-func #'identity))
  (let ((new-attr-value (make-attribute-value)))
    (when (slot-boundp original-attr-value '%semantic)
      (setf (semantic new-attr-value)
            (funcall copier-func (semantic original-attr-value))))
    (when (slot-boundp original-attr-value '%computed)
      (setf (computed new-attr-value)
            (funcall copier-func (computed original-attr-value))))
    new-attr-value))


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
;; the name and the attr-value.
(defmethod do-attr ((attr-bag attribute-bag) func
                    &key copy-table (copier-func #'identity))
  (let* ((attr-table (attributes attr-bag))
         (attr-table (if copy-table
                         (u:copy-hash-table attr-table :key copier-func)
                         attr-table)))
    (u:do-hash (name attr-value attr-table)
      (funcall func name attr-value))))

;;; ------------------------------------------------------------------------
;; Semantic Attribute API
;;; ------------------------------------------------------------------------

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
        (setf (semantic attr-value) new-semantic-value)
        (let ((new-attr-value
                (make-attribute-value :semantic new-semantic-value)))
          (setf (attr attr-bag name) new-attr-value)
          new-semantic-value))))

(defmethod do-sattr ((attr-bag attribute-bag) func
                     &key copy-table (copier-func #'identity))
  (let* ((attr-table (attributes attr-bag))
         (attr-table (if copy-table
                         (u:copy-hash-table attr-table :key copier-func)
                         attr-table)))
    (u:do-hash (name attr-value attr-table)
      (funcall func name (semantic attr-value)))))

;;; ------------------------------------------------------------------------
;; Computed Attribute API
;;; ------------------------------------------------------------------------

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
        (setf (computed attr-value) new-computed-value)
        (error "(setf cattr): There is no attribute-value for name: ~S"
               name))))

(defmethod do-cattr ((attr-bag attribute-bag) func
                     &key copy-table (copier-func #'identity))
  (let* ((attr-table (attributes attr-bag))
         (attr-table (if copy-table
                         (u:copy-hash-table attr-table :key copier-func)
                         attr-table)))
    (u:do-hash (name attr-value attr-table)
      (funcall func name (computed attr-value)))))
