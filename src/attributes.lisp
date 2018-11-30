(in-package #:%fl)

;; This will be the new home of the attributes type so we can manage DSL
;; attributes in a nicer way.

;; Semantic-attributes are usually written by a human and read from a DSL.
(defclass semantic-attributes ()
  ((%semantic-attributes :reader semantic-attributes
                         :initarg :semantic-attributes
                         :initform (au:dict #'eql))))

(defun make-semantic-attributes (&rest initargs)
  (apply #'make-instance 'semantic-attributes initargs))

(defmethod semantic-attribute ((s semantic-attributes) (attribute-name symbol))
  (au:href (semantic-attributes s) attribute-name))

(defmethod (setf semantic-attribute) (newobj
                                      (s semantic-attributes)
                                      (attribute-name symbol))
  (setf (au:href (semantic-attributes s) attribute-name) newobj))

(defmethod clear-semantic-attributes ((s semantic-attributes))
  (clrhash (semantic-attributes s)))

(defmethod do-semantic-attributes ((s semantic-attributes) func)
  ;; First make a copy-ish of the hash table in case the func wants
  ;; to modify the hash table.
  ;; TODO: A little slow and memory using, but also safe.
  (let ((attrs (au:hash->alist (semantic-attributes s))))
    (dolist (attr attrs)
      (destructuring-bind (key value) attr
        (funcall func key value)))))





;; Computed-attributed are derived from semantic-attributes and contain changes
;; to the semantics attribute values as needed.
(defclass computed-attributes (semantic-attributes)
  ((%computed-attributes :reader computed-attributes
                         :initarg :computed-attributes
                         :initform (au:dict #'eql))))

(defmethod computed-attribute ((s computed-attributes) (attribute-name symbol))
  (au:href (computed-attributes s) attribute-name))

(defmethod (setf computed-attribute) (newobj
                                      (s computed-attributes)
                                      (attribute-name symbol))
  (setf (au:href (computed-attributes s) attribute-name) newobj))

(defmethod clear-computed-attributes ((s computed-attributes))
  (clrhash (computed-attributes s)))

(defmethod do-computed-attributes ((s computed-attributes) func)
  ;; First make a copy-ish of the hash table in case the func wants
  ;; to modify the hash table.
  ;; TODO: A little slow and memory using, but also safe.
  (let ((attrs (au:hash->alist (computed-attributes s))))
    (dolist (attr attrs)
      (destructuring-bind (key value) attr
        (funcall func key value)))))

(defun make-computed-attributes (&rest initargs)
  (apply #'make-instance 'computed-attributes initargs))






(defmethod compute-attributes ((c computed-attributes) copier-func)
  ;; Convert the semantic attributes to computed attributes using the
  ;; copier.

  nil)

(defmethod merge-computed-attributes ((c computed-attributes) &rest more-attrs)
  nil)
