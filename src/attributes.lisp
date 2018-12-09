(in-package #:%fl)

;; This will be the new home of the attributes type so we can manage DSL
;; attributes in a nicer way.

;; Semantic-attributes are usually written by a human and read from a DSL.
(defclass attributes ()
  ((%semantic-attributes :reader semantic-attributes
                         :initarg :semantic-attributes
                         :initform (fu:dict #'eql))
   (%computed-attributes :reader computed-attributes
                         :initarg :computed-attributes
                         :initform (fu:dict #'eql))))


(defun make-attributes (&rest initargs)
  (apply #'make-instance 'attributes initargs))

;; Semantic attributes API
(defmethod semantic-attribute ((a attributes) (attribute-name symbol))
  (fu:href (semantic-attributes a) attribute-name))

(defmethod (setf semantic-attribute) (newobj
                                      (a attributes)
                                      (attribute-name symbol))
  (setf (fu:href (semantic-attributes a) attribute-name) newobj))

(defmethod clear-semantic-attributes ((a attributes))
  (clrhash (semantic-attributes a)))

(defmethod do-semantic-attributes ((a attributes) func)
  ;; First make a copy-ish of the hash table in case the func wants
  ;; to modify the hash table.
  ;; TODO: A little slow and memory using, but also safe.
  (let ((attrs (fu:hash->alist (semantic-attributes a))))
    (dolist (attr attrs)
      (destructuring-bind (key value) attr
        (funcall func key value)))))

;; Computed attributes API

(defmethod computed-attribute ((a attributes) (attribute-name symbol))
  (fu:href (computed-attributes a) attribute-name))

(defmethod (setf computed-attribute) (newobj
                                      (a attributes)
                                      (attribute-name symbol))
  (setf (fu:href (computed-attributes a) attribute-name) newobj))

(defmethod clear-computed-attributes ((a attributes))
  (clrhash (computed-attributes a)))

(defmethod do-computed-attributes ((a attributes) func)
  ;; First make a copy-ish of the hash table in case the func wants
  ;; to modify the hash table.
  ;; TODO: A little slow and memory using, but also safe.
  (let ((attrs (fu:hash->alist (computed-attributes a))))
    (dolist (attr attrs)
      (destructuring-bind (key value) attr
        (funcall func key value)))))


;; Attribute computation and merging API

(defmethod compute-attributes ((a attributes) &key (copier-func #'identity))
  ;; Convert the semantic attributes to computed attributes using the
  ;; copier.
  ;;
  ;; NOTE: This will always clear the computed-attributes before doing
  ;; the computation.
  ;; NOTE: For now, this uses the same copier func for all attributes.

  (clear-computed-attributes a)
  (do-semantic-attributes a
    (lambda (key value)
      (setf (computed-attribute a key) (funcall copier-func value)))))


(defmethod overlay-computed-attributes ((a attributes) &rest ordering)
  (declare (ignore a ordering))
  nil)
