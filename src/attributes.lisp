(in-package #:%fl)

;; This will be the new home of the attributes type so we can manage DSL
;; attributes in a nicer way.

;; Semantic-attributes are usually written by a human and read from a DSL.
(defclass attributes ()
  ((%semantic-attributes :reader semantic-attributes
                         :initarg :semantic-attributes
                         :initform (au:dict #'eql))
   (%computed-attributes :reader computed-attributes
                         :initarg :computed-attributes
                         :initform (au:dict #'eql))))


(defun make-attributes (&rest initargs)
  (apply #'make-instance 'attributes initargs))

;; Semantic attributes API
(defmethod semantic-attribute ((s attributes) (attribute-name symbol))
  (au:href (semantic-attributes s) attribute-name))

(defmethod (setf semantic-attribute) (newobj
                                      (s attributes)
                                      (attribute-name symbol))
  (setf (au:href (semantic-attributes s) attribute-name) newobj))

(defmethod clear-semantic-attributes ((s attributes))
  (clrhash (semantic-attributes s)))

(defmethod do-semantic-attributes ((s attributes) func)
  ;; First make a copy-ish of the hash table in case the func wants
  ;; to modify the hash table.
  ;; TODO: A little slow and memory using, but also safe.
  (let ((attrs (au:hash->alist (semantic-attributes s))))
    (dolist (attr attrs)
      (destructuring-bind (key value) attr
        (funcall func key value)))))

;; Computed attributes API

(defmethod computed-attribute ((s attributes) (attribute-name symbol))
  (au:href (computed-attributes s) attribute-name))

(defmethod (setf computed-attribute) (newobj
                                      (s attributes)
                                      (attribute-name symbol))
  (setf (au:href (computed-attributes s) attribute-name) newobj))

(defmethod clear-computed-attributes ((s attributes))
  (clrhash (computed-attributes s)))

(defmethod do-computed-attributes ((s attributes) func)
  ;; First make a copy-ish of the hash table in case the func wants
  ;; to modify the hash table.
  ;; TODO: A little slow and memory using, but also safe.
  (let ((attrs (au:hash->alist (computed-attributes s))))
    (dolist (attr attrs)
      (destructuring-bind (key value) attr
        (funcall func key value)))))


;; Attribute computation and merging API

(defmethod compute-attributes ((c attributes) &key (copier-func #'identity))
  ;; Convert the semantic attributes to computed attributes using the
  ;; copier.
  ;;
  ;; NOTE: This will always clear the computed-attributes before doing
  ;; the computation.
  ;; NOTE: For now, this uses the same copier func for all attributes.

  (clear-computed-attributes c)
  (do-semantic-attributes s
    (lambda (key value)
      (setf (computed-attribute c key) (funcall copier-func value)))))


(defmethod overlay-computed-attributes ((c attributes) &rest ordering)
  nil)
