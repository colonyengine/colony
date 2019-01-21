(in-package #:%first-light)

;;; This will be the new home of the attributes type so we can manage DSL attributes in a nicer way.
;;; Semantic-attributes are usually written by a human and read from a DSL.

(defclass attributes ()
  ((%semantic-attributes :reader semantic-attributes
                         :initarg :semantic-attributes
                         :initform (u:dict #'eql))
   (%computed-attributes :reader computed-attributes
                         :initarg :computed-attributes
                         :initform (u:dict #'eql))))

(defun make-attributes (&rest args)
  (apply #'make-instance 'attributes args))

;;; Semantic attributes API

(defmethod semantic-attribute ((attrs attributes) (name symbol))
  (u:href (semantic-attributes attrs) name))

(defmethod (setf semantic-attribute) (value (attrs attributes) (name symbol))
  (setf (u:href (semantic-attributes attrs) name) value))

(defmethod clear-semantic-attributes ((attrs attributes))
  (clrhash (semantic-attributes attrs)))

(defmethod do-semantic-attributes ((attrs attributes) func)
  ;; First make a copy-ish of the hash table in case the func wants to modify the hash table.
  (u:do-hash (k v (u:copy-hash-table (semantic-attributes attrs)))
    (funcall func k v)))

;;; Computed attributes API

(defmethod computed-attribute ((attrs attributes) (name symbol))
  (u:href (computed-attributes attrs) name))

(defmethod (setf computed-attribute) (value (attrs attributes) (name symbol))
  (setf (u:href (computed-attributes attrs) name) value))

(defmethod clear-computed-attributes ((attrs attributes))
  (clrhash (computed-attributes attrs)))

(defmethod do-computed-attributes ((attrs attributes) func)
  ;; First make a copy-ish of the hash table in case the func wants to modify the hash table.
  (u:do-hash (k v (u:copy-hash-table (computed-attributes attrs)))
    (funcall func k v)))

;;; Attribute computation and merging API

(defmethod compute-attributes ((attrs attributes) &key (copier-func #'identity))
  ;; Convert the semantic attributes to computed attributes using the copier.
  ;; NOTE: This will always clear the computed-attributes before doing the computation.
  ;; NOTE: For now, this uses the same copier func for all attributes.
  (clear-computed-attributes attrs)
  (do-semantic-attributes attrs
    (lambda (key value)
      (setf (computed-attribute attrs key) (funcall copier-func value)))))

(defmethod overlay-computed-attributes ((attrs attributes) &rest ordering)
  (declare (ignore attrs ordering))
  nil)
