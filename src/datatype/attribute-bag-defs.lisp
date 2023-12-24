(in-package #:virality.attribute-bag)

;;; The ATTRIBUTE-BAG system is a CLOS object that holds dynamically created
;;; associations between EQUAL comparable names and values which are stored as
;;; ATTRIBUTE-VALUSE. The ATTRIBUTE-VALUE has a 'semantic' value which is often
;;; the original value discoverd in the DSL (the usual use case) and a
;;; 'computed' value which is the post processed semantic value which is ready
;;; for use in a computation. It is intended that the ATTRIBUTE-BAG is derived
;;; into other classes which may or may not have additional lots depending on
;;; the reason for derivation.

(defclass attribute-value ()
  (;; The original value of this attribute.
   (%semantic :accessor semantic
              :initarg :semantic)
   ;; When the semantic value is set, this becomes NIL, otherwise it is T.
   (%dirty :accessor dirty
           :initarg :dirty
           :initform T)
   ;; A (once or more) post processed evaluation of the semantic value.
   (%computed :accessor computed
              :initarg :computed)))

(defclass attribute-bag ()
  (;; Key is an 'attr-name' which is a form comparable with EQUAL.
   ;; Value is an attribute-value instance.
   (%attributes :reader attributes
                :initarg :attributes
                :initform (u:dict #'equal))))


(defgeneric overlay (attribute-bag &rest containers))
(defgeneric attr (attr-bag name))
(defgeneric (setf attr) (new-attr-value attr-bag name))
(defgeneric clear-attrs (attr-bag))
(defgeneric do-attr (attr-bag func &key copy-table copier-func
                     &allow-other-keys))
(defgeneric sattr (attr-bag name &optional not-found))
(defgeneric (setf sattr) (new-semantic-value attr-bag name))
(defgeneric do-sattr (attr-bag func &key copy-table copier-func
                      &allow-other-keys))
(defgeneric cattr (attr-bag name &optional not-found))
(defgeneric (setf cattr) (new-computed-value attr-bag name
                          &optional default-semval))
(defgeneric do-cattr (attr-bag func &key copy-table copier-func
                      &allow-other-keys))
