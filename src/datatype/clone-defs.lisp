(in-package #:virality.clone)

;;;; This file describes the CLONE API and associated objects. The CLONE API is
;;;; a means to duplicate objects like arrays, class instances, structure
;;;; instances, etc, etc. Even though we use the word CLONE, the actual
;;;; semantics may be shallow, or deep, or whatever mixed copies which are
;;;; specified by a CLONE-POLICY system.
;;;;
;;;; We DO NOT use the MOP to clone objects because of the well known "EQUAL
;;;; problem" which is that if an object has multiple references to the same
;;;; sub-entity, we don't know how that sub-entity will compare in equality
;;;; terms and a generic copier would end up producing multiple copies of
;;;; something which should have been one copy and a reference.  So, we lessen
;;;; this problem by requiring the user of the CLONE API to write type specific
;;;; cloning methods. Then there is at least a chance of fixing it on a per
;;;; type basis.
;;;;
;;;; The CLONE API is split into two pieces: A generic function called CLONE
;;;; and generic function called CLONE-OBJECT. The responsibility of CLONE is
;;;; to figure out how to allocate an instance of memory (if applicable) that
;;;; will hold the copy, and CLONE-OBJECT is a progn style from least to most
;;;; specific generic function which is responsible for actually copying the
;;;; data from the old object into the new object by recusively calling CLONE
;;;; on the sub-objects.  What is actually copied is controlled by a
;;;; CLONE-POLICY instance passed to CLONE and subsequently to CLONE-OBJECT.
;;;;
;;;; There is a small "shortcut API" too to help cloning with common policies.

;;; The CLONE-POLICY representation.

;; All clone policies must have this class as a base class.
(defclass clone-policy () ())

;; An IDENTITY-CLONE policy returns the original object as if IDENTITY had been
;; the cloning function.
(defclass identity-clone (clone-policy) ())

;; An ALLOCATING-CLONE policy generally may cause a memory allocation and the
;; subsequent cloning of the aggregate pieces of the object to occur (elements
;; of an array, or slots in a class instance or structure, etc). This policy is
;; refined and controlled by derived types of this class.
(defclass allocating-clone (identity-clone) ())

;; A SHALLOW-CLONE will, when given a collection, allocate a new copy of that
;; collection and then process the elements of the collection with the
;; IDENTITY-CLONE policy.
;;
;; Here, collection is a loose term meaning any sequence like vectors,
;; or arrays, or aggregate type like hash tables.  User defined types like
;; structure or classes are left to have an individual method for them to
;; ensure the right semantics happen. Lists/cons are dealt with via a more
;; specialized type that has shallow-clone as a parent.
(defclass shallow-clone (allocating-clone) ())

;; This is for anything relating to shallow clones of list/cons structure.
;; CLONE and CLONE-OBJECT will specialize on CONS only
;; only for these policies and LIST is left unspecialized.
(defclass shallow-clone-cons (shallow-clone) ())
(defclass shallow-clone-list (shallow-clone) ())
(defclass shallow-clone-alist (shallow-clone) ())
(defclass shallow-clone-tree (shallow-clone) ())

;; A DEEP-CLONE will, when given a collection, allocate a new copy of that
;; collection and then process the elements of the collection recursively
;; passing the same policy given to CLONE. Deep clone always copies everything
;; and as deeply as possible.
(defclass deep-clone (allocating-clone) ())


;;; The EQL-MAP representation and API.

;; This class builds a map between original objects and their clones. It is
;; used to replicate referential graph structure between a set of objects
;; (which could bne cons cells, hash tables, structures, classes, arrays, etc).
(defclass eql-map ()
  (;; Key: Original object
   ;; Value: cloned object (if appropriate, could be the same object too).
   (%transition-table :allocation :class
                      :accessor transition-table
                      :initarg :transition-table
                      :initform (u:dict #'eql))))

;;; The CLONE API.

;;; The CLONE generic function which is how a clone of something is requested.
(defgeneric clone (object clone-policy eql-map &key &allow-other-keys)
  (:documentation "CLONE the OBJECT according to the CLONE-POLICY. Clone
policies of shallow-clone and deep-clone (and those derived from them) will
cause memory to be allocated at least for OBJECT under most circumstances."))

;;; The CLONE-OBJECT API.

;; The CLONE-OBJECT generic function which, from least specific to most, copies
;; the data from the original object into the cloned object (as dictated by the
;; CLONE-POLICY). This does the work of the actual cloning once the memory had
;; been allocated by CLONE.
(defgeneric clone-object (cloned-object original-object clone-policy eql-map
                          &key &allow-other-keys)
  (:method-combination progn :most-specific-last))
