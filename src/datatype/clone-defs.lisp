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

;; An ALLOCATING-CLONE policy generally will cause a memory allocation and the
;; subsequent cloning of the aggregate pieces of the object to occur (elements
;; of an array, or slots in a class instance or structure, etc). This policy is
;; refined and controlled by derived types of this class.
(defclass allocating-clone (identity-clone) ())

;; A SHALLOW-CLONE will, when given a collection, allocate a new copy of that
;; collection and then process the elements of the collection with the
;; IDENTITY-CLONE policy.
;;
;; Here, collection is a loose term meaning any sequence like lists, vectors,
;; or arrays, or aggregate type like hash tables.  User defined types like
;; structure or classes are left to have an individual method for them to
;; ensure the right semantics happen.
(defclass shallow-clone (allocating-clone) ())

;; A DEEP-CLONE will, when given a collection, allocate a new copy of that
;; collection and then process the elements of the collection recursively
;; passing the same policy given to CLONE.
(defclass deep-clone (allocating-clone) ())

;;; The CLONE API.

;;; The CLONE generic function which is how a clone of something is requested.
(defgeneric clone (object clone-policy &key &allow-other-keys))

;;; The CLONE-OBJET API.

;; The CLONE-OBJECT generic function which, from least specific to most, copies
;; the data from the original object into the cloned object (as dictated by the
;; CLONE-POLICY). This does the work of the actual cloning once the memory had
;; been allocated by CLONE.
(defgeneric clone-object (cloned-object original-object clone-policy
                          &key &allow-other-keys)
  (:method-combination progn :most-specific-last))
