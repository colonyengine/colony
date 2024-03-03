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
;;;; will hold the copy, and CLONE-OBJECT is a progn style generic function
;;;; from least to most specific generic which is responsible for actually
;;;; copying the data from the old object into the new object by recusively
;;;; calling CLONE on the sub-objects.  What is actually copied is controlled
;;;; by a CLONE-POLICY instance passed to CLONE and subsequently to
;;;; CLONE-OBJECT.
;;;;
;;;; There is a small "shortcut API" too to help cloning with common policies.


;;; The INTENTION representation.

;; Intentions are how we'd like to view cloning a particular type. This is
;; most important when cloning cons cells, because they can be viewed as:
;;
;; cons cells not other wise associated with anything,
;; list structure cells, alist kv cells, and graph cells (which include trees).
;; Plus cons cells can trivially point to themslves making cycles and other
;; horrors.
(defclass intention ()
  ((%sort-id :reader sort-id
             :initarg :sort-id
             :initform 0)))
(defclass no-specific-intention (intention) ()
  (:default-initargs :sort-id 1))
(defclass cons-intention (intention) ()
  (:default-initargs :sort-id 2))
(defclass list-intention (intention) ()
  (:default-initargs :sort-id 3))
(defclass alist-intention (intention) ()
  (:default-initargs :sort-id 4))
(defclass graph-intention (intention) ()
  (:default-initargs :sort-id 5))
(defgeneric compare-intention (intention-left intention-right)
  (:documentation "Intentions can be thought of as identifiers of
complexity/functionality/etc. This function returns -1 if INTENTION-LEFT is
'less complex' than INTENTION-RIGHT, or it returns 0 if the 'complexity is the
same', and 1 if the right side is 'more complex' than the left side.
Complexity has an undefined general meaning. It is only meaningful to the
specified types of intention and their contextual use."))


;; TODO: Change CLONE and CLONE-OBJECT to additionally take an intention
;; object. Then fix clone-shallow and such to pass the right intention.  This
;; makes it so the methods can be finely defined to solve interesting problems
;; with how the eql-map-entry records the intention for why the transition was
;; created in the first place. It sorta sucks to increase the interface width,
;; but it follows KMP's point of view and allows us to better record the
;; intention in the eql-map-entry.





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
;; IDENTITY-CLONE policy. Shared structure is a bit painful with this policy,
;; so try not to do that, it does try to at least by cycle aware though.
(defclass shallow-clone (allocating-clone) ())

;; A DEEP-CLONE will, when given a collection, allocate a new copy of that
;; collection and then process the elements of the collection recursively
;; passing the same policy given to CLONE. Deep clone always copies everything
;; (both connection structure and objects) as deeply as possible.
(defclass deep-clone (allocating-clone) ())


;;; The EQL-MAP representation and API.

(defclass eql-map-entry ()
  (;; The original object for which this entry exists. By virtue of the
   ;; eql-map-entry existing, the original object is also considered "visited"
   ;; when traversing the original structure in which it was found.
   (%origin :accessor origin
            :initarg :origin)
   ;; Is there a transition from the origin to the target?
   (%transition-p :accessor transition-p
                  :initarg :transition-p
                  :initform nil)
   ;; The target of the cloning transition, usually a clone of the original
   ;; object.  It isn't _required_ to be a newly allocated clone, but often is.
   (%target :accessor target
            :initarg :target)
   ;; Under what intent was this item cloned? It is a reference to the
   ;; intention object. This is here because it could be the fact that the same
   ;; origin might be cloned under a different intention and that might affect
   ;; how the previous copy is modified.
   (%intent :accessor intent
            :initarg :intent)))

;; This class builds a map between original objects and their clones. It is
;; used to replicate (as needed under the intentions used) referential graph
;; structure between a set of objects. It is the case that anything inserted
;; into this table markes the original object as being "visited".
(defclass eql-map ()
  (;; Key: original object
   ;; Value: eql-map-entry instance.
   (%entry-table :accessor entry-table
                 :initarg :entry-table
                 :initform (u:dict #'eql))
   ;; An EQ hash table, present if stats are being kept.
   ;; Key is a keyword symbol representing a statistics domain.
   ;; Value is whatever you want it to be, often another hash table.
   (%stats :accessor stats
           :initarg :stats
           :initform nil)))

;;; The CLONE API.

;;; The CLONE generic function which is how a clone of something is requested.
(defgeneric clone (object clone-policy intenton eql-map &key &allow-other-keys)
  (:documentation "CLONE the OBJECT according to the CLONE-POLICY and return
two values, the cloned object and the EQL-MAP. Clone policies of shallow-clone
and deep-clone (and those derived from them) will cause memory to be allocated
at least for OBJECT under most (but not all) circumstances."))

(defgeneric allocatablep (object)
  (:documentation "Return T if the object is an allocatable entity,
NIL otherwise."))

(defgeneric clone-allocate (object eql-map)
  (:documentation "This generic function must ONLY allocate a new instance of
the passed in object. It does not have any responsibility to fill in any values
beyond the required defaults for the allocation. Anything filled in by this
generic function will almost certainly be overwritten by CLONE-OBJECT when the
returned object is passed to that generic function. This function returns a
newly allocated default instance of OBJECT that maintains any original
contraints on the original OBJECT. The EQL-MAP is used to maintain statistics
about the allocation, such as how many of what type had been allocated.
Examples of constraints would be things like keeping the hash table test the
same, or if an array is adjustable then the newly allocated version of it is
also adjustable, etc, etc."))

;;; The CLONE-OBJECT API.

(defgeneric clone-object (cloned-object original-object clone-policy intention
                          last-known-intention eql-map
                          &key &allow-other-keys)
  (:method-combination progn :most-specific-last)
  (:documentation "The CLONE-OBJECT generic function which, from least specific
to most, copies the data from the original object into the cloned object (as
dictated by the CLONE-POLICY and INTENTION). This does the work of the actual
cloning once the memory had been allocated by CLONE. The LAST-KNOWN-INTENTION
is used when we encounter an object we had previously cloned before and we're
possibly attempting to clone again and we need to see if we can just reuse it,
or, if the intention is different, we need to alter the object somehow. Returns
the cloned object."))
