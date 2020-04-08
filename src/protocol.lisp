(in-package #:virality.engine)

;;; NOTE: These protocols should all be generic functions, without any :METHODs
;;; defined for them, and never DEFMETHODs. Additionally, do not define any
;;; DEFMETHODs for any protocol functions before this file in load order, or
;;; they will be overwritten by these DEFGENERICs. This file is loaded very
;;; early to define protocol functions so that we do not waste anymore time
;;; debugging why methods are getting overwritten due to DEFGENERICs defined
;;; later. All methods should be defined in the file for the respective
;;; functionality, sometime after the class definition so specializations can be
;;; computed.

;;; Kernel

(defgeneric destroy (kernel &key ttl)
  (:documentation "The keyword argument :TTL supplied in real seconds, is how
long the thing has yet to live, with NIL meaning infinity."))

;;; Component

(defgeneric on-component-initialize (component))

(defgeneric on-component-attach (component actor))

(defgeneric on-component-detach (component actor))

(defgeneric on-component-physics-update (component))

(defgeneric on-component-update (component))

(defgeneric on-component-render (component))

(defgeneric on-component-destroy (component))

;;; Collider

(defgeneric col:on-collision-enter (collider other))

(defgeneric col:on-collision-continue (collider other))

(defgeneric col:on-collision-exit (collider other))

(defgeneric col:collide-p (collider other))

;;; Resource Cache

(defgeneric rcache-layout (entry-type))

(defgeneric rcache-peek (context entry-type &rest keys))

(defgeneric rcache-lookup (context entry-type &rest keys))

(defgeneric rcache-construct (context entry-type &rest keys))

(defgeneric rcache-remove (context entry-type &rest keys))

(defgeneric rcache-dispose (context entry-type removed-value))

;;; Shared Storage

(defgeneric %shared-storage (context key))

(defgeneric (setf %shared-storage) (value context key))

;;; Actions

(defgeneric action:on-insert (action type))

(defgeneric action:on-finish (action type))

(defgeneric action:on-update (action type))

;;; Prologue/Epilogue

(defgeneric prologue (context))

(defgeneric epilogue (context))
