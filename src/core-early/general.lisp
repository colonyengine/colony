(in-package #:colony)

;;;; This file houses various general-purpose utilities used across Colony
;;;; Engine, that either don't belong in vutils, or just haven't been
;;;; cross-ported yet.

;;;; Please refrain from adding any functions to this file that do not belong;
;;;; these are general purpose utilities that could be useful in other
;;;; scenarios outside of game development. This rule will help us should we
;;;; choose to lift any of these into external libraries.

;; TODO: This code seems to be at the meta-level but we don't quite have a
;; place in the source for it to go, so when we get more of this kind of code,
;; we'll make a place. This stuff has to happen EARLY in the code loading too
;; and maybe shoved into a live-coding.lisp or meta-environment.lisp named
;; file.

(defvar *no-core-value* :no-core-available)
(defvar *core-debug* *no-core-value*)

(defun core-bound-p (core)
  (not (eq core *no-core-value*)))

;; NOTE: This macro is simple right now, but if there is a choice of currently
;; running cores (say *core-debug* is a collection of executing cores), it
;; could provide a menu to allow the appdev to select one with which the live
;; coding action should happen. When there is only one core (currently this is
;; how the code is written), it selects it automatically. If there is no
;; interactive core, do nothing.
(defmacro with-selected-interactive-core ((core-var) &body body)
  `(when (c:core-bound-p c::*core-debug*)
     (let ((,core-var c::*core-debug*))
       (declare (ignorable ,core-var))
       ,@body)))
