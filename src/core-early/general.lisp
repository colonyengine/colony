(in-package #:virality)

;;;; This file houses various general-purpose utilities used across Virality
;;;; Engine, that either don't belong in vutils, or just haven't been
;;;; cross-ported yet.

;;;; Please refrain from adding any functions to this file that do not belong;
;;;; these are general purpose utilities that could be useful in other
;;;; scenarios outside of game development. This rule will help us should we
;;;; choose to lift any of these into external libraries.

(defvar *no-core-value* :no-core-available)
(defvar *core-debug* *no-core-value*)

(defun core-bound-p (core)
  (not (eq core *no-core-value*)))
