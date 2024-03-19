(in-package #:colony)

(defstruct (button-state
            (:predicate nil)
            (:copier nil))
  enter
  enabled
  exit)
