(in-package :gear)

;; This variable is used to hold a hash table when preparing macroed
;; scene-definitions.  It is rebound to a hashtable _just before_ the
;; prepare, and then unbound after the hash table contents have been
;; recorded in another way.
;;
;; NOTE: It is my direct intention that this is NOT used to hold any
;; _persistent_ data in the image of gear.
(defvar *scene-table*)
