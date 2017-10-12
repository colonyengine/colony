;; I think this entire file should just be Lisp with the exception of
;; the LAST FORM, which is the scene tree form, this allows you to
;; write any additional code you need in here that you might need.
;;
;; I think this file should be named scene.scn or something too.

(defun random-shots (base extra)
  (+ base (random extra)))

;; The scene tree is the LAST form in this DSL

;; I suggest using <name> for the gobjs names and enforcing that. I have
;; two string reasons:
;;
;; A) <value> is ONLY an entire reference to a game object as opposed
;; to :value, which could stand for itself OR a gobj name OR a initarg.
;;
;; B) symbol-macrolet will do the wrong thing in the case where the gobj name
;; coincides with a slot name in this form (and maybe in other places, like
;; using keywords for tag names, etc, etc, etc).

(<universe> ;; This must always be present and first.
 (<player-ship>
  ((transform))
  (<turret>
   ((transform :translation/current (vec 1 2 0))
    (gun-manager
     :guns (list <laser> <missile>)))
   (<laser>
    ((transform)
     (gun :shots 10
          :type :beam)))
   (<missile>
    ((transform)
     (gun :shots (random-shots 5 5)
          :type :homing))))))

l;; Normally EOF here.

;; ----------------------------------------------------
;; Here is a possible expansion of the above scene tree into real CL.
;; TODO
