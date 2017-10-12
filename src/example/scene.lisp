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
 ((transform)) ;; along with this.
 (<player-ship>
  ((transform))
  (<turret>
   ((transform :translation/current (vec 1 2 0))
    (gun-manager
     :active-gun 0
     :guns (vector <laser> <missile>)))
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
;; Once we implment the component and gobj stuff, we should just see if this
;; executes in its raw form, if it works like how we like it, then we write the
;; macro to generate this.
;;
;; In short, I propose we put this progn into a DEFUN called "make-scene-tree"
;; and see if it works when we call it. :)

(progn
  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; all of the stuff before the last form, verbatim.
  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;
  (defun random-shots (base extra)
    (+ base (random extra)))

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; The last scene-tree form, reimagined....
  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;

  ;; NOTE: Whenever you see initarg-* it means a gensymed form.
  ;; NOTE: Other ones should be fixed up to be gensymed, but I haven't done
  ;; that yet.

  (let* ((num-objs 5)
         (objs (make-hash-table))
         (obj-names `(<universe> <player-ship> <turret> <laser> <missle>)))
    ;; First, create the empty game objects so I can have real
    ;; references to them.
    (loop :for name :in obj-names :do
      (setf (gethash name objs) (make-instance 'gobj)))

    ;;;; ;;;;;;;;;;;;;;;;;;;
    ;;;; Initializating <universe>
    ;;;; ;;;;;;;;;;;;;;;;;;;
    ;; add component transform
    (let ((initarg-gobj (gethash '<universe> objs)))
      (add-component (gethash '<universe> objs)
                     (make-instance 'transform
                                    :gobj initarg-gobj)))

    ;; Each gobj os processed one by one in tree order.
    ;; Each gob then has a pile of LET forms around each creation and
    ;; insertion od the component. The LET form binds the values of the
    ;; initarg plist and then assignes them to the initarg.


    ;;;; ;;;;;;;;;;;;;;;;;;;
    ;;;; Initializating <player-ship>
    ;;;; ;;;;;;;;;;;;;;;;;;;
    ;; add component transform.
    (let ((initarg-gobj (gethash '<player-ship> objs)))
      (add-component (gethash '<player-ship> objs)
                     (make-instance 'transform
                                    :gobj initarg-gobj)))

    ;;;; ;;;;;;;;;;;;;;;;;;;
    ;;;; Initializaing <turret>
    ;;;; ;;;;;;;;;;;;;;;;;;;
    ;; add component transform
    (let ((initarg-gobj (gethash '<turret> objs))
          (initarg-translation/current (vec 1 2 0)))
      (add-component
       (gethash '<turret> objs)
       (make-instance 'transform
                      :gobj initarg-gobj
                      :translation/current initarg-translation/current)))

    ;; add component gun-manager
    (let ((initarg-gobj (gethash '<turrent> objs))
          (initarg-active-gun 0)
          (initarg-guns (list (gethash '<laser> objs)
                              (gethash '<missle> objs))))
      (add-component (gethash '<turret> objs)
                     (make-instance 'gun-manager
                                    :gobj initarg-gobj
                                    :active-gun initarg-active-gun
                                    :guns initarg-guns)))

    ;;;; ;;;;;;;;;;;;;;;;;;;
    ;;;; Initializing <laser>
    ;;;; ;;;;;;;;;;;;;;;;;;;
    ;; add component transform
    (let ((initarg-gobj (gethash '<laser> objs)))
      (add-component (gethash '<laser> objs)
                     (make-instance 'transform
                                    :gobj initarg-obj)))
    ;; add component gun
    (let ((initarg-gobj (gethash '<laser> objs))
          (initarg-shots 10)
          (initarg-type :beam))
      (add-component (gethash '<laser> objs)
                     (make-instance 'gun
                                    :gobj initarg-gobj
                                    :shots initarg-shots
                                    :type initarg-type)))
    ;;;; ;;;;;;;;;;;;;;;;;;;
    ;;;; Initializing <missle>
    ;;;; ;;;;;;;;;;;;;;;;;;;
    ;; add component transform
    (let ((initarg-gobj (gethash '<missle> objs)))
      (add-component (gethash '<missle> objs)
                     (make-instance 'transform
                                    :gobj initarg-obj)))
    ;; add component gun
    (let ((initarg-gobj (gethash '<missle> objs))
          (initarg-shots (random-shots 5 5))
          (initarg-type :homing))
      (add-component (gethash '<missle> objs)
                     (make-instance 'gun
                                    :gobj initarg-gobj
                                    :shots initarg-gobj
                                    :type initarg-type)))


    ;;;; Now, we wire together the actual scene tree through the
    ;;;; transform components. The transform component has the
    ;;;; interface to and keeps the scene tree.

    (add-child
     ;; parent
     (get-component 'transform (gethash '<universe> objs))
     ;; child
     (get-component 'transform (gethash '<player-ship> objs)))

    (add-child
     (get-component 'transform (gethash '<player-ship> objs))
     (get-component 'transform (gethash '<turret> objs)))

    (add-child
     (get-component 'transform (gethash '<laser> objs))
     (get-component 'transform (gethash '<turret> objs)))

    (add-child
     (get-component 'transform (gethash '<missle> objs))
     (get-component 'transform (gethash '<turret> objs)))

    ;; and now return the scene tree as a values with the root noted:
    (values objs '<universe>)))
