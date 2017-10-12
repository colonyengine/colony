(defun random-shots (base extra)
  (+ base (random extra)))

((<player-ship>
  ((transform))
  (<turret>
   ((transform :translation/current (vec 1 2 0))
    (gun-manager :active-gun 0
                 :guns (vector <laser> <missile>)))
   (<laser>
    ((transform)
     (gun :shots 10
          :shot-type :beam)))
   (<missile>
    ((transform)
     (gun :shots (random-shots 5 5)
          :shot-type :homing))))))

;; Normally EOF here.

;;; Possible expansion

;; Here is a possible expansion of the above scene tree into real CL. Once we
;; implment the component and gobj stuff, we should just see if this executes in
;; its raw form, if it works like how we like it, then we write the macro to
;; generate this.
;;
;; In short, I propose we put this progn into a DEFUN called "make-scene-tree"
;; and see if it works when we call it. :)

;; NOTE: In order for the macro to generate a call to create a component, it
;; will synthesize the name make-TYPE and use that to make that component.

(progn

;;; butlast forms

  (defun random-shots (base extra)
    (+ base (random extra)))

;;; last form (scene dsl) [possible] expansion

  ;; NOTE: Whenever you see initarg-* it means a gensymed form.

  ;; NOTE: Other ones should be fixed up to be gensymed, but I haven't done that
  ;; yet.

  (let ((tmp-objects (make-hash-table))
        (tmp-object-names `(<universe> <player-ship> <turret> <laser> <missle>)))
    ;; First, create the empty game tmp-objects so I can have real references to
    ;; them.
    (loop :for name :in tmp-object-names
          :do (setf (gethash name tmp-objects) (make-instance 'game-object)))

;;; Initialize <universe>

    (let ((initarg-game-object (gethash '<universe> tmp-objects)))
      (add-component (gethash '<universe> tmp-objects)
                     (make-transform :game-object initarg-game-object)))

    ;; Each game-object is processed one by one in tree order. Each then has a
    ;; pile of LET forms around each creation and insertion of the component.
    ;; The LET form binds the values of the initarg plist and then assigns them
    ;; to the initarg.

;;; Initialize <player-ship>

    (let ((initarg-game-object (gethash '<player-ship> tmp-objects)))
      (add-component (gethash '<player-ship> tmp-objects)
                     (make-transform :game-object initarg-game-object)))

;;; Initialize <turret>

    (let ((initarg-game-object (gethash '<turret> tmp-objects))
          (initarg-translation/current (vec 1 2 0)))
      (add-component
       (gethash '<turret> tmp-objects)
       (make-transform :game-object initarg-game-object
                       :translation/current initarg-translation/current)))

    (let ((initarg-game-object (gethash '<turrent> tmp-objects))
          (initarg-active-gun 0)
          (initarg-guns (vector (gethash '<laser> tmp-objects)
                                (gethash '<missle> tmp-objects))))
      (add-component (gethash '<turret> tmp-objects)
                     (make-gun-manager :game-object initarg-game-object
                                       :active-gun initarg-active-gun
                                       :guns initarg-guns)))

;;; Initialize <laser>

    (let ((initarg-game-object (gethash '<laser> tmp-objects)))
      (add-component (gethash '<laser> tmp-objects)
                     (make-transform :game-object initarg-game-object)))

    (let ((initarg-game-object (gethash '<laser> tmp-objects))
          (initarg-shots 10)
          (initarg-shot-type :beam))
      (add-component (gethash '<laser> tmp-objects)
                     (make-gun :game-object initarg-game-object
                               :shots initarg-shots
                               :shot-type initarg-shot-type)))

;;; Initialize <missle>

    (let ((initarg-game-object (gethash '<missle> tmp-objects)))
      (add-component (gethash '<missle> tmp-objects)
                     (make-transform :game-object initarg-game-object)))

    (let ((initarg-game-object (gethash '<missle> tmp-objects))
          (initarg-shots (random-shots 5 5))
          (initarg-shot-type :homing))
      (add-component (gethash '<missle> tmp-objects)
                     (make-gun :game-object initarg-game-object
                               :shots initarg-shots
                               :shot-type initarg-shot-type)))


;;; Now, we wire together the actual scene tree through the transform
;;; components. The transform component has the interface to and keeps the scene
;;; tree.

    (add-child
     (get-component 'transform (gethash '<universe> tmp-objects))
     (get-component 'transform (gethash '<player-ship> tmp-objects)))

    (add-child
     (get-component 'transform (gethash '<player-ship> tmp-objects))
     (get-component 'transform (gethash '<turret> tmp-objects)))

    (add-child
     (get-component 'transform (gethash '<turret> tmp-objects))
     (get-component 'transform (gethash '<laser> tmp-objects)))

    (add-child
     (get-component 'transform (gethash '<turret> tmp-objects))
     (get-component 'transform (gethash '<missle> tmp-objects)))

    (values tmp-objects '<universe>)))
