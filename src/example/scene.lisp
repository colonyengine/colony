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

;; NOTE: In order for the macro to generate a call to create a component, it
;; will synthesize the name make-TYPE and use that to make that component.

(progn

;;; all forms except last

  (defun random-shots (base extra)
    (+ base (random extra)))

;;; last form (scene dsl) [possible] expansion

  ;; NOTE: Whenever you see initarg-* it means a gensymed form.

  ;; NOTE: Other ones should be fixed up to be gensymed, but I haven't done that
  ;; yet.

  (let ((objects (make-hash-table))
        (object-names `(<universe> <player-ship> <turret> <laser> <missle>)))
    (dolist (name object-names)
      (setf (gethash name objects) (make-instance 'game-object)))

;;; Initialize <universe>

    (let ((initarg-game-object (gethash '<universe> objects)))
      (add-component (gethash '<universe> objects)
                     (make-transform :game-object initarg-game-object)))

;;; Initialize <player-ship>

    (let ((initarg-game-object (gethash '<player-ship> objects)))
      (add-component (gethash '<player-ship> objects)
                     (make-transform :game-object initarg-game-object)))

;;; Initialize <turret>

    (let ((initarg-game-object (gethash '<turret> objects))
          (initarg-translation/current (vec 1 2 0)))
      (add-component
       (gethash '<turret> objects)
       (make-transform :game-object initarg-game-object
                       :translation/current initarg-translation/current)))

    (let ((initarg-game-object (gethash '<turrent> objects))
          (initarg-active-gun 0)
          (initarg-guns (vector (gethash '<laser> objects)
                                (gethash '<missle> objects))))
      (add-component (gethash '<turret> objects)
                     (make-gun-manager :game-object initarg-game-object
                                       :active-gun initarg-active-gun
                                       :guns initarg-guns)))

;;; Initialize <laser>

    (let ((initarg-game-object (gethash '<laser> objects)))
      (add-component (gethash '<laser> objects)
                     (make-transform :game-object initarg-game-object)))

    (let ((initarg-game-object (gethash '<laser> objects))
          (initarg-shots 10)
          (initarg-shot-type :beam))
      (add-component (gethash '<laser> objects)
                     (make-gun :game-object initarg-game-object
                               :shots initarg-shots
                               :shot-type initarg-shot-type)))

;;; Initialize <missle>

    (let ((initarg-game-object (gethash '<missle> objects)))
      (add-component (gethash '<missle> objects)
                     (make-transform :game-object initarg-game-object)))

    (let ((initarg-game-object (gethash '<missle> objects))
          (initarg-shots (random-shots 5 5))
          (initarg-shot-type :homing))
      (add-component (gethash '<missle> objects)
                     (make-gun :game-object initarg-game-object
                               :shots initarg-shots
                               :shot-type initarg-shot-type)))


;;; Create the scene tree

    (add-child
     (get-component 'transform (gethash '<universe> objects))
     (get-component 'transform (gethash '<player-ship> objects)))

    (add-child
     (get-component 'transform (gethash '<player-ship> objects))
     (get-component 'transform (gethash '<turret> objects)))

    (add-child
     (get-component 'transform (gethash '<turret> objects))
     (get-component 'transform (gethash '<laser> objects)))

    (add-child
     (get-component 'transform (gethash '<turret> objects))
     (get-component 'transform (gethash '<missle> objects)))

    (values objects '<universe>)))
