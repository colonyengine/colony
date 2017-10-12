(defun random-shots (base extra)
  (+ base (random extra)))

((<player-ship>
  ((transform)
   (hit-points :hp 100))
  (<turret>
   ((transform :translation/current (vec 1 2 0))
    (gun-manager :active-gun 0
                 :guns (vector (get-component 'gun <laser>)
                               (get-component 'gun <missile>))))
   (<laser>
    ((transform)
     (gun :shot-count 10
          :shot-type :beam)))
   (<missile>
    ((transform)
     (gun :shot-count (random-shots 5 5)
          :shot-type :homing))))))

;;; Normally EOF here.

;;; Possible expansion

(progn

  (defun random-shots (base extra)
    (+ base (random extra)))

  (let ((objects (make-hash-table)))
    (dolist (name '(<universe> <player-ship> <turret> <laser> <missile>))
      (setf (gethash name objects) (make-instance 'game-object :id name)))

    (let ((<universe> (gethash '<universe> objects))
          (<player-ship> (gethash '<player-ship> objects))
          (<turret> (gethash '<turret> objects))
          (<laser> (gethash '<laser> objects))
          (<missile> (gethash '<missile> objects)))

      (add-multiple-components
       <universe>
       (list (make-component 'transform)))

      (add-multiple-components
       <player-ship>
       (list (make-component 'transform)
             (make-component 'hit-points)))

      (add-multiple-components
       <turret>
       (list (make-component 'transform)
             (make-component 'gun-manager)))

      (add-multiple-components
       <laser>
       (list (make-component 'transform)
             (make-component 'gun)))

      (add-multiple-components
       <missile>
       (list (make-component 'transform)
             (make-component 'gun)))

      (reinitialize-instance
       (get-component 'transform <universe>)
       :game-object <universe>)

      (reinitialize-instance
       (get-component 'transform <player-ship>)
       :game-object <player-ship>)

      (reinitialize-instance
       (get-component 'hit-points <player-ship>)
       :game-object <player-ship>
       :hp 100)

      (reinitialize-instance
       (get-component 'transform <turret>)
       :game-object <turret>
       :translation/current (vec 1 2 0))

      (reinitialize-instance
       (get-component 'gun-manager <turret>)
       :game-object <turret>
       :active-gun 0
       :guns (vector (get-component 'gun <laser>)
                     (get-component 'gun <missile>)))

      (reinitialize-instance
       (get-component 'transform <laser>)
       :game-object <laser>)

      (reinitialize-instance
       (get-component 'gun <laser>)
       :game-object <laser>
       :shot-count 10
       :shot-type :beam)

      (reinitialize-instance
       (get-component 'transform <missile>)
       :game-object <missile>)

      (reinitialize-instance
       (get-component 'gun <missile>)
       :game-object <missile>
       :shot-count (random-shots 5 5)
       :shot-type :homing)

      (add-child
       (get-component 'transform <universe>)
       (get-component 'transform <player-ship>))

      (add-child
       (get-component 'transform <player-ship>)
       (get-component 'transform <turret>))

      (add-child
       (get-component 'transform <turret>)
       (get-component 'transform <laser>))

      (add-child
       (get-component 'transform <turret>)
       (get-component 'transform <missile>))

      (values objects '<universe>))))
