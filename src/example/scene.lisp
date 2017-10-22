(defun random-shots (base extra)
  (+ base (random extra)))

((@player-ship
  ((transform)
   (hit-points :hp 100))
  (@turret
   ((transform :translation/current (vec 1 2 0))
    (gun-manager :active-gun 0
                 :guns (vector (get-component 'gun @laser)
                               (get-component 'gun @missile))))
   (@laser
    ((transform)
     (gun :shot-count 10
          :shot-type :beam)))
   (@missile
    ((transform)
     (gun :shot-count (random-shots 5 5)
          :shot-type :homing))))))

;;; Normally EOF here.

;;; Possible expansion

(progn

  (defun random-shots (base extra)
    (+ base (random extra)))

  ;; Called when we're ready to create all of the actors, components, and
  ;; scene-tree and shove it all into core-state.
  (lambda (core-state)
    ;; Until this gets used in here...
    (declare (ignorable core-state))

    (let ((actors (make-hash-table)))
      (dolist (name '(@universe @player-ship @turret @laser @missile))
        (setf (gethash name actors) (make-instance 'actor
                                                   :id name
                                                   :state :initialize)))

      (let ((@universe (gethash '@universe actors))
            (@player-ship (gethash '@player-ship actors))
            (@turret (gethash '@turret actors))
            (@laser (gethash '@laser actors))
            (@missile (gethash '@missile actors)))

        (add-multiple-components
         @universe
         (list (make-component 'transform :state :initialize)))

        (add-multiple-components
         @player-ship
         (list (make-component 'transform :state :initialize)
               (make-component 'hit-points :state :initialize)))

        (add-multiple-components
         @turret
         (list (make-component 'transform :state :initialize)
               (make-component 'gun-manager :state :initialize)))

        (add-multiple-components
         @laser
         (list (make-component 'transform :state :initialize)
               (make-component 'gun :state :initialize)))

        (add-multiple-components
         @missile
         (list (make-component 'transform :state :initialize)
               (make-component 'gun :state :initialize)))

        (reinitialize-instance
         (get-component 'transform @universe)
         :actor @universe)

        (reinitialize-instance
         (get-component 'transform @player-ship)
         :actor @player-ship)

        (reinitialize-instance
         (get-component 'hit-points @player-ship)
         :actor @player-ship
         :hp 100)

        (reinitialize-instance
         (get-component 'transform @turret)
         :actor @turret
         :translation/current (vec 1 2 0))

        (reinitialize-instance
         (get-component 'gun-manager @turret)
         :actor @turret
         :active-gun 0
         :guns (vector (get-component 'gun @laser)
                       (get-component 'gun @missile)))

        (reinitialize-instance
         (get-component 'transform @laser)
         :actor @laser)

        (reinitialize-instance
         (get-component 'gun @laser)
         :actor @laser
         :shot-count 10
         :shot-type :beam)

        (reinitialize-instance
         (get-component 'transform @missile)
         :actor @missile)

        (reinitialize-instance
         (get-component 'gun @missile)
         :actor @missile
         :shot-count (random-shots 5 5)
         :shot-type :homing)

        (add-child
         (get-component 'transform @universe)
         (get-component 'transform @player-ship))

        (add-child
         (get-component 'transform @player-ship)
         (get-component 'transform @turret))

        (add-child
         (get-component 'transform @turret)
         (get-component 'transform @laser))

        (add-child
         (get-component 'transform @turret)
         (get-component 'transform @missile))

        (values actors '@universe)))))
