(defun random-shots (base extra)
  (+ base (random extra)))

((@camera
  ((transform)))

 (@player-ship
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

;; this stuff is here to make testing this concrete expansion easier.

(defun expansion ()

;;; Possible expansion
  (progn

    (defun random-shots (base extra)
      (+ base (random extra)))

    ;; Called when we're ready to create all of the actors, components, and
    ;; scene-tree and shove it all into core-state.
    (lambda (core-state)

      (let ((actors (make-hash-table)))
        (dolist (name '(@universe @camera @player-ship @turret @laser
                        @missile))
          (setf (gethash name actors) (make-instance 'actor
                                                     :id name
                                                     :state :initialize)))

        (let ((@universe (gethash '@universe actors))
              (@universe-initializing-components ())
              (@camera (gethash '@camera actors))
              (@camera-initializing-components ())
              (@player-ship (gethash '@player-ship actors))
              (@player-ship-initializing-components ())
              (@turret (gethash '@turret actors))
              (@turret-initializing-components ())
              (@laser (gethash '@laser actors))
              (@laser-initializing-components ())
              (@missile (gethash '@missile actors))
              (@missile-initializing-components ()))

          (add-multiple-components
           @universe
           (list (make-component 'transform :state :initialize)))

          (add-multiple-components
           @camera
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

          (push (lambda ()
                  (reinitialize-instance
                   (get-component 'transform @universe)
                   :actor @universe))
                @universe-initializing-components)

          (push (lambda ()
                  (reinitialize-instance
                   (get-component 'transform @camera)
                   :actor @camera))
                @camera-initializing-components)

          (push (lambda ()
                  (reinitialize-instance
                   (get-component 'transform @player-ship)
                   :actor @player-ship))
                @player-ship-initializing-components)

          (push (lambda ()
                  (reinitialize-instance
                   (get-component 'hit-points @player-ship)
                   :actor @player-ship
                   :hp 100))
                @player-ship-initializing-components)

          (push (lambda ()
                  (reinitialize-instance
                   (get-component 'transform @turret)
                   :actor @turret
                   :translation/current (vec 1 2 0)))
                @turret-initializing-components)

          (push (lambda ()
                  (reinitialize-instance
                   (get-component 'gun-manager @turret)
                   :actor @turret
                   :active-gun 0
                   :guns (vector (get-component 'gun @laser)
                                 (get-component 'gun @missile))))
                @turret-initializing-components)

          (push (lambda ()
                  (reinitialize-instance
                   (get-component 'transform @laser)
                   :actor @laser))
                @laser-initializing-components)

          (push (lambda ()
                  (reinitialize-instance
                   (get-component 'gun @laser)
                   :actor @laser
                   :shot-count 10
                   :shot-type :beam))
                @laser-initializing-components)

          (push (lambda ()
                  (reinitialize-instance
                   (get-component 'transform @missile)
                   :actor @missile))
                @missile-initializing-components)

          (push (lambda ()
                  (reinitialize-instance
                   (get-component 'gun @missile)
                   :actor @missile
                   :shot-count (random-shots 5 5)
                   :shot-type :homing))
                @missile-initializing-components)

          (add-child
           (get-component 'transform @universe)
           (get-component 'transform @player-ship))

          (add-child
           (get-component 'transform @universe)
           (get-component 'transform @camera))

          (add-child
           (get-component 'transform @player-ship)
           (get-component 'transform @turret))

          (add-child
           (get-component 'transform @turret)
           (get-component 'transform @laser))

          (add-child
           (get-component 'transform @turret)
           (get-component 'transform @missile))

          (add-initializing-actor core-state @universe
                                  @universe-initializing-components)

          (add-initializing-actor core-state @camera
                                  @camera-initializing-components)

          (add-initializing-actor core-state @player-ship
                                  @player-ship-initializing-components)

          (add-initializing-actor core-state @turret
                                  @turret-initializing-components)

          (add-initializing-actor core-state @laser
                                  @laser-initializing-components)

          (add-initializing-actor core-state @missile
                                  @missile-initializing-components)

          (add-scene-tree-root core-state @universe)

          (values actors @universe core-state))))))


(defun testme ()
  (multiple-value-bind (ht root core-state)
      (funcall (expansion) (make-core-state))

    (list ht root core-state)))
