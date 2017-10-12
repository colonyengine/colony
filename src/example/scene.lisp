(defun random-shots (base extra)
  (+ base (random extra)))

((<player-ship>
  ((transform))
  (<turret>
   ((transform :translation/current (vec 1 2 0))
    (gun-manager :active-gun 0
                 :guns (vector (get-component 'gun <laser>)
                               (get-component 'gun <missile>))))
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

  (let ((tmp-objects (make-hash-table))
        (tmp-object-names
          `(<universe> <player-ship> <turret> <laser> <missle>)))

    ;; A helper function to make piles of default components for a gobj.
    (flet ((insert-default-components (gobj default-components)
             (dolist (comp default-components)
               (add-component gobj comp))))

;;; Create all game-objects so future references all are valid.
      (dolist (name tmp-object-names)
        (setf (gethash name tmp-objects) (make-instance 'game-object :id name)))

;;; Create all default components for all game-object to all future reference
;;; to them in the user values in the DSL are valid.

;;; Create default components for <universe>
      (insert-default-components (gethash '<universe> tmp-objects)
                                 (list (make-transform)))
;;; Create default components for <player-ship>
      (insert-default-components (gethash '<player-ship> tmp-objects)
                                 (list (make-transform)))
;;; Create default components for <turret>
      (insert-default-components (gethash '<turret> tmp-objects)
                                 (list (make-transform)
                                       (make-gun-manager)))
;;; Create default components for <laser>
      (insert-default-components (gethash '<laser> tmp-objects)
                                 (list (make-transform)
                                       (make-gun)))
;;; Create default components for <missle>
      (insert-default-components (gethash '<missle> tmp-objects)
                                 (list (make-transform)
                                       (make-gun)))

;;; Then reinitialize all components with user values.

;;; Initialize <universe>
      (let ((initarg-game-object (gethash '<universe> tmp-objects)))
        (reinitialize-instance
         (get-component 'transform (gethash '<universe> tmp-objects))
         :game-object initarg-game-object))

;;; Initialize <player-ship>

      (let ((initarg-game-object (gethash '<player-ship> tmp-objects)))
        (reinitialize-instance
         (get-component 'transform (gethash '<player-ship> tmp-objects))
         :game-object initarg-game-object))

;;; Initialize <turret>

      (let ((initarg-game-object (gethash '<turret> tmp-objects))
            (initarg-translation/current (vec 1 2 0)))
        (reinitialize-instance
         (get-component 'transform (gethash '<turret> tmp-objects))
         :game-object initarg-game-object
         :translation/current initarg-translation/current))

      (let ((initarg-game-object (gethash '<turrent> tmp-objects))
            (initarg-active-gun 0)
            (initarg-guns
              (vector (get-component 'gun (gethash '<laser> tmp-objects))
                      (get-component 'gun (gethash '<missle> tmp-objects)))))

        (reinitialize-instance
         (get-component 'gun-manager (gethash '<turret> tmp-objects))
         :game-object initarg-game-object
         :active-gun initarg-active-gun
         :guns initarg-guns))

;;; Initialize <laser>

      (let ((initarg-game-object (gethash '<laser> tmp-objects)))
        (reinitialize-instance
         (get-component 'transform (gethash '<laser> tmp-objects))
         :game-object initarg-game-object))

      (let ((initarg-game-object (gethash '<laser> tmp-objects))
            (initarg-shots 10)
            (initarg-shot-type :beam))

        (reinitialize-instance
         (get-component 'gun (gethash '<laser> tmp-objects))
         :game-object initarg-game-object
         :shots initarg-shots
         :shot-type initarg-shot-type))


;;; Initialize <missle>

      (let ((initarg-game-object (gethash '<missle> tmp-objects)))
        (reinitialize-instance
         (get-component 'transform (gethash '<missle> tmp-objects))
         :game-object initarg-game-object))

      (let ((initarg-game-object (gethash '<missle> tmp-objects))
            (initarg-shots (random-shots 5 5))
            (initarg-shot-type :homing))

        (reinitialize-instance
         (get-component 'gun (gethash '<missle> tmp-objects))
         :game-object initarg-game-object
         :shots initarg-shots
         :shot-type initarg-shot-type))

;;; Create the scene tree

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

      (values tmp-objects '<universe>))))
