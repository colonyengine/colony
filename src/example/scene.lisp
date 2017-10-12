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

      ;; This generated let allows unedited use of the user's
      ;; evaluable forms from the DSL.  We actually use the names for
      ;; the gobjs as supplied by the user as variables in the which will
      ;; all be set to the right value in the user's evaluable forms.
      (let ((<universe> (gethash '<universe> tmp-objects))
            (<player-ship> (gethash '<player-ship> tmp-objects))
            (<turret> (gethash '<turret> tmp-objects))
            (<laser> (gethash '<laser> tmp-objects))
            (<missle> (gethash '<missle> tmp-objects)))

;;; Create all default components for all game-object to all future reference
;;; to them in the user values in the DSL are valid.

;;; Create default components for <universe>
        (insert-default-components <universe>
                                   (list (make-component 'transform)))
;;; Create default components for <player-ship>
        (insert-default-components <player-ship>
                                   (list (make-component 'transform)
                                         (make-component 'hit-points)))
;;; Create default components for <turret>
        (insert-default-components <turret>
                                   (list (make-component 'transform)
                                         (make-component 'gun-manager)))
;;; Create default components for <laser>
        (insert-default-components <laser>
                                   (list (make-component 'transform)
                                         (make-component 'gun)))
;;; Create default components for <missle>
        (insert-default-components <missle>
                                   (list (make-component 'transform)
                                         (make-component 'gun)))

;;; Then reinitialize all components with user values.

;;; Initialize <universe>
        (let ((initarg-game-object <universe>))
          (reinitialize-instance
           (get-component 'transform <universe>)
           :game-object initarg-game-object))

;;; Initialize <player-ship>

        (let ((initarg-game-object <player-ship>))
          (reinitialize-instance
           (get-component 'transform <player-ship>)
           :game-object initarg-game-object))

        (let ((initarg-game-object <player-ship>)
              (initarg-hp 100))
          (reinitialize-instance
           (get-component 'hit-points <player-ship>)
           :game-object initarg-game-object
           :hp initarg-hp))

;;; Initialize <turret>

        (let ((initarg-game-object <turret>)
              (initarg-translation/current (vec 1 2 0)))
          (reinitialize-instance
           (get-component 'transform <turret>)
           :game-object initarg-game-object
           :translation/current initarg-translation/current))

        (let ((initarg-game-object <turret>)
              (initarg-active-gun 0)
              (initarg-guns
                (vector (get-component 'gun <laser>)
                        (get-component 'gun <missle>))))

          (reinitialize-instance
           (get-component 'gun-manager <turret>)
           :game-object initarg-game-object
           :active-gun initarg-active-gun
           :guns initarg-guns))

;;; Initialize <laser>

        (let ((initarg-game-object <laser>))
          (reinitialize-instance
           (get-component 'transform <laser>)
           :game-object initarg-game-object))

        (let ((initarg-game-object <laser>)
              (initarg-shots 10)
              (initarg-shot-type :beam))

          (reinitialize-instance
           (get-component 'gun <laser>)
           :game-object initarg-game-object
           :shots initarg-shots
           :shot-type initarg-shot-type))


;;; Initialize <missle>

        (let ((initarg-game-object <missle>))
          (reinitialize-instance
           (get-component 'transform <missle>)
           :game-object initarg-game-object))

        (let ((initarg-game-object <missle>)
              (initarg-shots (random-shots 5 5))
              (initarg-shot-type :homing))

          (reinitialize-instance
           (get-component 'gun <missle>)
           :game-object initarg-game-object
           :shots initarg-shots
           :shot-type initarg-shot-type))

;;; Create the scene tree

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
         (get-component 'transform <missle>))

        (values tmp-objects '<universe>)))))
