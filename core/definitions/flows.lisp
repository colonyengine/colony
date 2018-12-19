(in-package :%first-light)

(define-call-flow :default ()
  (flow initialize-phase
        (flow-state entry/initialize-phase :reset ()
                    (selector nil)
                    (action nil)
                    (transition pending-tasks))

        ;; High level description of this flow:
        ;;
        ;; 0. If pre-init is empty, go to 7.
        ;; 1. straight up move components from pre-init to init.
        ;; 2. straight up move actors from pre-init to init.
        ;; 3. run initalize-component by-type in init.
        ;; 4. realize-components from init to active.
        ;; 5. realize-actors from init to active.
        ;; 6. realize-phase-commit (new name: WHILE-INITIALIZE-PHASE)
        ;;    make a decision:
        ;;    If still stuff in pre-init go to 1
        ;;    If nothing in pre-init, go to 7.
        ;; 7. exit flow

        ;; 0
        (flow-state pending-tasks :reset ()
                    (selector nil)
                    (action nil)
                    (transition
                     (lambda (core-state)
                       (if (pending-preinit-tasks-p core-state)
                           init-components
                           exit/initialize-phase))))

        ;; 1
        (flow-state init-components :reset ()
                    (selector
                        (lambda (core-state)
                          (values :type-policy
                                  (component-preinit-by-type-view (tables core-state)))))
                    (action #'component/preinit->init)
                    (transition init-actors))
        ;; 2
        (flow-state init-actors :reset ()
                    (selector
                        (lambda (core-state)
                          (values :identity-policy
                                  (actor-preinit-db (tables core-state)))))
                    (action #'actor/preinit->init)
                    (transition protocol-initialize-components))

        ;; 3
        (flow-state protocol-initialize-components :reset ()
                    (selector
                        (lambda (core-state)
                          (values :type-policy
                                  (component-init-by-type-view (tables core-state)))))
                    (action #'initialize-component)
                    (transition realize-components))
        ;; 4
        (flow-state realize-components :reset ()
                    (selector
                        (lambda (core-state)
                          (values :type-policy
                                  (component-init-by-type-view (tables core-state)))))
                    (action #'component/init->active)
                    (transition realize-actors))

        ;; 5
        (flow-state realize-actors :reset ()
                    (selector
                        (lambda (core-state)
                          (values :identity-policy
                                  (actor-init-db (tables core-state)))))
                    (action #'actor/init->active)
                    (transition while-initialize-phase))

        ;; 6
        (flow-state while-initialize-phase :reset ()
                    (selector nil)
                    (action nil)
                    (transition
                     (lambda (core-state)
                       ;; Running PROTOCOL-INITIALIZE-COMPONENTS may have
                       ;; caused additional actors/components to be created, so
                       ;; we check for that here and repeat as needed.
                       (if (pending-preinit-tasks-p core-state)
                           ;; Then do the process over again.
                           init-components
                           ;; Or exit this phase, we're done initializing.
                           exit/initialize-phase))))

        ;; 7
        (flow-state exit/initialize-phase :reset ()
                    (selector nil)
                    (action nil)
                    (transition nil)))

  (flow active-phase
        (flow-state entry/active-phase :reset ()
                    (selector nil)
                    (action nil)
                    (transition physics-loop))

        (flow-state physics-loop :reset ()
                    (selector
                        (lambda (core-state)
                          (values :identity-policy
                                  core-state)))
                    (action
                     (lambda (core-state)
                       (tick core-state)
                       (fl.comp::interpolate-transforms core-state)))
                    (transition make-active-camera-view))

        (flow-state protocol-physics-update :reset ()
                    (selector
                        (lambda (core-state)
                          (values :type-policy
                                  (component-active-by-type-view (tables core-state)))))
                    (action #'physics-update-component)
                    (transition nil))

        ;; TODO: PHYSICS-COLLISIONS is not yet implemented, there may be
        ;; more work here than this single state. This will compute
        ;; collisions and then inform the recipients of those
        ;; collisions as desired in the boundary regions components
        ;; (yet to be written).
        (flow-state physics-collisions :reset ()
                    (selector nil)
                    (action nil)
                    (transition nil))

        ;; TODO: Should I run flow destroy-phase just before this next
        ;; flow-state so that those actors/components aren't even drawn?
        ;; Currently, I defer all actor/component destruction until the end of
        ;; frame, far after this. I don't know what is better yet.
        (flow-state make-active-camera-view :reset ()
                    (selector
                        ;; TODO: Move this code into a real function inside of
                        ;; the engine.
                        (lambda (core-state)
                          (symbol-macrolet ((camera (active-camera (context core-state))))
                            (unless (and camera (fl.comp::activep camera))
                              (let ((new-camera (fl.comp:find-active-camera core-state)))
                                (setf camera new-camera)))
                            (values :identity-policy
                                    camera))))
                    (action #'fl.comp:compute-camera-view)
                    (transition protocol-update-component))

        (flow-state protocol-update-component :reset ()
                    (selector
                        (lambda (core-state)
                          (values :type-policy
                                  (component-active-by-type-view (tables core-state)))))
                    (action #'update-component)
                    (transition protocol-render-component))

        (flow-state protocol-render-component :reset ()
                    (selector
                        (lambda (core-state)
                          (values :type-policy
                                  (component-active-by-type-view (tables core-state)))))
                    (action #'render-component)
                    (transition exit/active-phase))

        (flow-state exit/active-phase :reset ()
                    (selector nil)
                    (action nil)
                    (transition nil)))

  (flow destroy-phase
        (flow-state entry/destroy-phase :reset ()
                    (selector nil)
                    (action nil)
                    (transition pending-predestroy-tasks))

        ;; TODO: Fix me.
        ;; This should handle if I spawn an actor/component and then destroy it
        ;; in the same function, etc, etc.
        ;;
        ;; 0. If pre-destroy (actor and components) is empty, go to 5
        ;;
        ;; 1. move components (<= ttl 0) in pre-destroy from init/active to
        ;;    destroy.
        ;;
        ;; 2. move root actors (<= ttl 0) (and their components)
        ;;    in pre-destroy from init/active to destroy.
        ;;
        ;; 3. Recursively move all actor children found in destroy from
        ;;    init/active into destroy, Set any actor/component ttl in any
        ;;    discovered instances to 0, since the root with ttl <= 0 overrides
        ;;    all pending ttls for anything that may be in the process of being
        ;;    destroyed.
        ;;
        ;; 4. A) decrement ttl by frame-time for predestroying components
        ;;    B) decrement ttl by frame-time for predestroying actors.
        ;;
        ;; 5. If destroy is empty, go to 11.
        ;;
        ;; 6. run DESTROY-COMPONENT by-type in destroy.
        ;;
        ;; 7. disconnect all destroyed actors from the scene heirarchy.
        ;;
        ;; 8. release-components (and remove from actors) from destroy.
        ;;
        ;; 9. release-actors (which now should be empty) from destroy.
        ;;
        ;; 10. restart to see if anything else got marked to be destroyed
        ;;     during this entire process, goto 0.
        ;;
        ;; 11. exit flow.

        ;; 0
        (flow-state pending-predestroy-tasks :reset ()
                    (selector nil)
                    (action nil)
                    (transition
                     (lambda (core-state)
                       (if (pending-predestroy-tasks-p core-state)
                           prepare-predestroy-components
                           pending-destroy-tasks))))

        ;; 1
        (flow-state prepare-predestroy-components :reset ()
                    (selector
                        (lambda (core-state)
                          (values :identity-policy
                                  (component-predestroy-view (tables core-state)))))
                    (action #'component/init-or-active->destroy)
                    (transition prepare-predestroy-actors))

        ;; 2
        (flow-state prepare-predestroy-actors :reset ()
                    (selector
                        (lambda (core-state)
                          (values :identity-policy
                                  (actor-predestroy-view (tables core-state)))))
                    (action #'actor/init-or-active->destroy)
                    (transition destroy-actor-children))

        ;; 3
        (flow-state destroy-actor-children :reset ()
                    (selector
                        (lambda (core-state)
                          (values :identity-policy
                                  ;; NOTE: We get the keys here because the action will be adding
                                  ;; values to this hash table as we iterate over the keys. We need
                                  ;; to copy the list of keys in order to satisfy the traversal
                                  ;; rules of hash tables.
                                  (fl.util:hash-keys (actor-destroy-db (tables core-state))))))
                    ;; NOTE: See selector for this flow-state.
                    (action #'actor/destroy-descendants)
                    (transition decrement-component-destroy-timer))
        ;; 4 A
        (flow-state decrement-component-destroy-timer :reset ()
                    (selector
                        (lambda (core-state)
                          (values :identity-policy
                                  (component-predestroy-view (tables core-state)))))
                    (action #'component/countdown-to-destruction)
                    (transition decrement-actor-destroy-timer))

        ;; 4 B
        (flow-state decrement-actor-destroy-timer :reset ()
                    (selector
                        (lambda (core-state)
                          (values :identity-policy
                                  (actor-predestroy-view (tables core-state)))))
                    (action #'actor/countdown-to-destruction)
                    (transition pending-destroy-tasks))

        ;; 5
        (flow-state pending-destroy-tasks :reset ()
                    (selector nil)
                    (action nil)
                    (transition
                     (lambda (core-state)
                       (if (pending-destroy-tasks-p core-state)
                           protocol-destroy-component
                           exit/destroy-phase))))

        ;; 6
        (flow-state protocol-destroy-component :reset ()
                    (selector
                        (lambda (core-state)
                          (values :type-policy
                                  (component-destroy-by-type-view (tables core-state)))))
                    (action #'destroy-component)
                    (transition disconnect-destroyed-actors))

        ;; 7
        (flow-state disconnect-destroyed-actors :reset ()
                    (selector
                        (lambda (core-state)
                          (values :identity-policy
                                  (actor-destroy-db (tables core-state)))))
                    (action #'actor/disconnect)
                    (transition release-components))

        ;; 8
        (flow-state release-components :reset ()
                    (selector
                        (lambda (core-state)
                          (values :type-policy
                                  (component-destroy-by-type-view (tables core-state)))))
                    (action #'component/destroy->released)
                    (transition release-actors))

        ;; 9
        (flow-state release-actors :reset ()
                    (selector
                        (lambda (core-state)
                          (values :identity-policy
                                  (actor-destroy-db (tables core-state)))))
                    (action #'actor/destroy->released)
                    (transition restart-predestroy-phase))

        ;; 10
        (flow-state restart-predestroy-phase :reset ()
                    (selector nil)
                    (action nil)
                    (transition pending-predestroy-tasks))

        ;; 11
        (flow-state exit/destroy-phase :reset ()
                    (selector nil)
                    (action nil)
                    (transition nil)))

  (flow maintenance-phase
        (flow-state entry/maintenance-phase :reset ()
                    (selector nil)
                    (action nil)
                    (transition shader-refresh))

        (flow-state shader-refresh :reset ()
                    (selector
                        (lambda (core-state)
                          (values :identity-policy
                                  core-state)))
                    (action nil)
                    (transition exit/maintenance-phase))

        (flow-state exit/maintenance-phase :reset ()
                    (selector nil)
                    (action nil)
                    (transition nil)))

  (flow perform-one-frame
        (flow-state entry/perform-one-frame :reset ()
                    (selector nil)
                    (action nil)
                    (transition perform-recompilations))

        ;; If we have anything to recompile in the queue, do it now.
        (flow-state perform-recompilations :reset ()
                    (selector
                        (lambda (core-state)
                          (values :identity-policy
                                  core-state)))
                    (action #'recompile-queued-items)
                    (transition initialize-phase))

        (flow-state initialize-phase :reset ()
                    (selector
                        (lambda (core-state)
                          (values :identity-policy
                                  core-state)))
                    (action
                     (lambda (core-state)
                       (execute-flow core-state
                                     :default
                                     'initialize-phase
                                     'entry/initialize-phase
                                     :come-from-state-name
                                     :ef-realize-phase)))
                    (transition active-phase))

        (flow-state active-phase :reset ()
                    (selector
                        (lambda (core-state)
                          (values :identity-policy
                                  core-state)))
                    (action
                     (lambda (core-state)
                       (execute-flow core-state
                                     :default
                                     'active-phase
                                     'entry/active-phase
                                     :come-from-state-name
                                     :ef-active-phase)))
                    (transition destroy-phase))

        (flow-state destroy-phase :reset ()
                    (selector
                        (lambda (core-state)
                          (values :identity-policy
                                  core-state)))
                    (action
                     (lambda (core-state)
                       (execute-flow core-state
                                     :default
                                     'destroy-phase
                                     'entry/destroy-phase
                                     :come-from-state-name
                                     :ef-destroy-phase)))
                    (transition maintenance-phase))

        (flow-state maintenance-phase :reset ()
                    (selector
                        (lambda (core-state)
                          (values :identity-policy
                                  core-state)))
                    (action
                     (lambda (core-state)
                       (execute-flow core-state
                                     :default
                                     'maintenance-phase
                                     'entry/maintenance-phase
                                     :come-from-state-name
                                     :ef-maintenance-phase)))
                    (transition exit/do-next-frame))

        (flow-state exit/do-next-frame :reset ()
                    (selector nil)
                    (action nil)
                    (transition nil))

        (flow-state exit/game-over :reset ()
                    (selector nil)
                    (action nil)
                    (transition nil))))