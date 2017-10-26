(in-package :gear)

(defclass flow-state ()
  (
   (%name :accessor name
          :initarg :name)
   (%policy :accessor policy ;; :reset only for now.
            :initarg :policy)
   (%exitingp :accessor exitingp
              :initarg :exitingp)

   ;; User supplied functions which represent the flow state itself.
   (%selector :accessor selector
              :initarg :selector)
   (%action :accessor action
            :initarg :action)
   (%transition :accessor transition
                :initarg :transition)

   ;; Helper functions to deal with the internal state of this flow state.
   (%reset :accessor reset
           :initarg :reset)))

(defun make-flow-state (&rest initargs)
  (apply #'make-instance 'flow-state  initargs))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; All this is used to parse the DSL
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun canonicalize-binding (binding)
  (if (symbolp binding)
      (list binding NIL)
      binding))

(defun binding-partition (bindings)
  (loop :for (b v) :in bindings
        :collect b :into bs
        :collect v :into vs
        :finally (return (values bs vs))))

(defun gen-reset-function (bind-syms bind-vals)
  `(lambda ()
     (setf
      ,@(mapcan (lambda (s v)
                  `(,s ,v))
                bind-syms
                bind-vals))))

(defun ensure-matched-symbol (sym name)
  (assert (string= (symbol-name sym) (string-upcase name))))

;; parse a single flow-state dsl form and return a form which creates
;; the flow-state CLOS instance for it.
(defun parse-flow-state (form)
  (let ((match (first form))
        (name (second form))
        (policy (third form))
        (bindings (mapcar #'canonicalize-binding (fourth form)))
        (selector (fifth form))
        (action (sixth form))
        (transition (seventh form)))
    (multiple-value-bind (bind-syms bind-vals)
        (binding-partition bindings)

      (ensure-matched-symbol match "flow-state")

      (let ((reset-function (gen-reset-function bind-syms bind-vals)))
        ;; Generate the instance maker for this flow-state.
        `(',name
          (let ,bindings ;; these are canonicalized, available for user.
            (make-flow-state :name ',name
                             :policy ,policy
                             :exitingp ,(and (null selector)
                                             (null action)
                                             (null transition))
                             :selector ,selector
                             :action ,action
                             :transition ,transition
                             :reset ,reset-function)))))))

;; Parse a single flow dsl node into a form that evalutes to a hash
;; table containing each flow-state indexed by name.
(defun parse-flow (form)
  (let* ((flow-ht (gensym))
         (match (first form))
         (flow-name (second form))
         (flow-states (cddr form))
         (processed-flow-state-forms
           (loop :for flow-state :in flow-states
                 :collect (parse-flow-state flow-state))))

    (ensure-matched-symbol match "flow")

    `(',flow-name
      (let ((,flow-ht (make-hash-table)))
        ,@(loop :for (n f) :in processed-flow-state-forms
                :collect `(setf (gethash ,n ,flow-ht) ,f))
        ,flow-ht))))

;; Parse an entire call-flow and return a list of the name of it and a
;; form which evaluates into a hash table of flows keyed by their
;; name.
(defun parse-call-flow (form)
  (let* ((call-flow-ht (gensym))
         (match (first form))
         (call-flow-name (second form))
         (flows (cddr form))
         (processed-flow-forms (loop :for flow :in flows
                                     :collect (parse-flow flow))))

    (ensure-matched-symbol match "call-flow")

    `(',call-flow-name
      (let ((,call-flow-ht (make-hash-table)))
        ,@(loop :for (n f) :in processed-flow-forms
                :collect `(setf (gethash ,n ,call-flow-ht) ,f))
        ,call-flow-ht))))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; End parsing DSL codes.
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun test-call-flow ()

  (let ((form
          ;; This is directly from the ORG doc.

          `(call-flow
            default
            ;; Hrm. This is all single dispatch, is that good? Is
            ;; there more opportunity for CL's strengths in here?

            ;; NOTE: If the functions inside of the state machine
            ;; internally recurse by returning the correct states, the
            ;; executor will recurse forever until something about a
            ;; state transition picks a different path.

            (flow actor-initialization-flow
                  (flow-state ENTRY :reset () ;; bindings in a let for
					      ;; the two functions.

                              ;; Select what I want to work on.
                              (lambda (core-state)
                                (actors-initialize-db core-state))

                              ;; This function is run for every instance
                              (lambda (core-state inst)
                                ;; a core function, not exposed to users.
                                (realize-actor inst (context core-state)))

                              ;; After all instances have been
                              ;; processed, this function is run once
                              ;; by the executor in order to choose
                              ;; the next state. The let form contains
                              ;; anything we need to store while
                              ;; running the instance function which
                              ;; may determine the state we go to.
                              (lambda (core-state)
                                EXIT/FLOW-FINISHED))

                  (flow-state EXIT/FLOW-FINISHED :reset ()
                              NIL))

            (flow component-logic-flow
                  (flow-state ENTRY/PHYSICS-UPDATE :reset ()
                              (lambda (core-state)
                                ;; Fix to use the type-flow structures.
                                (components-db core-state))

                              (lambda (core-state inst)
                                ;; this is the USER method they want to run at
                                ;; physics speed.
                                (physics-update inst (context core-state)))

                              (lambda (core-state)
                                EXIT/PHYSICS))

                  (flow-state EXIT/PHYSICS :reset ()
                              NIL)

                  (flow-state ENTRY/COLLISIONS :reset ()
                              (lambda (core-state)
                                ;; Fix to use the type-flow structures.
                                (components-db core-state))

                              (lambda (core-state inst)
                                ;; I don't know how this is working yet.
                                (perform-collide inst (context core-state)))

                              (lambda (core-state)
                                EXIT/COLLISIONS))

                  (flow-state EXIT/COLLISIONS :reset ()
                              NIL)

                  ;; Once looped physics/collisions are dealt with, we
                  ;; can do the rest of this flow properly.
                  (flow-state ENTRY/AFTER-PHYSICS :reset ()
                              (lambda (core-state)
                                ;; Fix to use the type-flow structures.
                                (components-db core-state))

                              (lambda (core-state inst)
                                (update inst (context core-state)))

                              (lambda (core-state)
                                RENDER))

                  (flow-state RENDER :reset ()
                              (lambda (core-state)
                                ;; Fix to use the type-flow structures.
                                (components-db core-state))

                              (lambda (core-state inst)
                                (render inst (context core-state)))
                              (lambda (core-state)
                                EXIT/FLOW-FINISHED))

                  (flow-state EXIT/FLOW-FINISHED :reset ()
                              NIL))

            (flow actor-maintenance-flow
                  (flow-state ENTRY :reset ()
                              (lambda (core-state)
                                (actors-db core-state))

                              (lambda (core-state inst)
                                (unless (eq (status inst) :alive)
                                  ;; This should mark all components as
                                  ;; dead and including the actor.
                                  ;; NOT a user facing API.
                                  (destroy-actor inst (context core-state))))

                              (lambda (core-state)
                                EXIT/FLOW-FINISHED))

                  (flow-state EXIT/FLOW-FIISHED :reset ()
                              NIL))

            (flow component-maintenance-flow
                  (flow-state ENTRY :reset ()
                              (lambda (core-state)
                                ;; Fix to use the type-flow structures.
                                (components-db core-state))

                              (lambda (core-state inst)
                                (unless (eq (status inst) :active)
                                  (destroy-component inst
						     (context core-state))))

                              (lambda (core-state)
                                EXIT/FLOW-FIISHED))

                  (flow-state EXIT/FLOW-FINISHED :reset ()
                              NIL))

            (flow frame-flow
                  ;; First spawn any actors (which may or may not be
                  ;; empty of components, but were created LAST frame
                  ;; and put into a staging area.
                  (flow-state ENTRY :reset ()
                              (lambda (core-state)
                                nil)

                              (lambda (core-state inst)
                                (execute-flow 'ENTRY
                                              (flow 'actor-initialization-flow
                                                    core-state)
                                              (actor-init-db core-state)))

                              (lambda (core-state)
                                INIT-COMPONENTS))

                  ;; Then initialize any components that need initializaing.
                  (flow-state INIT-COMPONENTS :reset ()
                              (lambda (core-state)
                                nil)

                              (lambda (core-state inst)
                                (execute-flow
				 'ENTRY
				 (flow 'component-initialization-flow
				       core-state)
				 (component-init-db core-state)))

                              (lambda (core-state)
                                UPDATE-COMPONENTS))

                  ;; Then run the component logic for all the components
                  (flow-state UPDATE-COMPONENTS :reset ()
                              (lambda (core-state)
                                nil)

                              (lambda (core-state inst)
                                ;; First, we run the physics and collision
                                ;; updates, maybe in a loop depending what is
                                ;; required.
                                (loop :with again = T
                                      :while again
                                      :do ;; First, run the User's physics
                                          ;; functions over all ordered
                                          ;; components.
                                          (execute-flow
                                           'ENTRY/PHYSICS-UPDATE
                                           (flow 'component-logic-flow
						 core-state)
                                           ;; Fix to use type-flow
                                           (component-db core-state))

                                          ;; Then, update ALL transforms to
                                          ;; current local/model

                                          ;; TODO: maybe wrap in box:tick?

                                          ;; TODO: pass the right stuff to get
                                          ;; universe root.

                                          (do-nodes #'transform-node)

                                          ;; Then, run any collisions
                                          ;; that may have happened
                                          ;; over ordered components.

                                          ;; TODO, exactly figure out
                                          ;; how to call collisions
                                          ;; with the right collidees
                                          ;; and such.
                                          (execute-flow
                                           'ENTRY/COLLISIONS
                                           (flow 'component-logic-flow
						 core-state)
                                           ;; Fix to use type-flow
                                           (component-db core-state))

                                          ;; Check to see if we're
                                          ;; done doing physics.
                                          (unless (physics-loop-required-p
						   core-state)
                                            (setf again NIL)))

                                ;; Then, complete the logic for the components.
                                (execute-flow 'ENTRY/AFTER-PHYSICS
                                              (flow 'component-logic-flow
						    core-state)
                                              (component-db core-state)))

                              (lambda (core-state)
                                ACTOR-MAINTENANCE))

                  ;; if game objects are marked destroeyd, then kill all
                  ;; components too.
                  (flow-state ACTOR-MAINTENANCE :reset ()
                              (lambda (core-state)
                                nil)

                              (lambda (core-state inst)
                                (execute-flow 'ENTRY
                                              (flow 'actor-maintenance-flow
						    core-state)
                                              (actor-db core-state)))
                              (lambda (core-state)
                                COMPONENT-MAINTENANCE))

                  ;; Then, any game objects that died, or other components
                  ;; previously marked as being destroyed get destroeyd.
                  (flow-state COMPONENT-MAINTENANCE :reset ()
                              (lambda (core-state)
                                nil)

                              (lambda (core-state inst)
                                (execute-flow 'ENTRY
                                              (flow 'component-maintenance-flow
                                                    core-state)
                                              (component-db core-state)))
                              (lambda (core-state)
                                CONTINUE/EXIT))

                  (flow-state CONTINUE/EXIT :reset ()
                              (lambda (core-state)
                                nil)

                              NIL ;; no flows to run!

                              (lambda (core-state)
                                (if (exitingp core-state)
                                    EXIT/GAME-OVER
                                    EXIT/DO-NEXT-FRAME)))

                  (flow-state EXIT/DO-NEXT-FRAME :reset ()
                              NIL)

                  (flow-state EXIT/GAME-OVER :reset ()
                              NIL)))



          ))

    (parse-call-flow form)))
