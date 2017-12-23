(in-package :first-light-example)

(define-component spawn-destroy-test ()
  (spawn 0)
  (destroy-self nil))

(defmethod initialize-component
    ((component spawn-destroy-test) (context context))

  (format t "spawn-destroy-test[~A]: initialize-component invoked~%"
          component)

  ;; create an actor when tasked to do so
  (when (> (spawn component) 0)
    (format t "spawn-destroy-test[~A]: requested to spawn actor~%"
            component)
    (let ((new-actor (make-actor context :id '@spawn-destroy-test-new-actor)))
      (add-multiple-components new-actor
                               (list
                                (make-component 'transform context)
                                (make-component 'spawn-destroy-test context
                                                :destroy-self t)))

      ;; Add it to the universe? Hrm... how does that work?
      ;; How do I get this right by default?

      ;; put it into the game world.
      (spawn-actor new-actor context)

      (decf (spawn component)))))

(defmethod update-component ((component spawn-destroy-test) (context context))
  (format t "spawn-destroy-test[~A]: updating.~%" component)

  (when (destroy-self component)
    (format t "spawn-destroy-test[~A]: Spawned actor attempting to destroy itself!~%"
            component)
    (destroy (actor component) context)))

(defmethod destroy-component ((component spawn-destroy-test) (context context))
  (format t "spawn-destroy-test[~A]: destroyed!~%" component))
