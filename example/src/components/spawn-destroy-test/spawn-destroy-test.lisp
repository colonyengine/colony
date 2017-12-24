(in-package :first-light-example)

(define-component spawn-destroy-test ()
  (spawn 0)
  (marked-destroying nil)
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

      ;; put it into the game world.
      (spawn-actor new-actor context)

      (decf (spawn component)))))

(defmethod update-component ((component spawn-destroy-test) (context context))
  (format t "spawn-destroy-test[~A]: updating.~%" component)

  (when (and (destroy-self component)
             (null (marked-destroying component)))

    (format t "spawn-destroy-test[~A]: Spawned actor attempting to destroy its component!~%"
            component)

    (destroy (actor component) context :ttl 2)

    (setf (marked-destroying component) t)))

(defmethod destroy-component ((component spawn-destroy-test) (context context))
  (format t "spawn-destroy-test[~A]: destroyed!~%" component))
