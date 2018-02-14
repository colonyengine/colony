(in-package :first-light-example)

(define-component spawn-destroy-test ()
  (spawn 0)
  (marked-destroying nil)
  (destroy-self nil))

(simple-logger:define-message :trace :comp.spawn-destroy-test.init
  "spawn-destroy-test: ~s: Initialize-component invoked.")

(simple-logger:define-message :trace :comp.spawn-destroy-test.spawn
  "spawn-destroy-test: ~s: Requested to spawn actor.")

(simple-logger:define-message :trace :comp.spawn-destroy-test.update
  "spawn-destroy-test: ~s: Updating.")

(simple-logger:define-message :trace :comp.spawn-destroy-test.destroying
  "spawn-destroy-test: ~s: Spawned actor attempting to destroy its component.")

(simple-logger:define-message :trace :comp.spawn-destroy-test.destroyed
  "spawn-destroy-test: ~s: Destroyed.")

(defmethod initialize-component
    ((component spawn-destroy-test) (context context))
  (simple-logger:emit :comp.spawn-destroy-test.init component)
  ;; create an actor when tasked to do so
  (when (plusp (spawn component))
    (simple-logger:emit :comp.spawn-destroy-test.spawn component)
    (let ((new-actor (make-actor context :id '@spawn-destroy-test-new-actor)))
      (add-multiple-components new-actor
                               (list
                                (make-component 'transform context)
                                (make-component 'spawn-destroy-test context
                                                :destroy-self t)))
      (spawn-actor new-actor context)
      (decf (spawn component)))))

(defmethod update-component ((component spawn-destroy-test) (context context))
  (simple-logger:emit :comp.spawn-destroy-test.update component)
  (when (and (destroy-self component)
             (null (marked-destroying component)))
    (simple-logger:emit :comp.spawn-destroy-test.destroying component)
    (destroy (actor component) context :ttl 2)
    (setf (marked-destroying component) t)))

(defmethod destroy-component ((component spawn-destroy-test) (context context))
  (simple-logger:emit :comp.spawn-destroy-test.destroyed component))
