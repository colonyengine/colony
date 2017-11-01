(in-package :gear)

(defun test-scene-load ()
  (let ((cs (make-core-state)))
    (prepare-scenes cs (get-path :gear-example "data"))
    (load-scene cs :demo)
    cs))

(defun test-scene-code ()
  (let ((cs (make-core-state)))
    (prepare-scenes cs (get-path :gear-example "data"))
    (data (get-scene cs :demo))))

(defun test-frames (&optional (num-frames 1))
  (let ((cs (test-scene-load)))
    (prepare-call-flows cs (get-path :gear-example "data"))
    (loop
      :repeat num-frames
      :do (execute-flow cs :default 'perform-one-frame
                        'ENTRY/INITIALIZE-PHASE
                        :come-from-state-name (gensym "EXEC-FLOW-")))
    cs))

(defun test-protocol-method-0 (inst cxt)
  (format t "TEST-PROTOCOL-METHOD-0 called: inst=~A cxt=~A~%" inst cxt))

(defun test-protocol-method-1 (inst cxt)
  (format t "TEST-PROTOCOL-METHOD-1 called: inst=~A cxt=~A~%" inst cxt))

(defun test-execute-flow (core-state call-flow-name flow-name flow-state-name
                          &optional (flow-init-state (gensym "EF-INIT-")))
  (execute-flow core-state call-flow-name flow-name flow-state-name
                :come-from-state-name flow-init-state))
