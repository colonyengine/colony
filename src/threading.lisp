(in-package #:virality.engine)

(defclass thread-pool ()
  ((%worker-count :reader worker-count
                  :initarg :worker-count)
   (%channels :reader channels
              :initform (u:dict #'eq))
   (%queues :reader queues
            :initform (u:dict #'eq))))

(defun make-thread-pool (core)
  (let* ((worker-count v:=threads=)
         (thread-pool (make-instance 'thread-pool :worker-count worker-count)))
    (setf lparallel:*kernel* (lparallel:make-kernel worker-count)
          (slot-value core '%thread-pool) thread-pool)
    thread-pool))

(defun ensure-channel (core purpose)
  (let ((channels (channels (thread-pool core))))
    (a:ensure-gethash purpose channels (lparallel:make-channel))))

(defun ensure-queue (core purpose)
  (let ((queues (queues (thread-pool core))))
    (a:ensure-gethash purpose queues (lparallel.queue:make-queue))))

(defun submit-job (core purpose job &optional (priority :high))
  (let ((channel (ensure-channel core purpose))
        (lparallel:*task-priority* priority))
    (lparallel:submit-task channel job)))

(defun get-job-results (core purpose)
  (let ((channel (ensure-channel core purpose)))
    (lparallel:receive-result channel)))

(defun push-queue (core purpose data)
  (let ((queue (ensure-queue core purpose)))
    (lparallel.queue:push-queue data queue)))

(defun pop-queue (core purpose)
  "Pops an item off of the queue designated by `PURPOSE`.
If the queue is not empty, return two values: the result, and T. If `FUNC` is
supplied, return the result of applying `FUNC` to the result for the first
return value."
  (let ((queue (ensure-queue core purpose)))
    (unless (lparallel.queue:queue-empty-p queue)
      (let ((result (lparallel.queue:pop-queue queue)))
        (values result
                t)))))
