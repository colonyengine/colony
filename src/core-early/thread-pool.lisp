(in-package #:virality.thread-pool)

;; Implementation of THREAD-POOL

(defmacro %with-thread-kernel ((thread-pool) &body body)
  `(let ((lparallel:*kernel* (kernel ,thread-pool)))
     ,@body))

(defun make-thread-pool (worker-count)
  (make-instance 'thread-pool
                 :worker-count worker-count
                 :kernel (lparallel:make-kernel worker-count)))

(defun destroy-thread-pool (thread-pool)
  (%with-thread-kernel (thread-pool)
    (lparallel:end-kernel)))

(defun ensure-channel (thread-pool purpose)
  (%with-thread-kernel (thread-pool)
    (let ((channels (channels thread-pool)))
      (u:ensure-gethash purpose channels (lparallel:make-channel)))))

(defun ensure-queue (thread-pool purpose)
  (%with-thread-kernel (thread-pool)
    (let ((queues (queues thread-pool)))
      (u:ensure-gethash purpose queues (lparallel.queue:make-queue)))))

(defun submit-job (thread-pool purpose job &optional (priority :default))
  (%with-thread-kernel (thread-pool)
    (let ((channel (ensure-channel thread-pool purpose))
          (lparallel:*task-priority* priority)
          (lparallel:*task-category* purpose))
      (lparallel:submit-task channel job))))

(defun get-job-results (thread-pool purpose)
  (%with-thread-kernel (thread-pool)
    (let ((channel (ensure-channel thread-pool purpose)))
      (lparallel:receive-result channel))))

(defun kill-jobs (thread-pool purpose)
  (%with-thread-kernel (thread-pool)
    (lparallel:kill-tasks purpose)))

(defun enqueue (thread-pool purpose data)
  (let ((queue (ensure-queue thread-pool purpose)))
    (%with-thread-kernel (thread-pool)
      (lparallel.queue:push-queue data queue))))

(defun dequeue (thread-pool purpose)
  (let ((queue (ensure-queue thread-pool purpose)))
    (%with-thread-kernel (thread-pool)
      (lparallel.queue:pop-queue queue))))

(defun queue-empty-p (thread-pool purpose)
  (let ((queue (ensure-queue thread-pool purpose)))
    (%with-thread-kernel (thread-pool)
      (lparallel.queue:queue-empty-p queue))))

;;; TODO: The below functions are not fully baked yet. The live recompilation
;;; queue needs some work to make it more generic and easier to work with. See
;;; how I did it in Pyx when it comes time to fix this later. ~axion 4/8/2020

(defun push-queue (thread-pool purpose data)
  (let ((queue (ensure-queue thread-pool purpose)))
    (%with-thread-kernel (thread-pool)
      (lparallel.queue:push-queue data queue))))

(defun pop-queue (thread-pool purpose)
  (let ((queue (ensure-queue thread-pool purpose)))
    (%with-thread-kernel (thread-pool)
      (unless (lparallel.queue:queue-empty-p queue)
        (let ((result (lparallel.queue:pop-queue queue)))
          (values result t))))))

(defun process-queue (thread-pool purpose)
  (u:while (not (queue-empty-p thread-pool purpose))
    (destructuring-bind (&optional event-type data)
        (dequeue thread-pool purpose)
      (when event-type
        (handle-queued-event purpose event-type data)))))

(defgeneric handle-queued-event (purpose event-type data))

(defmethod handle-queued-event (purpose event-type data)
  (error "Unhandled queue event type ~s for queue purpose ~a."
         event-type purpose))
