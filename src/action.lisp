(in-package #:%first-light)

(defclass action-manager ()
  ((%action-list :reader action-list
                 :initform (doubly-linked-list:make-dlist :test #'eq))
   (renderer :reader renderer
             :initarg :renderer)))

(defclass action ()
  ((%manager :reader manager
             :initarg :manager)
   (%node :accessor node)
   (%type :reader action-type
          :initarg :type)
   (%elapsed :accessor elapsed
             :initarg :elapsed
             :initform 0)
   (%duration :reader duration
              :initarg :duration
              :initform 1)
   (%finished-p :accessor finished-p
                :initarg :finished-p
                :initform nil)
   (%self-finishing-p :reader self-finishing-p
                      :initarg :self-finishing-p
                      :initform nil)
   (%blocking-p :reader blocking-p
                :initarg :blocking-p
                :initform nil)
   (%repeat-p :reader repeat-p
              :initarg :repeat-p
              :initform nil)
   (%shape :reader shape
           :initarg :shape
           :initform 'origin.shaping:linear)
   (%attrs :reader attrs
           :initarg :attrs
           :initform nil)))

(defmethod initialize-instance :after ((instance action) &key &allow-other-keys)
  (with-slots (%attrs) instance
    (setf %attrs (u:plist->hash %attrs :test #'eq))))

(defun insert-action (action where &key target)
  (with-slots (%manager %type) action
    (let* ((action-list (action-list %manager))
           (node (doubly-linked-list:insert-dlist-node
                  where action-list %type action :target-key target)))
      (setf (node action) node)
      (on-action-insert action %type)
      action)))

(defun remove-action (action)
  (with-slots (%manager %type) action
    (doubly-linked-list:remove-dlist-node (action-list %manager) %type)))

(defun replace-action (action type &rest args)
  (let ((action (apply #'reinitialize-instance action
                       :type type
                       :elapsed 0
                       :finished-p nil
                       args)))
    (doubly-linked-list:update-dlist-node-key (node action) type)))

(defun action-step (action)
  (with-slots (%shape %elapsed %duration) action
    (funcall %shape (a:clamp (/ %elapsed %duration) 0f0 1f0))))

(defun insert-default-actions (manager action-specs)
  (dolist (spec action-specs)
    (let ((action (apply #'make-instance 'action :manager manager spec)))
      (insert-action action :tail))))

(defun make-action-manager (renderer specs)
  (let ((manager (make-instance 'action-manager :renderer renderer)))
    (insert-default-actions manager specs)
    manager))

(defun process-actions (manager)
  (loop :with list = (doubly-linked-list:dlist-elements (action-list manager))
        :for (type . action) :in list
        :do (on-action-update action type)
        :when (finished-p action)
          :do (on-action-finish action type)
        :when (blocking-p action)
          :do (return)))

;;; Action event hooks

(defgeneric on-action-insert (action type)
  (:method (action type)))

(defgeneric on-action-finish (action type)
  (:method (action type))
  (:method :around (action type)
    (let ((actor (actor (renderer (manager action)))))
      (call-next-method)
      (log:trace :changeme "Action ~a finished for actor ~a."
                 type (id actor)))))

(defgeneric on-action-update (action type)
  (:method (action type))
  (:method :before (action type)
    (with-slots (%manager %elapsed %self-finishing-p %duration %finished-p)
        action
      (incf %elapsed (frame-time (context (renderer %manager))))
      (when (and (not %self-finishing-p)
                 (>= %elapsed %duration))
        (setf %finished-p t)))))
