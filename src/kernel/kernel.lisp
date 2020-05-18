(in-package #:virality)

(defclass kernel ()
  ((%core :reader core)
   (%context :reader context
             :initarg :context)
   (%state :accessor state
           :initarg :state
           :initform :initialize)
   (%id :reader id
        :initarg :id
        :initform nil)
   (%uuid :reader uuid
          :initform (make-uuid))
   (%display-id :accessor display-id
                :initarg :display-id
                :initform "Un-named kernel")
   (%ttl :accessor ttl
         :initarg :ttl
         :initform 0)))

;; NOTE: This ensures that all kernels have a core so we don't have to go
;; through the context to get it all over the codebase.
(defmethod initialize-instance :after ((instance kernel) &key)
  (setf (slot-value instance '%core) (core (context instance))))

(u:define-printer (kernel stream)
  (format stream "~a" (display-id kernel)))

;;; Query table management

(defun register-kernel-uuid (kernel)
  (u:if-let ((table (kernels-by-uuid (tables (core kernel))))
             (uuid (uuid kernel)))
    (symbol-macrolet ((found (u:href table uuid)))
      (u:if-found (found (u:href table uuid))
                  (error "A UUID collision occured between the following ~
                           kernels:~%~s~%~s."
                         found kernel)
                  (setf (u:href table uuid) kernel)))
    (error "Kernel ~s has no UUID. This is a bug and should be reported."
           kernel)))

(defmethod register-kernel-id ((kernel actor))
  (u:when-let ((table (actors-by-id (tables (core kernel))))
               (id (id kernel)))
    (unless (u:href table id)
      (setf (u:href table id) (u:dict #'eq)))
    (setf (u:href table id kernel) kernel)))

(defmethod register-kernel-id ((kernel component))
  (u:when-let ((table (components-by-id (tables (core kernel))))
               (id (id kernel)))
    (unless (u:href table id)
      (setf (u:href table id) (u:dict #'eq)))
    (setf (u:href table id kernel) kernel)))

(defun deregister-kernel-uuid (kernel)
  (remhash (uuid kernel) (kernels-by-uuid (tables (core kernel)))))

(defmethod deregister-kernel-id ((kernel actor))
  (u:when-let ((table (actors-by-id (tables (core kernel))))
               (id (id kernel)))
    (symbol-macrolet ((actors (u:href table id)))
      (remhash kernel actors)
      (unless (plusp (hash-table-count actors))
        (remhash id table)))))

(defmethod deregister-kernel-id ((kernel component))
  (u:when-let ((table (components-by-id (tables (core kernel))))
               (id (id kernel)))
    (symbol-macrolet ((components (u:href table id)))
      (remhash kernel components)
      (unless (plusp (hash-table-count components))
        (remhash id table)))))

;;; Public API

(defmethod (setf id) (value (kernel kernel))
  "Change the ID of a kernel object."
  (with-slots (%id) kernel
    (when %id
      (deregister-kernel-id kernel))
    (setf %id value)
    (register-kernel-id kernel)))

(defmethod find-by-uuid (context (uuid uuid))
  "Return the kernel instance with the given `UUID` object."
  (let ((table (kernels-by-uuid (tables (core context)))))
    (u:href table uuid)))

(defmethod find-by-uuid (context (uuid string))
  "Return the kernel instance with the given `UUID` string representation."
  (let ((table (kernels-by-uuid (tables (core context)))))
    (u:href table (string->uuid uuid))))

(defun find-actors-by-id (context id)
  "Return a list of all actor instances with the given `ID`."
  (u:when-let* ((table (actors-by-id (tables (core context))))
                (by-id (u:href table id)))
    (u:hash-values by-id)))

(defun find-components-by-id (context id)
  "Return a list of all component instances with the given `ID`."
  (u:when-let* ((table (components-by-id (tables (core context))))
                (by-id (u:href table id)))
    (u:hash-values by-id)))

;;; Protocol methods

(defmethod destroy ((kernel component) &key (ttl 0))
  (let ((table (u:href (component-predestroy-view (tables (core kernel))))))
    ;; TODO: Fix this. TTL is never nill so this won't do what we expect.
    (setf (ttl kernel) (and ttl (max 0 ttl)))
    (if ttl
        (setf (u:href table kernel) kernel)
        ;; If the TTL is stopped, we want to remove the component from the
        ;; pre-destroy view!
        (remhash kernel table))))

(defmethod destroy ((kernel actor) &key (ttl 0))
  (let* ((core (core kernel))
         (table (actor-predestroy-view (tables core))))
    (when (eq (id kernel) 'universe)
      (error "Cannot destroy the scene tree root."))
    ;; TODO: this needs fixing because TTL is never nil
    (setf (ttl kernel) (and ttl (max 0 ttl)))
    ;; TODO: Same for this
    (if ttl
        (setf (u:href table kernel) kernel)
        ;; If the TTL is stopped, we want to remove the actor from the
        ;; pre-destroy view!
        (remhash kernel table))))
