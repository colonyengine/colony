(in-package :%first-light)

;;;; This file handles the framework for querying actors and components by the
;;;; different ID types, such is ID, UUID, and DISPLAY-ID.

;;; Class definition
;;; This is a superclass that can be added to any object to bring in slots for
;;; identifying the object with one of the various types of identification
;;; types. Currently, this is applied to actors and components.

(defclass queryable ()
  ((%id :reader id
        :initarg :id
        :initform nil)
   (%uuid :reader uuid
          :initform (make-uuid))
   (%display-id :accessor display-id
                :initarg :display-id
                :initform "No name")))

(au:define-printer (queryable stream)
  (format stream "~a" (display-id queryable)))

;;; Query table management

(defun register-object-uuid (object)
  (au:if-let ((table (objects-by-uuid (tables (core (context object)))))
              (uuid (uuid object)))
    (symbol-macrolet ((found (au:href table uuid)))
      (au:if-found (found (au:href table uuid))
                   (error "A UUID collision occured between the following ~
                           objects:~%~s~%~s."
                          found object)
                   (setf (au:href table uuid) object)))
    (error "Object ~s has no UUID. This is a bug and should be reported."
           object)))

(defmethod register-object-id ((object actor))
  (au:when-let ((table (actors-by-id (tables (core (context object)))))
                (id (id object)))
    (unless (au:href table id)
      (setf (au:href table id) (au:dict #'eq)))
    (setf (au:href table id object) object)))

(defmethod register-object-id ((object component))
  (au:when-let ((table (components-by-id (tables (core (context object)))))
                (id (id object)))
    (unless (au:href table id)
      (setf (au:href table id) (au:dict #'eq)))
    (setf (au:href table id object) object)))

(defun deregister-object-uuid (object)
  (remhash (uuid object)
           (objects-by-uuid (tables (core (context object))))))

(defmethod deregister-object-id ((self actor))
  (au:when-let ((table (actors-by-id (tables (core (context self)))))
                (id (id self)))
    (symbol-macrolet ((actors (au:href table id)))
      (remhash self actors)
      (unless (plusp (hash-table-count actors))
        (remhash id table)))))

(defmethod deregister-object-id ((self component))
  (au:when-let ((table (components-by-id (tables (core (context self)))))
                (id (id self)))
    (symbol-macrolet ((components (au:href table id)))
      (remhash self components)
      (unless (plusp (hash-table-count components))
        (remhash id table)))))

;;; Public API

(defmethod (setf id) (value (object queryable))
  "Change the ID of a queryable object."
  (with-slots (%id) object
    (when %id
      (deregister-object-id object))
    (setf %id value)
    (register-object-id object)))

(defun find-by-uuid (context uuid)
  "Return the object instance with the given `UUID` as a string."
  (let ((table (objects-by-uuid (tables (core context)))))
    (au:href table uuid)))

(defun find-actors-by-id (context id)
  "Return a list of all actor instances with the given `ID`."
  (au:when-let* ((table (actors-by-id (tables (core context))))
                 (by-id (au:href table id)))
    (au:hash-values by-id)))

(defun find-components-by-id (context id)
  "Return a list of all component instances with the given `ID`."
  (au:when-let* ((table (components-by-id (tables (core context))))
                 (by-id (au:href table id)))
    (au:hash-values by-id)))
