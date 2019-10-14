(in-package #:virality.models)

;; TODO: In progress of sketching out this code. It does not function yet.

(defclass model-profile ()
  ((%name :reader name
          :initarg :name)
   (%attributes :reader attributes
                :initarg :attributes
                :initform (u:dict))))

(defclass model-descriptor ()
  ((%name :reader name
          :initarg :name)))

(defun parse-model-profile (name body-form)
  (declare (ignore name body-form))
  nil)

(defmacro define-model-profile (name &body body)
  (a:with-gensyms (profile)
    (let ((definition '(v::meta 'model-profiles)))
      `(let ((,profile ,(parse-model-profile name body)))
         (unless ,definition
           (setf ,definition (u:dict)))
         (setf (u:href ,definition (name ,profile)) ,profile)))))

(defun update-model/interactively (old-model-descriptor new-model-descriptor)
  (declare (ignore old-model-descriptor new-model-descriptor))
  nil)

(defmacro define-model (name (&rest profile-overlay-names) &body body)
  "Construct a semantic MODEL-DESCRIPTOR. "
  (a:with-gensyms (desc-lookup old-desc new-desc)
    (let ((definition '(v::meta 'models)))
      `(symbol-macrolet ((,desc-lookup (u:href ,definition ',name)))
         (unless ,definition
           (setf ,definition (u:dict)))
         (let ((,new-desc (make-model-descriptor
                           :name ',name
                           :profile-overlay-names ',profile-overlay-names))
               (,old-desc ,desc-lookup))
           ;; Record the parameters we'll overlay on the profile at use time.
           (setf ,@(loop :for (key value) :in body
                         :append `((u:href (attributes ,new-desc) ,key)
                                   ,value)))
           (setf ,desc-lookup ,new-desc)

           (update-model/interactively ,old-desc ,new-desc)

           (export ',name))))))

(defun load-model-descriptors (core)
  (declare (ignore core))
  nil)
