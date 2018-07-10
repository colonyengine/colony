(in-package :%fl)

(defclass component ()
  ((%context :reader context
             :initarg :context
             :initform nil)
   (%type :reader component-type
          :initarg :type)
   (%state :accessor state
           :initarg :state
           :initform :initialize)
   (%actor :accessor actor
           :initarg :actor
           :initform nil)
   (%ttl :accessor ttl
         :initarg :ttl
         :initform 0)
   (%initializer-thunk :accessor initializer-thunk
                       :initarg :initializer-thunk
                       :initform nil)))

(defun %generate-component-slot-forms (slots)
  (loop :for slot :in slots
        :collect
        (destructuring-bind (slot-name &key default allocation type &allow-other-keys) slot
          (append
           `(,(au:symbolicate '% slot-name)
             :accessor ,slot-name
             :initarg ,(au:make-keyword slot-name)
             :initform ,default)
           (when type
             `(:type ,type))
           (when allocation
             `(:allocation ,allocation))))))

(defmacro define-component (name super-classes &body body)
  (let* (;; NOTE: Can't use destructuring-bind because these forms might
         ;; not actually be present in the form so there would be a mismatch
         ;; or arguments for destructuring. We rely on the fact these accessors
         ;; return NIL for things beyond the end of the body.
         (slots (first body))
         (shared-storage-metadata (second body)))
    `(progn
       ;; It is ok if COMPONENT is added more than once in the heirarchy
       ;; because since it is the same type, there will only be one
       ;; version of it in the type heirarchy for the resultant class.
       (defclass ,name (,@(append '(component) super-classes))
         ,(%generate-component-slot-forms slots))

       ;; Create a method which provides the shared-storage-metadata for this
       ;; component-name.
       ;;
       ;; TODO: It is possible to make an individual function concerning this
       ;; concept like shared-storage-metadata/NAME. It would provide a
       ;; non-expensive function to get the data, but one for each
       ;; component. So the user API is a bit wonky. However, the actual game
       ;; dev probably wouldn't see this function too often. Still, pacakges
       ;; make it interesting....
       (defmethod ,(intern (symbol-name 'shared-storage-metadata) :%fl)
           ((component-name (eql ',name)) &optional namespace)

         (declare (ignore component-name))
         ;; TODO: for now, a hack, but make this more efficient in what it
         ;; returns. Namely, a static hash table or something related to the
         ;; data so it is easier to look up.
         (let ((ss-meta ',shared-storage-metadata))
           (if namespace
               ;; TODO: make this better.
               (find namespace ss-meta :key #'first)
               ss-meta))))))
