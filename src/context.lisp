(in-package :fl.core)

;; NOTE: This dynamic variable is only bound during the extension loading process and should not be
;; used at runtime. This is to allow extensions, such as scenes, access to context-specific
;; information, such as the initial display's resolution. Do _not_ use this variable at runtime.
(defvar *context*)

(defclass context ()
  ((%core-state :reader core-state
                :initarg :core-state)
   (%settings :reader settings
              :initform (au:dict #'eq))
   (%active-camera :accessor active-camera
                   :initform nil)
   (%shared-storage-table :reader shared-storage-table
                          :initform (au:dict #'eq))
   (%state :accessor state
           :initarg :state
           :initform NIL)))

(defun total-time (context)
  "Return the total time in seconds that the engine has been running."
  (au:when-let ((display (display (core-state context))))
    (box.frame:total-time display)))

(defun frame-time (context)
  "Return the amount of time in seconds of the last frame as a REAL."
  (box.frame:frame-time (display (core-state context))))

(defun delta (context)
  "Return the physics update delta. This is :delta from the cfg file."
  (cfg context :delta))

(defun debug-p (context)
  (eq (cfg context :log-level) :debug))

;; These functions can use qualify-component. That'll be magic.
(defun ss-href (context component-name namespace &rest keys)
  (let* ((qualified-component-name (qualify-component (core-state context) component-name))
         (metadata-ht-test-fns (cdr (shared-storage-metadata qualified-component-name namespace))))
    (when (null metadata-ht-test-fns)
      (error "Shared storage namespace ~S~%does not exist for component ~S~%in package ~A"
             namespace
             component-name
             (if qualified-component-name
                 (package-name (symbol-package qualified-component-name))
                 "[no package: component does not exist!]")))
    (assert (= (length metadata-ht-test-fns) (length keys)))
    (ensure-nested-hash-table (shared-storage-table context)
                              (list* 'eq 'eql metadata-ht-test-fns)
                              (list* qualified-component-name namespace keys))
    ;; How, we can just do the lookup
    (apply #'au:href
           (shared-storage-table context)
           (list* qualified-component-name namespace keys))))

(defun (setf ss-href) (new-value context component-name namespace &rest keys)
  (let* ((qualified-component-name (qualify-component (core-state context) component-name))
         (metadata-ht-test-fns (cdr (shared-storage-metadata qualified-component-name namespace))))
    (assert (= (length metadata-ht-test-fns) (length keys)))
    (ensure-nested-hash-table (shared-storage-table context)
                              (list* 'eq 'eql metadata-ht-test-fns)
                              (list* qualified-component-name namespace keys))
    ;; How, we can perform the setting.
    (apply #'(setf au:href)
           new-value
           (shared-storage-table context)
           (list* qualified-component-name namespace keys))))

(defun %lookup-form-to-bindings (lookup-form)
  "Convert a LOOKUP-FORM with form (a b c ... z) to a set of gensymed bindings like ((#:G0 a) (#:G1
b) (#:G2 c) ... (#:G26 z)) and return it."
  (mapcar
   (lambda (element)
     `(,(gensym) ,element))
   lookup-form))

(defun %generate-ss-get/set (context bindings body)
  "While there are bindings to perform, strip one off, build a lexical environemt for it that will
set it into the shared-storatge in CONTEXT, and keep expanding with further bindings. When done,
emit the body in the final and most dense lexical scope."
  (if (null bindings)
      `(progn ,@body)
      (destructuring-bind (lexical-var presentp-var lookup-form
                           cache-value-form)
          (first bindings)

        (let* ((lookup-binding-forms (%lookup-form-to-bindings lookup-form))
               (lookup-args (mapcar #'first lookup-binding-forms)))
          `(let ,lookup-binding-forms
             (au:mvlet ((,lexical-var ,presentp-var
                                      (ss-href ,context ,@lookup-args)))
               (unless ,presentp-var
                 (setf ,lexical-var ,cache-value-form
                       (ss-href ,context ,@lookup-args) ,lexical-var))
               ,(%generate-ss-get/set context (rest bindings) body)))))))

(defmacro with-shared-storage ((context-var context-form) cache-bindings &body body)
  "Short Form for shared storage access."
  `(let ((,context-var ,context-form))
     ,(%generate-ss-get/set context-var cache-bindings body)))
