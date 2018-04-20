(in-package :fl.core)

(defgeneric destroy (thing context &key ttl)
  (:documentation "Destroy may take either an ACTOR or a COMPONENT. The keyword argument :TTL
supplied in real seconds, how long the thing has yet to live."))

(defun type-table (key type-table)
  (au:href type-table key))

(defun (setf type-table) (entry type-name-key type-table)
  (symbol-macrolet ((entry-table (au:href type-table type-name-key)))
    (unless (nth-value 1 entry-table)
      (setf entry-table (au:dict #'eq)))
    (setf (au:href entry-table entry) entry)))

(defun eql/package-relaxed (obj1 obj2)
  (cond
    ((eql obj1 obj2) t) ; It succeeded? Oh good. Return quickly.
    ((and (symbolp obj1) (symbolp obj2)) ; Otherwise do a slower check.
     (string= (symbol-name obj1)
              (symbol-name obj2)))
    (t ; Hrm, sorry. It didn't EQL match,
     nil)))

(defmacro with-shared-storage
    ((component-name
      store-var
      cached-entry-var
      key-vars-values-form
      generate-cache-values-form)
     &body body)
  "This macro provides an environment to get/create/store cached values for things in shared storage
associated directly with all instances of the COMPONENT-NAME.

COMPONENT-NAME must be a defined component name, it can be package qualified.

STORE-VAR is a symbol you define as a lexical variable in the BODY that is the symbol-macro of the
STORAGE-PLACE-RETRIEVAL-FORM.

CACHED-ENTRY-VAR is a symbol you define as a lexical variable in the BODY which will hold the value
of either the looked up cache entry, or the newly created and stored cached entry.

KEY-VARS-VALUES-FORM is a single form that computes a VALUES form that returns, in the order
specified in the CRITICAL section below, the keys used to look up the cached entry in the shared
storage.

GENERATE-CACHE-VALUES-FORM is a single form that computes a VALUES form that are the values of what
should be put into the cache entry. The order must match the CRITICAL section below.

KEY-VARS-VALUES-FORM _MUST_ have the same order as the slots with the :SHARED specifier in the
 specified component's DEFINE-COMPONENT form.

GENERATE-CACHE-VALUES-FORM _MUST_ have the same order as the slots with the :SHARED specifier in the
specified component's DEFINE-COMPONENT form."
  (let* ((class-symbol (au:symbolicate component-name '-shared-storage))
         (lookup-function (au:symbolicate class-symbol '-entry))
         (make-function (au:symbolicate 'make- lookup-function)))
    (au:with-unique-names (presentp new-entry key-args)
      `(let (,cached-entry-var)
         (declare (ignorable ,cached-entry-var))
         (symbol-macrolet ((,store-var (shared-storage context component)))
           ;; Create the storage class instance of required.
           (unless ,store-var
             (setf ,store-var (make-instance ',class-symbol)))
           ;; determine the ref/set variables for the cache entry.
           ;; This is a once-only, but done carefully wrt a values form.
           ;; It is like this, since we don't know how many key variables
           ;; we're going to have!
           (multiple-value-call
               (lambda (&rest ,key-args)
                 ;; First lookup to see if I already have a cache entry.
                 (multiple-value-bind (,cached-entry-var ,presentp)
                     (apply (function ,lookup-function) ,store-var ,key-args)
                   ;; if it isn't present, then create the cached entry and store
                   ;; it in the cache and fixup the cached-entry-var to hold the
                   ;; right value.
                   (unless ,presentp
                     ;; make a new cached entry with the
                     ;; generate-cache-values-form.
                     (let ((,new-entry
                             (multiple-value-call
                                 (lambda (&rest args)
                                   (apply (function ,make-function) args))
                               ,generate-cache-values-form)))
                       ;; Now insert the cached entry.  NOTE: We call it exactly
                       ;; as the setf function lambda list indicates!
                       (apply (function (setf ,lookup-function))
                              ,new-entry ,store-var ,key-args)
                       (setf ,cached-entry-var ,new-entry)))
                   ;; Then run the BODY with the bound ,cached-entry-var to
                   ;; comething real.
                   ,@body))
             ,key-vars-values-form))))))
