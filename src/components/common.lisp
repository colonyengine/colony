(in-package #:fl.comp-common)

;;;; Utilities used for components.

;; Used for writing components.
(defmacro define-shared-storage (component-name
                                 (&rest slots)
                                 (&rest key-vars)
                                 &optional (package t))
  "Define a Shared Storage mechanism for COMPONENT-NAME. Useful for components.
Creates class <COMPONENT-NAME>-SHARED-STORAGE.
Creates class <COMPONENT-NAME>-SHARED-STORAGE-ENTRY.
  The SLOTS of this class are specified as slots of in this class. They will
  have :reader methods on them only.
Creates maker function MAKE-<COMPONENT-NAME>-SHARED-STORAGE-ENTRY.
  This function requires the initial value of ALL SLOTS to be passed to it.
Creates storage lookup function <COMPONENT-NAME>-SHARED-STORAGE-ENTRY.
  TODO: Describe this function's arguments better.
Creates storage setter function (setf <COMPONENT-NAME>-SHARED-STORAGE-ENTRY).
  TODO: Describe this function's arguments better.

  You can optionally supply a PACKAGE into which the generated symbol will be
  interned. By default it is the reasd package, but we recommend specifying
  the actual component package as a keyword in which this form is used."
  (let* ((shared-storage-class-symbol
           (alexandria:format-symbol package "~A-SHARED-STORAGE" component-name))
         (shared-storage-entry-class-symbol
           (alexandria:format-symbol package "~A-SHARED-STORAGE-ENTRY" component-name))
         (entry-slots
           (mapcar (lambda (slot)
                     `(,(alexandria:format-symbol package "%~A" slot)
                       :reader ,slot
                       :initarg ,(intern (symbol-name slot) :keyword)))
                   slots))
         (make-instance-slots-assignments
           (mapcan (lambda (slot)
                     `(,(intern (symbol-name slot) :keyword) ,slot))
                   slots))
         (make-function-symbol
           (alexandria:format-symbol package "MAKE-~A-SHARED-STORAGE-ENTRY"
                                     component-name))
         (cache-slot-hidden
           (alexandria:format-symbol package "%CACHE"))
         (cache-slot
           (alexandria:format-symbol package "CACHE"))
         (lookup-function-symbol shared-storage-entry-class-symbol)
         (store-var (gensym "STORE"))
         (entry-var (gensym "ENTRY")))

    `(progn
       (defclass ,shared-storage-class-symbol ()
         ((,cache-slot-hidden :accessor ,cache-slot
                              :initform (make-hash-table :test #'equalp))))

       (defclass ,shared-storage-entry-class-symbol ()
         ,entry-slots)

       (defun ,make-function-symbol ,slots
         (make-instance ',shared-storage-entry-class-symbol
                        ,@make-instance-slots-assignments))

       (defun ,lookup-function-symbol (,store-var ,@key-vars)
         (gethash (list ,@key-vars) (,cache-slot ,store-var)))

       (defun (setf ,lookup-function-symbol) (,entry-var ,store-var ,@key-vars)
         (setf (gethash (list ,@key-vars) (,cache-slot ,store-var))
               ,entry-var)))))


(defmacro with-shared-storage
    ;; the symbol name of the component for which we are use the shared storage
    ((component-name
      ;; a form to get the core-stsate storage location from context & component
      ;; and store it in the store-var, which is available in body.
      (store-var storage-place-retrieval-form)
      ;; the symbol containing the found/produced cached entry
      cached-entry-var
      ;; The form that represents the key-values to ref/setf the entry.
      ;; The bindings cannot be changed for these key-vars. So, if you
      ;; lookup something with one set of values for key-vars, it will be
      ;; exactly those values used when storing the value.
      key-vars-values-form
      ;; The form which produces all slot data (in order required)
      ;; to insert into the cached entry.
      generate-cache-values-form
      &optional (package t))

     ;; The body form, store and cached-entry-var is valid here and the slots
     ;; filled in.  User can use it for further initialization.
     &body body)
  "This macro provides an environment to get/create/store cached values for
things in shared storage associated directly with all instances of the
COMPONENT-NAME.

  COMPONENT-NAME must be a defined component name, it can be package qualified.

  STORE-VAR is a symbol you define as a lexical variable in the BODY that is the
  symbol-macro of the STORAGE-PLACE-RETRIEVAL-FORM.

  The STORAGE-PLACE-RETRIEVAL-FORM is a form that gets the shared storage
  location for the component instance. It is usually something like:
  (shared-storage context component)
  when using standard idiomatic naming in the compponent protocol code.

  CACHED-ENTRY-VAR is a symbol you define as a lexical variable in the BODY
  which will hold the value of either the looked up cache entry, or the newly
  created and stored cached entry.

  KEY-VARS-VALUES-FORM is a single form that computes a VALUES form that
  returns, in the order specified in the CRITICAL section below, the keys used
  to look up the cached entry in the shared storage.

  GENERATE-CACHE-VALUES-FORM is a single form that conputes a VALUES form that
  are the values of what should be put into the cache entry. The order must
  match the CRITICAL section below.

  You can optionally supply a PACKAGE into which the generated symbol will be
  interned. By default it is the reasd package, but we recommend specifying
  the actual component package as a keyword in which this form is used.

  CRITICAL SECTION
  ----------------

  The two things to ENSURE YOU GET RIGHT are:

  The KEY-VARS-VALUES-FORM _MUST_ have the same order as the KEY-ARGS argument
  in the original DEFINE-SHARED-STORAGE form for the specified COMPONENT-NAME.

  The GENERATE-CACHE-VALUES-FORM _MUST_ have the same order as the SLOTS form in
  the original DEFINE-SHARED-STORAGE form for the specified COMPONENT-NAME."

  (let ((lookup-function
          (alexandria:format-symbol package "~A-SHARED-STORAGE-ENTRY"
                                    component-name))
        (make-function
          (alexandria:format-symbol package "MAKE-~A-SHARED-STORAGE-ENTRY"
                                    component-name))
        (shared-storage-class-symbol
          (alexandria:format-symbol package "~A-SHARED-STORAGE" component-name))
        (presentp (gensym "PRESENTP"))
        (new-entry (gensym "NEW-ENTRY"))
        (key-args (gensym "KEY-ARGS")))

    `(let (,cached-entry-var)
       (declare (ignore ,cached-entry-var))
       (symbol-macrolet ((,store-var ,storage-place-retrieval-form))
         ;; Create the storage class instance of required.
         (unless ,store-var
           (setf ,store-var
                 (make-instance ',shared-storage-class-symbol)))

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

           ,key-vars-values-form)))))
