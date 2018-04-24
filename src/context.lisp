(in-package :fl.core)

(defclass context ()
  ((%core-state :reader core-state
                :initarg :core-state)
   (%settings :reader settings
              :initform (au:dict #'eq))
   (%shaders :accessor shaders
             :initform nil)
   (%active-camera :accessor active-camera
                   :initform nil)
   (%shared-storage-table :reader shared-storage-table
                          :initform (au:dict #'eq))))

(defun frame-time (context)
  "Return the amount of time in seconds the last frame as a REAL."
  (box.frame:frame-time (display (core-state context))))

(defun delta (context)
  "Return the physics update delta. This is :delta from the cfg file."
  (cfg context :delta))


(defun %ensure-nested-hash-table (ht test-fn-list key-list)
  ;; Now, we walk down the nested hash tables ensuring that we have
  ;; the correct number of hash tables made with the correct tests.

  ;; TODO: This looks painful for performance., oh well, we'll see if the
  ;; profiler actually cares or not. It is likely that these won't be
  ;; nested deeply. Also, the algorithm is slightly painful, but the deal
  ;; is that we can't make a hash table containing the last key, since the
  ;; last key is where we'll either look something up or store it.
  (loop :with keylen = (length key-list)
        :for test-fn :in (cdr test-fn-list)
        :for key :in key-list
        :for i :below keylen
        :for lastp = (= i (1- keylen))
        :with current-ht = ht
        :do
           (unless (nth-value 1 (gethash key current-ht))
             ;; If the key doesn't exist, we make a new hash table
             ;; and store it at the key UNLESS it is the last entry,
             ;; in which case we do nothing.
             (unless lastp
               (setf (gethash key current-ht)
                     (au:dict (fdefinition test-fn)))))

           ;; The key is potentially newly minted.
           (setf current-ht (gethash key current-ht)))
  ht)


;; These functions can use qualify-component. That'll be magic.
(defun ss-href (context component-name namespace &rest keys)
  (let* ((qualified-component-name
           (qualify-component (core-state context) component-name))
         (metadata-ht-test-fns
           ;; Only need the actual functions.
           (cdr (shared-storage-metadata qualified-component-name namespace))))

    (when (null metadata-ht-test-fns)
      (error "Shared storage namespace ~S~%does not exist for component ~S~%in package ~A"
             namespace
             component-name
             (if qualified-component-name
                 (package-name (symbol-package qualified-component-name))
                 "[no package: component does not exist!]")))

    (assert (= (length metadata-ht-test-fns)
               (length keys)))

    (%ensure-nested-hash-table (shared-storage-table context)
                               (list* 'eq 'eql metadata-ht-test-fns)
                               (list* qualified-component-name
                                      namespace
                                      keys))
    ;; How, we can just do the lookup
    (apply #'au:href
           (shared-storage-table context)
           (list* qualified-component-name namespace keys))))


(defun (setf ss-href) (new-value context component-name namespace &rest keys)
  (let* ((qualified-component-name
           (qualify-component (core-state context) component-name))
         (metadata-ht-test-fns
           ;; Only need the actual functions.
           (cdr (shared-storage-metadata qualified-component-name namespace))))

    (assert (= (length metadata-ht-test-fns)
               (length keys)))

    (%ensure-nested-hash-table (shared-storage-table context)
                               (list* 'eq 'eql metadata-ht-test-fns)
                               (list* qualified-component-name
                                      namespace
                                      keys))
    ;; How, we can perform the setting.
    (apply #'(setf au:href) new-value
           (shared-storage-table context)
           (list* qualified-component-name namespace keys))))




(defun %lookup-form-to-bindings (lookup-form)
  "COnvert a LOOKUP-FORM with form (a b c ... z) to a set of gensymed bindings
like ((#:G0 a) (#:G1 b) (#:G2 c) ... (#:G26 z)) and return it."
  (mapcar (lambda (element)
            `(,(gensym) ,element))
          lookup-form))

(defun %generate-ss-get/set (context bindings body)
  "While there are bindings to perform, strip one off, build a lexical
environemt for it that will set it into the shared-storatge in CONTEXT, and keep
expanding with further bindings. When done, emit the body in the final and most
dense lexical scope."
  (if (null bindings)
      `(progn ,@body)
      (destructuring-bind (lexical-var lookup-form cache-value-form)
          (first bindings)
        (let* ((presentp (gensym "PRESENTP-"))
               (lookup-binding-forms (%lookup-form-to-bindings lookup-form))
               (lookup-args (mapcar #'first lookup-binding-forms)))
          `(let ,lookup-binding-forms
             (multiple-value-bind (,lexical-var ,presentp)
                 (ss-href ,context ,@lookup-args)
               (unless ,presentp
                 (setf ,lexical-var ,cache-value-form
                       (ss-href ,context ,@lookup-args) ,lexical-var))
               ,(%generate-ss-get/set context (rest bindings) body)))))))

(defmacro with-shared-storage ((context-var context-form)
                               cache-bindings &body body)
  "Short Form for shared storage access."
  `(let ((,context-var ,context-form))
     ,(%generate-ss-get/set context-var cache-bindings body)))
