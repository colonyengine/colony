(in-package :gear)

(defun read-spec-forms (file)
  ;; Loop over every top-level form of FILE, until we reach EOF, collecting each
  ;; into a list.
  (let ((*package* (find-package :gear)))
    (with-open-file (in file)
      (loop :for form = (read in nil in)
            :until (eq form in)
            :collect form))))

(defun parse-scene-form (scene-spec)
  (destructuring-bind (actor components . children) (car scene-spec)
    (list actor components children)))

(defun collect-actor-symbols (scene-spec)
  (remove-duplicates (append
                      '(@universe)
                      (remove-if
                       (lambda (x) (or (not (symbolp x))
                                       (not (eq (char (symbol-name x) 0) #\@))))
                       (flatten scene-spec)))))

;; This does double duty, it builds a list of components for each actor
;; which includes the initform for that component. So, using first & cdr
;; you can get all the info you need for each component for each actor.
(defun collect-actor-component-info (form &optional (ht (make-hash-table)))
  ;; First, collect all types/initforms for each actor at this level.
  (loop :for actor :in form
        :do
           (let ((component-types (reverse (second actor))))
             (loop :for comp-type :in component-types
                   :do (push comp-type (gethash (first actor) ht)))))
  ;; Then, for each child of each actor, go get more.
  (loop :for actor :in form
        :do (collect-actor-component-info (cddr actor) ht))
  ht)

;; Produce the names for the variables holding component thunk lists
;; for that actor.
(defun gen-actor-symbol-thunk-vars (actor-names)
  (mapcar (lambda (an)
            (symbolicate an "-INITIALIZE-COMPONENTS-" (gensym "LIST")))
          actor-names))

;; Collect the data and produce the form.
(defun parse-scene (dsl)
  (let* ((actor-names (collect-actor-symbols form))
         (actor-name-thunk-vars (gen-actor-symbol-thunk-vars actor-names))
         (actor-component-info (collect-actor-component-info dsl))
         )
    nil))


(defmacro read-scene-file (file)
  `(let ((spec (read-spec-forms ,file)))
     ;; Here, we first rotate the spec forms forward, in order to make the last
     ;; form (the scene definition) be the first form instead. Logically, it
     ;; makes sense for the DSL to define the other forms first, and the scene
     ;; form last which references the other forms. However, for parsing, the
     ;; opposite is true, because a lambda list cannot bind a list of all but
     ;; the last form to a variable.

     ;; Finally, we bind the first form as SCENE and all the rest of the forms
     ;; as FORMS.
     (destructuring-bind (scene . forms) (rotate spec 1)
       `(progn
          ;; Emit all regular forms verbatim - this is lisp code intended to be
          ;; evaluated.
          ,@forms
          ;; Expand the scene form into our intended code.
          ,(lambda (core-state)
             (declare (ignore core-state))
             (let ((actors (make-hash-table)))
               (dolist (name (collect-actor-symbols scene))
                 (setf (gethash name actors)
                       (make-instance 'gear:actor :id name :state :initialize)))
               ;; TODO: finish expansion
               ))))))
