(in-package :%first-light)

(defmacro with-continue-restart (report &body body)
  `(restart-case (progn ,@body)
     (continue () :report ,report)))

#+(or slynk swank)
(macrolet ((install-repl-support ()
             (let ((repl-package (car (intersection '(:swank :slynk) *features*))))
               `(progn
                  (defun find-lisp-repl ()
                    (when ,repl-package
                      (load-time-value
                       (or ,(fl.util:ensure-symbol "*EMACS-CONNECTION*" repl-package)
                           (,(fl.util:ensure-symbol "DEFAULT-CONNECTION" repl-package))))))
                  (defun setup-lisp-repl ()
                    (if ,(eq repl-package :slynk)
                        (,(fl.util:ensure-symbol "SEND-PROMPT" "SLYNK-MREPL")
                         (find (bt:current-thread)
                               (,(fl.util:ensure-symbol "CHANNELS" repl-package))
                               :key #',(fl.util:ensure-symbol "CHANNEL-THREAD" repl-package)))
                        (fl.util:noop)))
                  (defun update-lisp-repl ()
                    (if ,repl-package
                        (fl.util:when-let ((repl (find-lisp-repl)))
                          (with-continue-restart "REPL"
                            (,(fl.util:ensure-symbol "HANDLE-REQUESTS" repl-package) repl t)))
                        (fl.util:noop)))))))
  (install-repl-support))

#-(or slynk swank)
(progn
  (defun setup-lisp-repl ()
    (fl.util:noop))
  (defun update-lisp-repl ()
    (fl.util:noop)))

(defgeneric destroy (thing &key ttl)
  (:documentation "Destroy may take either an ACTOR or a COMPONENT. The keyword argument :TTL
supplied in real seconds, how long the thing has yet to live."))

(defun type-table (key type-table)
  (fl.util:href type-table key))

(defun (setf type-table) (entry type-name-key type-table)
  (symbol-macrolet ((entry-table (fl.util:href type-table type-name-key)))
    (unless (nth-value 1 entry-table)
      (setf entry-table (fl.util:dict #'eq)))
    (setf (fl.util:href entry-table entry) entry)))

(defun type-table-drop (component component-type type-table)
  (remhash component (type-table component-type type-table)))

(defun eql/package-relaxed (obj1 obj2)
  (cond
    ((eql obj1 obj2) t) ; It succeeded? Oh good. Return quickly.
    ((and (symbolp obj1) (symbolp obj2)) ; Otherwise do a slower check.
     (string= (symbol-name obj1)
              (symbol-name obj2)))
    (t ; Hrm, sorry. It didn't EQL match,
     nil)))

(defun ensure-nested-hash-table (ht test-fn-list key-list)
  "Walk down the nested hash table HT ensuring that we have the correct number of hash tables
made (one less than the KEY-LIST set of keys) with the correct tests, gotten from TEST-FN-LIST.
NOTE: The first entry in TEST-FN-LIST is the test function for HT itself."

  ;; TODO: This looks painful for performance., oh well, we'll see if the profiler actually cares or
  ;; not. It is likely that these won't be nested deeply. Also, the algorithm is slightly painful,
  ;; but the deal is that we can't make a hash table containing the last key, since the last key is
  ;; where we'll either look something up or store it.
  (loop :with keylen = (length key-list)
        :for test-fn :in (cdr test-fn-list)
        :for key :in key-list
        :for i :below keylen
        :for lastp = (= i (1- keylen))
        :with current-ht = ht
        :do
           (unless (nth-value 1 (gethash key current-ht))
             ;; If the key doesn't exist, we make a new hash table and store it at the key UNLESS it
             ;; is the last entry, in which case we do nothing.
             (unless lastp
               (setf (gethash key current-ht)
                     (fl.util:dict (fdefinition test-fn)))))

           ;; The key is potentially newly minted.
           (setf current-ht (gethash key current-ht)))
  ht)

(fl.util:define-constant +sampler-type->texture-type+
    (fl.util:dict #'eq
                  :sampler-1d :texture-1d
                  :isampler-1d :texture-1d
                  :usampler-1d :texture-1d

                  :sampler-2d :texture-2d
                  :isampler-2d :texture-2d
                  :usampler-2d :texture-2d

                  :sampler-3d :texture-3d
                  :isampler-3d :texture-3d
                  :usampler-3d :texture-3d

                  :sampler-cube :texture-cube-map
                  :isampler-cube :texture-cube-map
                  :usampler-cube :texture-cube-map

                  :sampler-2d-rect :texture-rectangle
                  :isampler-2d-rect :texture-rectangle
                  :usampler-2d-rect :texture-rectangle

                  :sampler-1d-array :texture-1d-array
                  :isampler-1d-array :texture-1d-array
                  :usampler-1d-array :texture-1d-array

                  :sampler-2d-array :texture-2d-array
                  :isampler-2d-array :texture-2d-array
                  :usampler-2d-array :texture-2d-array

                  :sampler-cube-array :texture-cube-map-array
                  :isampler-cube-array :texture-cube-map-array
                  :usampler-cube-array :texture-cube-map-array

                  :sampler-buffer :texture-buffer
                  :isampler-buffer :texture-buffer
                  :usampler-buffer :texture-buffer

                  :sampler-2d-ms :texture-2d-multisample
                  :isampler-2d-ms :texture-2d-multisample
                  :usampler-2d-ms :texture-2d-multisample

                  :sampler-2d-ms-array :texture-2d-multisample-array
                  :isampler-2d-ms-array :texture-2d-multisample-array
                  :usampler-2d-ms-array :texture-2d-multisample-array)
  :test #'equalp
  :documentation
  "This variable is a hash table to map sampler types to texture types. It is a constant and will
never be changed at runtime.")

(fl.util:define-constant +cube-map-face->texture-type+
    (fl.util:dict #'eq
                  ;; Common sense shortcut name to real layer name.
                  :+x '(:texture-cube-map-positive-x 0)
                  :-x '(:texture-cube-map-negative-x 1)
                  :+y '(:texture-cube-map-positive-y 2)
                  :-y '(:texture-cube-map-negative-y 3)
                  :+z '(:texture-cube-map-positive-z 4)
                  :-z '(:texture-cube-map-negative-z 5)
                  ;; WYSIWYG shortcut name to real layer name
                  :right '(:texture-cube-map-positive-x 0)
                  :left '(:texture-cube-map-negative-x 1)
                  :top '(:texture-cube-map-positive-y 2)
                  :bottom '(:texture-cube-map-negative-y 3)
                  :back '(:texture-cube-map-positive-z 4)
                  :front '(:texture-cube-map-negative-z 5))
  :test #'equalp
  :documentation
  "This variable converts between an easy human (semantic) name for a cube
 map face to the complicated opengl name.")

(defun canonicalize-cube-map-face-signfier (face-signifier)
  (first (fl.util:href +cube-map-face->texture-type+ face-signifier)))

(defun sort-cube-map-faces-func (left right &key (test #'<))
  (let ((l (fl.util:href +cube-map-face->texture-type+ left))
        (r (fl.util:href +cube-map-face->texture-type+ right)))
    (funcall test (second l) (second r))))

;; TODO: This function is not entirely correct in that it won't copy structures or CLOS instances.
;; This need fixing. I would guess this is actually hard to do generally. WHen it becomes a problem
;; we'll deal with it then.
(defun copy-thing (thing)
  (if (typep thing 'sequence)
      (map-into (copy-seq thing) #'copy-thing thing)
      thing))

;;; Recompilation queue

(defun recompile-queued-items (core-state)
  (loop :with queue = (recompilation-queue core-state)
        :for ((kind data) found-p) = (multiple-value-list (fl.dst:qpop queue))
        :while found-p
        :do (ecase kind
              (:shader-recompilation (fl.shader:recompile-shaders data)))))
