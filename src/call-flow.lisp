(in-package :gear)

(defclass flow-state ()
  (
   (%name :accessor name
          :initarg :name)
   (%policy :accessor policy ;; :reset only for now.
            :initarg :policy)
   (%exitingp :accessor exitingp
              :initarg :exitingp)

   ;; User supplied functions which represent the flow state itself.
   (%selector :accessor selector
              :initarg :selector)
   (%action :accessor action
            :initarg :action)
   (%transition :accessor transition
                :initarg :transition)

   ;; Helper functions to deal with the internal state of this flow state.
   (%reset :accessor reset
           :initarg :reset)))

(defun make-flow-state (&rest initargs)
  (apply #'make-instance 'flow-state  initargs))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; All this is used to parse the DSL
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun canonicalize-binding (binding)
  (if (symbolp binding)
      (list binding NIL)
      binding))

(defun binding-partition (bindings)
  (loop :for (b v) :in bindings
        :collect b :into bs
        :collect v :into vs
        :finally (return (values bs vs))))

;; TODO: this should return a let over lambda to do a once only of the bind-vals
;; evaluation. Needs fixing.
(defun gen-reset-function (bind-syms bind-vals)
  `(lambda ()
     (setf
      ,@(mapcan (lambda (s v)
                  `(,s ,v))
                bind-syms
                bind-vals))))

(defun ensure-matched-symbol (sym name)
  (assert (string= (symbol-name sym) (string-upcase name))))

;; parse a single flow-state dsl form and return a form which creates
;; the flow-state CLOS instance for it.
(defun parse-flow-state (form)
  (let ((match (first form))
        (name (second form))
        (policy (third form))
        (bindings (mapcar #'canonicalize-binding (fourth form)))
        (selector (fifth form))
        (action (sixth form))
        (transition (seventh form)))
    (multiple-value-bind (bind-syms bind-vals)
        (binding-partition bindings)

      (ensure-matched-symbol match "flow-state")

      (let ((reset-function (gen-reset-function bind-syms bind-vals)))
        ;; Generate the instance maker for this flow-state.
        `(,name
          (let ,bindings ;; these are canonicalized, available for user.

            ;; TODO: Add a generated function to get the binding values out
            ;; in order of definition.
            (make-flow-state :name ',name
                             :policy ,policy
                             :exitingp ,(and (null selector)
                                             (null action)
                                             (null transition))
                             :selector ,selector
                             :action ,action
                             :transition ,transition
                             :reset ,reset-function)))))))

;; Parse a single flow dsl node into a form that evalutes to a hash
;; table containing each flow-state indexed by name.
(defun parse-flow (form)
  (let* ((flow-ht (gensym))
         (match (first form))
         (flow-name (second form))
         (flow-states (cddr form))
         (processed-flow-state-forms
           (loop :for flow-state :in flow-states
                 :collect (parse-flow-state flow-state))))

    (ensure-matched-symbol match "flow")

    `(,flow-name
      (let ((,flow-ht (make-hash-table)))
        ,@(loop :for (n f) :in processed-flow-state-forms
                :collect `(setf (gethash ',n ,flow-ht) ,f))
        ,flow-ht))))

;; Parse an entire call-flow and return a list of the name of it and a
;; form which evaluates into a hash table of flows keyed by their
;; name.
(defun parse-call-flow (form)
  (let* ((call-flow-ht (gensym))
         (match (first form))
         (call-flow-name (second form))
         (flows (cddr form))
         (processed-flow-forms (loop :for flow :in flows
                                     :collect (parse-flow flow))))

    (ensure-matched-symbol match "call-flow")

    `(,call-flow-name
      (let ((,call-flow-ht (make-hash-table)))
        ,@(loop :for (n f) :in processed-flow-forms
                :collect `(setf (gethash ',n ,call-flow-ht) ,f))
        ,call-flow-ht))))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; End parsing DSL codes.
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun execute-flow (core-state call-flow-name flow-name flow-state-name
                     &optional (come-from-state-name nil))
  "Find the CALL-FLOW-NAME call flow in the CORE-STATE, then lookup the
flow FLOW-NAME, and then the state FLOW-STATE-NAME inside of that flow.
Start execution of the flow from FLOW-STATE-NAME. Return a values of two
states, the one which executed before the currnet one which led to an
exit of the call-flow. COME-FROM-STATE-NAME is an arbitrary symbol that
indicates the previous flow-state name. This is often a symbolic name
so execute-flow can determine how the flow exited. Return two values
The previous state name and the current state name which resulted in
the exiting of the flow."


  (let* ((cfht (call-flow-table core-state))
         (call-flow (gethash call-flow-name cfht))
         (flow (gethash flow-name call-flow))
         (flow-state (gethash flow-state-name flow))
         (last-state-name nil)
         (current-state-name come-from-state-name)
         (selections ()))

    (loop ;; forever, that's right.
          (format t "Processing flow-state: ~A exiting=~A~%"
                  (name flow-state) (exitingp flow-state))

          ;; Step 1: Record state transition and update to current.
          (setf last-state-name current-state-name
                current-state-name (name flow-state))

          ;; Step 2: Perform execution policy
          (case (policy flow-state)
            (:reset
             ;; Execute the resetting thunk
             (funcall (reset flow-state))))

          ;; Step 3: Exit if reached exiting state.
          (when (exitingp flow-state)
            (return-from execute-flow (values last-state-name current-state-name)))

          ;; Step 4: Run Selector Function
          (format t "EF Calling Selector Function...~%")
          (setf selections (funcall (selector flow-state) core-state))

          ;; Step 5: Iterate the action across everything in the selections.
          ;; NOTE: This is not a map-tree or anything complex. It just assumes
          ;; selection is going to be one of:
          ;; a single hash table instance
          ;; a single instance of some class
          ;; a list of things that are either hash tables or class instances.
          (labels ((act-on-item (item)
                     (cond
                       ((hash-table-p item)
                        (maphash (lambda (k v)
                                   (declare (ignore k))
                                   (format t "EF Calling action function....~%")
                                   (funcall (action flow-state) core-state v))
                                 item))

                       ((atom item)
                        (funcall (action flow-state) core-state item)))))
            (cond
              ((consp selections)
               (loop :for item :in selections
                     :do (act-on-item item)))
              (t
               (act-on-item selections))))

          ;; Step 6: Run the transition function to determine the next
          ;; flow-state.  Currently, a transition can only go into the
          ;; SAME flow.
          (format t "EF Calling transition function.....~%")
          (setf flow-state
                (gethash (funcall (transition flow-state) core-state) flow)))))


(defun test-protocol-method-0 (inst cxt)
  (format t "TEST-PROTOCOL-METHOD-0 called: inst=~A cxt=~A~%" inst cxt))

(defun test-protocol-method-1 (inst cxt)
  (format t "TEST-PROTOCOL-METHOD-1 called: inst=~A cxt=~A~%" inst cxt))

(defun gen-call-flow ()
  (let ((form
          ;; This is directly from the ORG doc.
          `(call-flow
            default
            ;; Hrm. This is all single dispatch, is that good? Is
            ;; there more opportunity for CL's strengths in here?

            ;; NOTE: If the functions inside of the state machine
            ;; internally recurse by returning the correct states, the
            ;; executor will recurse forever until something about a
            ;; state transition picks a different path.

            (flow a-test-flow
                  (flow-state TEST-INSTANCE :reset ()
                              (lambda (core-state)
                                (declare (ignorable core-state))
                                (format t "Selector function called.~%")
                                42)

                              (lambda (core-state inst)
                                (format t "Action function called.~%")
                                (test-protocol-method-0 inst
                                                        (context core-state)))

                              (lambda (core-state)
                                (declare (ignore core-state))
                                (format t "Transition function called.~%")
                                'TEST-HT))

                  (flow-state TEST-HT :reset ()
                              (lambda (core-state)
                                (declare (ignorable core-state))
                                (format t "Selector function called.~%")
                                (let ((ht (make-hash-table)))
                                  (loop :for i :in '(1 2 3)
                                        :do (setf (gethash i ht) (+ i 10)))
                                  ht))

                              (lambda (core-state inst)
                                (format t "Action function called.~%")
                                (test-protocol-method-1 inst
                                                        (context core-state)))

                              (lambda (core-state)
                                (declare (ignore core-state))
                                (format t "Transition function called.~%")
                                'TEST-LIST-INSTANCES))

                  (flow-state TEST-LIST-INSTANCES :reset ()
                              ;; the two functions.

                              ;; Select what I want to work on.
                              (lambda (core-state)
                                (declare (ignorable core-state))
                                (format t "Selector function called.~%")
                                (list 1 2 3))

                              ;; This function is run for every instance
                              (lambda (core-state inst)
                                ;; a core function, not exposed to users.
                                (format t "Action function called.~%")
                                (test-protocol-method-0 inst
                                                        (context core-state)))

                              (lambda (core-state)
                                (declare (ignore core-state))
                                (format t "Transition function called.~%")
                                'TEST-LIST-HT))

                  (flow-state TEST-LIST-HT :reset ()
                              (lambda (core-state)
                                (declare (ignorable core-state))
                                (format t "Selector function called.~%")
                                (loop :for j :below 3
                                      :collect
                                      (let ((ht (make-hash-table)))
                                        (loop :for i :in '(1 2 3)
                                              :do (setf (gethash i ht)
                                                        (+ i (* j 10))))
                                        ht)))

                              (lambda (core-state inst)
                                (format t "Action function called.~%")
                                (test-protocol-method-1 inst
                                                        (context core-state)))

                              (lambda (core-state)
                                (declare (ignore core-state))
                                (format t "Transition function called.~%")
                                'EXIT/FLOW-FINISHED))

                  (flow-state EXIT/FLOW-FINISHED :reset ()
                              NIL)))))

    (let ((parsed-form (parse-call-flow form))
          (ht (make-hash-table :test #'eq)))
      (setf (gethash (first parsed-form) ht)
            (eval (second parsed-form)))
      ht)))

(defun test-execute-flow ()
  (let ((cs (make-core-state)))
    (merge-call-flow-table cs (gen-call-flow))
    (execute-flow cs 'default 'a-test-flow 'test-instance
                  (gensym "INIT-"))))
