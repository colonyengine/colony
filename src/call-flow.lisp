(in-package :gear)

(defclass flow-state ()
  ((%name :accessor name
          :initarg :name)
   (%policy :accessor policy ; :reset only for now.
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

(defun canonicalize-binding (binding)
  (if (symbolp binding)
      (list binding nil)
      binding))

(defun binding-partition (bindings)
  (loop :for (b v) :in bindings
        :collect b :into bs
        :collect v :into vs
        :finally (return (values bs vs))))

;; TODO: this should return a let over lambda to do a once only of the bind-vals
;; evaluation. Needs fixing.
(defun gen-reset-function (symbols values)
  `(lambda ()
     (setf
      ,@(mapcan
         (lambda (symbol value)
           `(,symbol ,value))
         symbols
         values))))

(defun ensure-matched-symbol (symbol name)
  (assert (string= (symbol-name symbol) (string-upcase name))))

(defun parse-flow-state (form)
  "Parse a single flow-state DSL form and return a form which creates the
flow-state CLOS instance for it."
  (destructuring-bind (match name policy binds selector action transition) form
    (let ((binds (mapcar #'canonicalize-binding binds)))
      (multiple-value-bind (bind-syms bind-vals) (binding-partition binds)
        (ensure-matched-symbol match "flow-state")
        (let ((reset-function (gen-reset-function bind-syms bind-vals)))
          ;; Generate the instance maker for this flow-state.
          `(,name
            (let ,binds ; these are canonicalized, available for user.
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
                               :reset ,reset-function))))))))

(defun parse-flow (form)
  "Parse a single flow DSL node into a form that evaluates to a hash table
containing each flow-state indexed by name."
  (destructuring-bind (match flow-name . flow-states) form
    (let ((flow-table (gensym)))
      (ensure-matched-symbol match "flow")
      `(,flow-name
        (let ((,flow-table (make-hash-table)))
          ,@(loop :for (name state) :in (mapcar #'parse-flow-state flow-states)
                  :collect `(setf (gethash ',name ,flow-table) ,state))
          ,flow-table)))))

(defun parse-call-flow (form)
  "Parse an entire call-flow and return a list of the name of it and a form
which evaluates to a hash table of flows keyed by their name."
  (destructuring-bind (match call-flow-name . flows) form
    (let ((call-flow-table (gensym)))
      (ensure-matched-symbol match "call-flow")
      `(,call-flow-name
        (let ((,call-flow-table (make-hash-table)))
          ,@(loop :for (name flow) :in (mapcar #'parse-flow flows)
                  :collect `(setf (gethash ',name ,call-flow-table) ,flow))
          ,call-flow-table)))))

(defun execute-flow (core-state call-flow-name flow-name flow-state-name
                     &optional come-from-state-name)
  "Find the CALL-FLOW-NAME call flow in the CORE-STATE, then lookup the flow
FLOW-NAME, and then the state FLOW-STATE-NAME inside of that flow. Start
execution of the flow from FLOW-STATE-NAME. Return a values of two states, the
one which executed before the currnet one which led to an exit of the call-flow.
COME-FROM-STATE-NAME is an arbitrary symbol that indicates the previous
flow-state name. This is often a symbolic name so execute-flow can determine how
the flow exited. Return two values The previous state name and the current state
name which resulted in the exiting of the flow."
  (loop :with call-flow = (gethash call-flow-name (call-flow-table core-state))
        :with flow = (gethash flow-name call-flow)
        :with flow-state = (gethash flow-state-name flow)
        :with current-state-name = come-from-state-name
        :with last-state-name
        :with selections
        :do (format t "Processing flow-state: ~A exiting=~A~%"
                    (name flow-state) (exitingp flow-state))
            ;; Step 1: Record state transition and update to current.
            (setf last-state-name current-state-name
                  current-state-name (name flow-state))
            ;; Step 2: Perform execution policy
            (case (policy flow-state)
              (:reset ; Execute the resetting thunk
               (funcall (reset flow-state))))
            ;; Step 3: Exit if reached exiting state.
            (when (exitingp flow-state)
              (return-from execute-flow
                (values last-state-name current-state-name)))
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
                          (maphash
                           (lambda (k v)
                             (declare (ignore k))
                             (format t "EF Calling action function....~%")
                             (funcall (action flow-state) core-state v))
                           item))
                         ((atom item)
                          (funcall (action flow-state) core-state item)))))
              (if (consp selections)
                  (map nil #'act-on-item selections)
                  (act-on-item selections)))
            ;; Step 6: Run the transition function to determine the next
            ;; flow-state. Currently, a transition can only go into the SAME
            ;; flow.
            (format t "EF Calling transition function.....~%")
            (setf flow-state
                  (gethash (funcall (transition flow-state) core-state) flow))))

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
                              NIL NIL NIL)))))
    (let ((parsed-form (parse-call-flow form))
          (ht (make-hash-table :test #'eq)))
      (setf (gethash (first parsed-form) ht)
            (eval (second parsed-form)))
      ht)))

(defun test-execute-flow ()
  (let ((core-state (make-core-state)))
    (merge-call-flow-table core-state (gen-call-flow))
    (execute-flow core-state 'default 'a-test-flow 'test-instance
                  (gensym "INIT-"))))
