(in-package :first-light)

(defclass graphdef ()
  ((%name :accessor name
          :initarg :name)
   (%enabled :accessor enabled
             :initarg :enabled)
   (%category :accessor category
              :initarg :category)
   (%depends-on :accessor depends-on
                :initarg :depends-on)
   (%roots :accessor roots
           :initarg :roots)
   (%subforms :accessor subforms
              :initarg :subforms)))

(defun make-graphdef (name &rest init-args)
  (apply #'make-instance 'graphdef :name name init-args))

(defclass subform ()
  ((%name :accessor name
          :initarg :name)
   (%kind :accessor kind
          :initarg :kind)
   (%depforms :accessor depforms
              :initarg :depforms)))

(defun make-subform (name &rest init-args)
  (apply #'make-instance 'subform :name name init-args))

(defclass depform ()
  ;; the original depform for debugging/error output
  ((%original-form :accessor original-form
                   :initarg :original-form)
   ;; the lifted form with the new symbols, if any
   (%lifted-form :accessor lifted-form
                 :initarg :lifted-form)
   ;; Is it :empty, :vertex, or :hyperedges
   (%kind :accessor kind
          :initarg :kind)
   ;; The symbol -> form mapping
   (%lifted-vars :accessor lifted-vars
                 :initarg :lifted-vars)
   ;; the form -> symbol mapping
   (%lifted-forms :accessor lifted-forms
                  :initarg :lifted-forms)))

(defun make-depform (&rest init-args)
  (apply #'make-instance 'depform init-args))





(defun graph-roots (g)
  "Find all vertex roots (vertexes with no parents) in the directed
graph G and return them as a list in no particular order."
  (let ((results ()))
    (cl-graph:iterate-vertexes g (lambda (v)
                                   (when (cl-graph:rootp v)
                                     (push v results))))
    results))

(defun graph-leaves (g)
  "Find all vertex leaves (vertexes with no children) in the graph G and
return them as a list."
  (let ((results ()))
    (cl-graph:iterate-vertexes g (lambda (v)
                                   (unless (cl-graph:has-children-p v)
                                     (push v results))))
    results))





(defun is-syntax-form-p (syntax-symbol form)
  (if (and form (consp form) (eq (first form) syntax-symbol))
      T
      NIL))

(defun lift-splices (dependency-form)
  (let* ((lifted-vars (make-hash-table :test #'equal))
         (lifted-forms (make-hash-table :test #'equal))
         (lifted-dependency-form
           (loop :for element :in dependency-form
                 :collect
                 (cond
                   ((is-syntax-form-p 'splice element)
                    (multiple-value-bind (var presentp)
                        (gethash element lifted-forms)
                      (if presentp
                          var
                          (let ((new-var
                                  (gensym
                                   (concatenate 'string
                                                (string-upcase
                                                 (symbol-name
                                                  (second element)))
                                                "-"))))
                            (setf (gethash new-var lifted-vars) element)
                            (setf (gethash element lifted-forms) new-var)))))
                   (t element)))))

    (values lifted-dependency-form lifted-vars lifted-forms)))

(defun segment-dependency-form (form)
  "Lift splices and then segment the dependency FORM into hyperedges.

If the form is null, return four values:
  NIL, :empty, lift-vars, lift-forms

If the form is not null, but contains no hyper edges, return three values:
  lifted-form and :vertex, lift-vars, lift-forms

If the form is not null, and contains hyper edges, return three values:
  list of hyper-edge pairs, :hyperedges, lift-vars, lift-forms"

  (multiple-value-bind (lifted-form lift-vars lift-forms) (lift-splices form)

    (let* ((x (split-sequence:split-sequence '-> lifted-form))
           ;; cut into groups of two with rolling window
           (connections
             (loop :for (k j . nil) :in (maplist #'identity x)
                   :when j :collect `(,k ,j))))
      (cond
        ((null form)
         (values form :empty lift-vars lift-forms))
        ((= (length x) 1)
         ;; no hyperedges
         (values form :vertex lift-vars lift-forms))
        (t ;; hyperedges found
         (values connections :hyperedges lift-vars lift-forms))))))


;; Then the code to perform the parsing.
(defun parse-subform (form)
  (assert (or (is-syntax-form-p 'subdag form)
              (is-syntax-form-p 'subgraph form)))

  (destructuring-bind (kind name . dependency-forms) form
    (make-subform
     name
     :kind kind
     :depforms
     (loop :for dep :in dependency-forms
           :collect
           (multiple-value-bind (lifted-dependency-form kind lifted-vars
                                 lifted-forms)
               (segment-dependency-form dep)
             (make-depform :original-form dep
                           :lifted-form lifted-dependency-form
                           :kind kind
                           :lifted-vars lifted-vars
                           :lifted-forms lifted-forms))))))


(defun get-graph-option (option-name option-form)
  (second (member option-name option-form)))

(defun parse-graph-definition (form)
  (assert (is-syntax-form-p 'graph-definition form))

  (destructuring-bind (name options . subforms) (rest form)
    (make-graphdef
     name
     :enabled (get-graph-option :enabled options)
     :category (get-graph-option :category options)
     :depends-on (get-graph-option :depends-on options)
     :roots (get-graph-option :roots options)
     :subforms
     (let ((subform-db (make-hash-table)))
       (loop :for i :in subforms
             :do (let ((sf (parse-subform i)))
                   (setf (gethash (name sf) subform-db)
                         sf)))
       subform-db))))

(defun doit ()
  (parse-graph-definition
   `(graph-definition
     :gear-example
     (:enabled t
      :category component-dependency
      :depends-on ((:core (unknown-types core-types)))
      :roots (all-ordered-types))

     ;; user chooses this name
     (subdag ordered-types
             ())

     ;; user creates the master ordering of the types.
     (subdag all-ordered-types
             ((splice unknown-types)
              -> (splice ordered-types)
              -> (splice core-types))))))
