(in-package :first-light)

(defstruct subform
  ;; Name of the subform
  name
  ;; Is it a 'subdag or a 'subgraph
  kind
  ;; a list of parsed and lifted depforms.
  depforms)

(defstruct depform
  ;; the original depform for debugging/error output
  original-form
  ;; the lifted form with the new symbols, if any
  lifted-form
  ;; Is it :empty, :vertex, or :hyperedges
  kind
  ;; The symbol -> form mapping
  lifted-vars
  ;; the form -> symbol mapping
  lifted-forms)

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
     :name name
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
