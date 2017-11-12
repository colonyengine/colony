(in-package :first-light)

;; First, some graph utilities I need:

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

(defun lift-splices (form)
  "If the dependency FORM has splices in it, lift them into a dictionary
with gensymed names associated with the splice form. Return two values,
the modified (if required) dependency form and an association list
 (a hash-table) keyed by the splice form and value is the variable
name as a symbol."
  (let ((lifts (make-hash-table :test #'equal)))
    (labels ((substitute-splice (form)
               (if (null form)
                   nil
                   (cons
                    (let ((thingy (first form)))
                      (cond
                        ((symbolp thingy)
                         thingy)
                        ((is-syntax-form-p 'splice thingy)
                         (multiple-value-bind (splice-var presentp)
                             (gethash thingy lifts)
                           (if presentp
                               splice-var
                               (let ((new-var
                                       (gensym
                                        (concatenate 'string
                                                     (string-upcase
                                                      (symbol-name
                                                       (second thingy)))
                                                     "-"))))
                                 (setf (gethash thingy lifts) new-var)
                                 new-var))))
                        (t (error "lift-splices is broken."))))

                    (substitute-splice (rest form))))))

      (values (substitute-splice form) lifts))))

(defun segment-dependency-form (form)
  "Lift splices and then Segment the dependency FORM.

If the form is null, return three values:
  NIL, :empty, lifts

If the form is not null, but contains no hyper edges, return three values:
  form and :vertex, lifts

If the form is not null, and contains hyper edges, return three values:
  list of hyper-edge pairs, :hyperedges, lifts"

  (multiple-value-bind (lifted-form lifts) (lift-splices form)

    (let* ((x (split-sequence:split-sequence '-> lifted-form))
           ;; cut into groups of two with rolling window
           (connections
             (loop :for (k j . nil) :in (maplist #'identity x)
                   :when j :collect `(,k ,j))))
      (cond
        ((null form)
         (values form :empty lifts))
        ((= (length x) 1)
         ;; no hyperedges
         (values form :vertex lifts))
        (t ;; hyperedges found
         (values connections :hyperedges lifts))))))


;; Then the code to perform the parsing.
