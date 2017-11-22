(in-package :first-light)

(defclass analyzed-graph ()
  (;; for what category is this CL-GRAPH graph for?
   (%category :accessor category
              :initarg :category)
   ;; What are the entry roots into the CL-GRAPH?
   (%roots :accessor roots
           :initarg :roots
           :initform nil)
   ;; The completed CL-GRAPH representation of this category.
   (%graph :accessor graph
           :initarg :graph
           :initform nil)
   ;; The list of graphdefs that participated in this cl-graph rendition.
   (%graphdefs :accessor graphdefs
               :initarg :graphdefs
               :initform nil)))

(defun make-analyzed-graph (&rest init-args)
  (apply #'make-instance 'analyzed-graph init-args))

(defclass graphdef ()
  ((%name :accessor name
          :initarg :name)
   (%original-form :reader original-form
                   :initarg :original-form)
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

(defclass graphdef-depends-on ()
  (;; name of the graphdef we're depending upon.
   (%name :accessor name
          :initarg :name)
   ;; When we transmute the original form into this class type, we
   ;; repserve it for future debuggin.
   (%original-form :accessor original-form
                   :initarg :original-form)
   ;; a reference to the real graphdef containing the referenced subforms.
   (%graphdef :accessor graphdef
              :initarg :graphdef)
   ;; a hash table keyed by subform name, and value is the subform itself in
   ;; the appropriate graphdef instance.
   (%subforms :accessor subforms
              :initarg :subforms
              :initform (make-hash-table))))

(defun make-graphdef-depends-on (name &rest init-args)
  (apply #'make-instance 'graphdef-depends-on :name name init-args))

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
                                   (unless (cl-graph:parent-vertexes v)
                                     (push v results))))
    results))

(defun graph-leaves (g)
  "Find all vertex leaves (vertexes with no children) in the graph G and
return them as a list."
  (let ((results ()))
    (cl-graph:iterate-vertexes g (lambda (v)
                                   (unless (cl-graph:child-vertexes v)
                                     (push v results))))
    results))





(defun is-syntax-form-p (syntax-symbol form)
  (if (and form (consp form) (eq (first form) syntax-symbol))
      T
      NIL))

(defun is/->/p (thing)
  (when (symbolp thing)
    (string= (symbol-name thing) (symbol-name '->))))

(defun lift-splices (dependency-form)
  (let* ((lifted-vars (make-hash-table :test #'equal))
         (lifted-forms (make-hash-table :test #'equal))
         (lifted-dependency-form
           (loop :for element :in dependency-form
                 :collect
                 (cond
                   ((consp element)
                    element)
                   ((is/->/p element)
                    element)
                   (t
                    `(component-type ,element))))))

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

    (let* ((x (split-sequence:split-sequence
               '-> lifted-form :test (lambda (sym1 sym2)
                                       ;; HACK! -> is in two different packages.
                                       ;; The :first-light and user package.
                                       ;; So resolve to the symbol name itself.
                                       (if (and (symbolp sym1) (symbolp sym2))
                                           (string= (symbol-name sym1)
                                                    (symbol-name sym2))
                                           (eql sym1 sym2)))))
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
     :original-form form
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


;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod extension-file-type ((extension-type (eql 'graphs)))
  "gph")

(defmethod prepare-extension ((extension-type (eql 'graphs))
                              owner path)

  ;; 1) Collect ALL graph-definitions into the appropriate
  ;; analyzed-graph objects.  The graphs aren't fully analyzed yet,
  ;; this is only the parsing phase.
  (loop
    :with defs = (collect-extension-forms extension-type path)
    :for def :in defs
    :do (let ((parsed-def (parse-graph-definition def)))
          (multiple-value-bind (analyzed-graph present)
              (gethash (category parsed-def) (analyzed-graphs owner))

            (unless present
              (let ((new-analyzed-graph (make-analyzed-graph
                                         :category (category parsed-def))))

                (setf (gethash (category parsed-def) (analyzed-graphs owner))
                      new-analyzed-graph

                      analyzed-graph
                      new-analyzed-graph)))

            (when (enabled parsed-def)
              (push parsed-def (graphdefs analyzed-graph))))))

  ;; TODO: 2) perform the analysis for each graph category type. Note:
  ;; a category may be a consp form in addition to a symbol.
  ;; A) Ensure if subdag, all are subdag.
  ;; B) Ensure if subgraph, all are subgraph.

  ;; TODO: 3) convert each category to appropriate cl-graph version
  (loop :for angph :being :the :hash-values :in (analyzed-graphs owner)
        :do (analyze-graph angph))

  )




;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun add-cross-product-edges (clg source-list target-list)
  (map-product
   (lambda (v1 v2)
     (cl-graph:add-edge-between-vertexes clg v1 v2 :edge-type :directed))
   source-list
   target-list))

(defun annotate-splices (val-list &rest args)
  "Take each member VAL of VAL-LIST and return a list of (VAL ,@ARGS)."
  (mapcar (lambda (v)
            (if (is-syntax-form-p 'splice v)
                `(,v ,@(copy-seq args))
                v))
          val-list))

(defun absorb-depforms (clg gdef depforms)
  "Traverse the depforms and add them into the clg as edges while
keeping reference to the initial gdef assocated with the
depforms. Return three values: the cl-graph, the roots as elements,
the leaves as elements."
  (let ((roots ())
        (leaves ()))
    (loop
      :for depform :in depforms
      :for lifted-form = (lifted-form depform)
      ;; check this when condition for validity.
      :when lifted-form :do
        (pushnew (car (first lifted-form)) roots :test #'equalp)
        (pushnew (cadar (last lifted-form)) leaves :test #'equalp)
        (loop :for (from to) :in lifted-form :do
          (add-cross-product-edges clg
                                   (annotate-splices from gdef)
                                   (annotate-splices to gdef))))

    (values clg
            (remove-duplicates (mapcan #'identity roots) :test #'equalp)
            (remove-duplicates (mapcan #'identity leaves) :test #'equalp))))

(defun analyze-graphdef-depends-on (angph)
  "Transmute the :depends-on form in each graphdef object in the ANGPH
into real graphdef references holding real references to the named
subforms."

  nil)


;; TODO: This should return two values. the subform and the gdef it is
;; contained in.
(defun lookup-splice (splice-form gdef)
  "Find the subform named SPLICE-FORM in GDEF, or in any available imports."
  (let ((splice-name (second splice-form)))
    (gethash splice-name (subforms gdef))))

(defun analyze-graph (angph)
  ;; TODO: Huge bug/missing feature. This only knows how to analyze/construct
  ;; directed graphs. Maybe that is actually ok and we don't need undirected
  ;; graphs....in which case, dump the subgraph form in the dsl.

  ;; First, resolve all depends-on lines into meaniingful structures
  ;; with real references and such. This helps us look up splices.
  (analyze-graphdef-depends-on angph)


  (let ((clg (cl-graph:make-graph 'cl-graph:graph-container
                                  ;; I store complex elements and
                                  ;; edges.
                                  :vertex-test #'equalp
                                  :edge-test #'equalp
                                  :default-edge-type :directed)))

    ;; We do an iterative algorithm where we continuously refine the
    ;; graph we're making by expanding slices in additional passes until there
    ;; are no splices left.
    ;;(format t "Processing graph category: ~A~%" (category angph))

    ;; First, add the initial depforms from the roots.
    (loop :for gdef :in (graphdefs angph)
          :when (roots gdef) :do
            (loop :for root :in (roots gdef) :do
              ;; For the initial seeding, we don't care about the
              ;; roots/leaves of the initial root subforms.
              (absorb-depforms clg gdef
                               (depforms (gethash root (subforms gdef))))))
    ;; debug output
    (format t "Original graph....~%")
    (dolist (edge (cl-graph:edges clg))
      (let ((*print-right-margin* most-positive-fixnum))
        (format t "Computed edge: from: ~A to: ~A~%"
                (cl-graph:element (cl-graph:source-vertex edge))
                (cl-graph:element (cl-graph:target-vertex edge)))))

    ;; Then, iterate the graph, expanding the splicing vertexes (of
    ;; which there should only be ONE of each kind of splice vertex)
    ;; until no more splices exist.

    (format t "Run iterative algorithm to resolve full graph.~%")
    (dolist (splice (cl-graph:find-vertexes-if
                     clg (lambda (v)
                           (is-syntax-form-p
                            'splice (first (cl-graph:element v))))))
      (format t "Processing splice: ~A~%" splice)
      (let (;; source vertexes with a target of this splice vertex.
            (parents (cl-graph:parent-vertexes splice))
            ;; target vertexes with a source of this splice vertex.
            (children (cl-graph:child-vertexes splice)))

        (format t "  parents: ~A~%  children: ~A~%"
                parents children)

        (destructuring-bind (splice-form gdef)
            (cl-graph:element splice)

          ;; TODO: ensure gdefs are correct with splice
          ;; expansions.  They aren't currently.

          ;; Now, absorb the splice from the right spot, get the
          ;; roots and leaves, then fixup the edges.
          (multiple-value-bind (clg splice-roots splice-leaves)
              (absorb-depforms
               clg gdef (depforms (lookup-splice splice-form gdef)))

            (format t "  splice-roots: ~A~%" splice-roots)
            (format t "  splice-leaves: ~A~%" splice-leaves)
            ;; delete the original parent edges.
            (loop :for parent :in parents :do
              (cl-graph:delete-edge-between-vertexes
               clg parent splice))
            ;; add the new edges from the parents to the new-roots.
            (add-cross-product-edges
             clg parents (annotate-splices splice-roots gdef))
            ;; delete the original child edges.
            (loop :for child :in children :do
              (cl-graph:delete-edge-between-vertexes
               clg splice child))
            ;; add the new edges from the new-leaves to the children.
            (add-cross-product-edges
             clg (annotate-splices splice-leaves gdef) children)
            ;; Then finally, delete the expanded splice vertex
            (cl-graph:delete-vertex clg splice)))))



    (format t "Single graph expansion....~%")
    (dolist (edge (cl-graph:edges clg))
      (let ((*print-right-margin* most-positive-fixnum))
        (format t "Expanded edge: from: ~A to: ~A~%"
                (cl-graph:element (cl-graph:source-vertex edge))
                (cl-graph:element (cl-graph:target-vertex edge)))))

    (format t "Graph roots for clg are: ~A~%" (graph-roots clg))
    (format t "Graph leaves for clg are: ~A~%" (graph-leaves clg))

    ;; This is reversed cause of the "depends-on" meaning of -> in a
    ;; component-dependenct graph. For other uses of -> which mean
    ;; "and then do" then we don't need the reverse. I'm contemplating
    ;; => and -> for those meanings....
    (format t "toposort: ~A~%" (reverse (cl-graph:topological-sort clg)))

    ;; and we're done with this analyzed-graph.
    (setf (graph angph) clg)))


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
