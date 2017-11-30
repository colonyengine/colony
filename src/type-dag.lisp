(in-package :first-light)

(defclass analyzed-graph ()
  (;; for what category is this CL-GRAPH graph for?
   (%category :accessor category
              :initarg :category)
   ;; The completed CL-GRAPH representation of this category.
   (%graph :accessor graph
           :initarg :graph
           :initform nil)
   ;; A topological sort of the graph, NIL if unable to be created due
   ;; to cycles (or there are no nodes in the graph).
   (%toposort :accessor toposort
              :initarg :toposort
              :initform NIL)
   ;; different graph categories can create some annotated data that is used in
   ;; context specific ways.
   (%annotation :accessor annotation
                :initarg :annotation
                :initform NIL)
   ;; The table of graphdefs that participated in this cl-graph rendition.
   ;; Key is graph-definition name, value is graphdef instance.
   (%graphdefs :accessor graphdefs
               :initarg :graphdefs
               :initform  (make-hash-table))))

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
   ;; This starts out as a list grabbed from the graph-definition dsl,
   ;; but we transform it later into a hash table with values of
   ;; graphdef-depends-on instances to hold a richer semantic
   ;; understanding of original depends-on forms in the
   ;; graph-definition dsl.
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
   (%canonical-form :accessor canonical-form
                    :initarg :canonical-form)
   ;; Is it :empty, :vertex, or :hyperedges
   (%kind :accessor kind
          :initarg :kind)))

(defun make-depform (&rest init-args)
  (apply #'make-instance 'depform init-args))

;; ;;;; annotation classes for different graph categories


;; For category COMPONNENT-DEPENDENCY
(defclass graph-annotation/component-dependency ()
  ((%unknown-type-id :accessor unknown-type-id
                     :initarg :unknown-type-id)
   (%referenced-types :accessor referenced-types
                      :initarg :referenced-types
                      :initform (make-hash-table))))

(defun make-graph-annotation/component-dependency (&rest init-args)
  (apply #'make-instance 'graph-annotation/component-dependency init-args))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TODO: stick in a util file somewhere.
(defun eql/package-relaxed (obj1 obj2)
  (if (and (symbolp obj1) (symbolp obj2))
      (string= (symbol-name obj1)
               (symbol-name obj2))
      (eql obj1 obj2)))




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





;; A crappy kind of pattern matching.
(defun is-syntax-form-p (syntax-symbol form)
  (cond
    ((consp syntax-symbol)
     (when (consp form)
       (eql/package-relaxed (first syntax-symbol) (first form))))
    ((symbolp syntax-symbol)
     (when (symbolp form)
       (eql/package-relaxed syntax-symbol form)))
    ;; maybe other cases needed?
    (t nil)))


(defun canonicalize-dependency-form (dependency-form)
  (loop :for element :in dependency-form
        :collect
        (cond
          ((consp element)
           element)
          ((is-syntax-form-p '-> element)
           element)
          (t
           `(component-type ,element)))))


(defun segment-dependency-form (form)
  "Lift splices and then segment the dependency FORM into hyperedges.

If the form is null, return four values:
  NIL, :empty

If the form is not null, but contains no hyper edges, return three values:
  canonical-form and :vertex

If the form is not null, and contains hyper edges, return three values:
  list of hyper-edge pairs, :hyperedges"

  (let ((canonical-form (canonicalize-dependency-form form)))

    (let* ((x (split-sequence:split-sequence
               '-> canonical-form :test #'eql/package-relaxed))
           ;; cut into groups of two with rolling window
           (connections
             (loop :for (k j . nil) :in (maplist #'identity x)
                   :when j :collect `(,k ,j))))
      (cond
        ((null canonical-form)
         (values canonical-form :empty))
        ((= (length x) 1)
         ;; no hyperedges
         (values canonical-form :vertex))
        (t ;; hyperedges found
         (values connections :hyperedges))))))


;; Then the code to perform the parsing.
(defun parse-subform (form)
  (assert (or (is-syntax-form-p '(subdag) form)
              (is-syntax-form-p '(subgraph) form)))

  (destructuring-bind (kind name . dependency-forms) form
    (make-subform
     name
     :kind kind
     :depforms
     (loop :for dep :in dependency-forms
           :collect
           (multiple-value-bind (lifted-dependency-form kind)
               (segment-dependency-form dep)
             (make-depform :original-form dep
                           :canonical-form lifted-dependency-form
                           :kind kind))))))


(defun get-graph-option (option-name option-form)
  (second (member option-name option-form)))

(defun parse-graph-definition (form)
  (assert (is-syntax-form-p '(graph-definition) form))

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
              (setf (gethash (name parsed-def) (graphdefs analyzed-graph))
                    parsed-def)))))

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

(defun annotate-splice (v &rest args)
  (if (is-syntax-form-p '(splice) v)
      `(,v ,@(copy-seq args))
      v))

(defun annotate-splices (val-list &rest args)
  (mapcar (lambda (v)
            (apply #'annotate-splice v args))
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
      :for kind = (kind depform)
      :for canonical-form = (canonical-form depform)
      ;; check this when condition for validity.
      :when canonical-form :do
        (ecase kind
          ((:empty)
           #++(format t "absorb-depforms: ignoring :empty form.~%")
           nil)
          ((:hyperedges)
           #++(format t "absorb-depforms: absorbing :hyperedges.~%")
           (pushnew (car (first canonical-form)) roots :test #'equalp)
           (pushnew (cadar (last canonical-form)) leaves :test #'equalp)
           (loop :for (from to) :in canonical-form :do
             (add-cross-product-edges clg
                                      (annotate-splices from gdef)
                                      (annotate-splices to gdef))))
          ((:vertex)
           #++(format t "absorb-depforms: absorbing :vertex.~%")
           ;; the :vertex for is not only the roots, but also the leaves.
           (pushnew canonical-form roots :test #'equalp)
           (pushnew canonical-form leaves :test #'equalp)
           (loop :for vert :in canonical-form :do
             #++(format t "Adding :vertex: ~A~%" vert)
             (cl-graph:add-vertex clg (annotate-splice vert gdef))))))

    (values clg
            (remove-duplicates (mapcan #'identity roots) :test #'equalp)
            (remove-duplicates (mapcan #'identity leaves) :test #'equalp))))

(defun analyze-graphdef-depends-on (angph)
  "Transmute the :depends-on form in each graphdef object in the ANGPH
into real graphdef references holding real references to the named
subforms."

  (loop :for gdef :being :the :hash-values :in (graphdefs angph) :do
    (let ((whole-depends-on-form (depends-on gdef)))

      ;; depends-on used to be list, but now we're converting it into a
      ;; hash table with more semantic data in it.
      (setf (depends-on gdef) (make-hash-table))

      (loop :for (gdef-name subform-names) :in whole-depends-on-form :do
        (let* ((gdef-reference (gethash gdef-name (graphdefs angph)))
               (analyzed-depends-on
                 (make-graphdef-depends-on
                  gdef-name
                  :graphdef gdef-reference
                  :original-form (list gdef-name subform-names))))

          (assert gdef-reference)

          ;; Now set up the subforms entry in the analyzed-depends-on object.
          (if (eq subform-names :all)
              ;; get all subform names in gdef
              (maphash
               (lambda (subform-name subform-instance)
                 (setf (gethash subform-name (subforms analyzed-depends-on))
                       subform-instance))
               (subforms gdef-reference))
              ;; find the listed subform-names (which if it is NIL, do
              ;; nothing) in the gdef and assign them.
              (loop :for subform-name :in subform-names :do
                (setf (gethash subform-name (subforms analyzed-depends-on))
                      (gethash subform-name (subforms gdef-reference)))))

          ;; store the semantic analysis of the depends-on form,
          ;; transmuted into its new graphdef-depends-on form, back
          ;; into the initiating gdef.
          (setf (gethash (name analyzed-depends-on) (depends-on gdef))
                analyzed-depends-on))))))

(defun lookup-splice (splice-form gdef)
  "Find the subform describing SPLICE-FORM in GDEF, or in any
available depends-on in that GDEF."
  (let ((splice-name (second splice-form)))
    ;; 1) Check of the splice is natively in the current gdef.
    (when-let ((splice (gethash splice-name (subforms gdef))))
      #++(format t "Found splice ~A as subform ~A in current gdef ~A~%"
              splice-name splice (name gdef))
      (return-from lookup-splice
        (values splice gdef)))

    ;; 2) If it isn't, then check which depends-on in the current gdef
    (loop :for dep-inst :being :the :hash-values :in (depends-on gdef) :do
      (multiple-value-bind (subform present)
          (gethash splice-name (subforms dep-inst))
        (when present
          #++(format t "Found splice ~A as subform ~A in depended-on gdef ~A~%"
                  splice-name subform (name dep-inst))
          (return-from lookup-splice
            (values subform (graphdef dep-inst))))))

    ;; #0 Otheriwse, you're out of luck. Prolly should put an error here.
    (values nil nil)))

(defun analyze-graph (angph)
  ;; TODO: Huge bug/missing feature. This only knows how to analyze/construct
  ;; directed graphs. Maybe that is actually ok and we don't need undirected
  ;; graphs....in which case, dump the subgraph form in the dsl.

  ;; First, resolve all depends-on lines into meaningful structures
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
    (loop :for gdef :being :the :hash-values :in (graphdefs angph)
          :when (roots gdef) :do
            (loop :for root :in (roots gdef) :do
              ;; For the initial seeding, we don't care about the
              ;; roots/leaves of the initial root subforms.
              (absorb-depforms
               clg
               ;; This is the gdef that these depforms came from...
               gdef
               ;; ...in which all splices must be looked up in,
               ;; either directly or through a graphdef-depends-on object.
               (depforms (gethash root (subforms gdef))))))

    ;; debug output
    #++(format t "Original graph....~%")
    #++(dolist (edge (cl-graph:edges clg))
      (let ((*print-right-margin* most-positive-fixnum))
        (format t "Computed edge: from: ~A to: ~A~%"
                (cl-graph:element (cl-graph:source-vertex edge))
                (cl-graph:element (cl-graph:target-vertex edge)))))

    ;; Then, iterate the graph. Each iteration will substitute the
    ;; current splice forms for the actual graphs indicated by those
    ;; splice names. This may ntroduce more splices the next iteration
    ;; will get. Stop when there are no more splices to substitute.
    (loop
      ;; Don't convert to dolist, I need to recompute the find-vertexes-if
      ;; in each iteration.
      :for splices = (cl-graph:find-vertexes-if
                      clg (lambda (v)
                            (is-syntax-form-p
                             '(splice) (first (cl-graph:element v)))))
      :unless splices :return nil
        :do
           #++(format t "Found splice vertexes: ~A~%" splices)

           (dolist (splice splices)
             #++(format t "processing splice: ~A~%" splice)
             (let (;; source vertexes with a target of this splice vertex.
                   (parents (cl-graph:parent-vertexes splice))
                   ;; target vertexes with a source of this splice vertex.
                   (children (cl-graph:child-vertexes splice)))

               #++(format t "  parents: ~A~%  children: ~A~%"
                       parents children)

               (destructuring-bind (splice-form gdef)
                   (cl-graph:element splice)

                 (multiple-value-bind (lookedup-splice lookedup-gdef)
                     (lookup-splice splice-form gdef)

                   ;; Now, absorb the splice into clg, get the roots
                   ;; and leaves, then fixup the edges.
                   (multiple-value-bind (clg splice-roots splice-leaves)
                       (absorb-depforms
                        clg lookedup-gdef (depforms lookedup-splice))

                     #++(format t "  splice-roots: ~A~%" splice-roots)
                     #++(format t "  splice-leaves: ~A~%" splice-leaves)
                     ;; delete the original parent edges.
                     (dolist (parent parents)
                       (cl-graph:delete-edge-between-vertexes
                        clg parent splice))
                     ;; add the new edges from the parents to the new-roots.
                     (add-cross-product-edges
                      clg
                      parents
                      (annotate-splices splice-roots lookedup-gdef))
                     ;; delete the original child edges.
                     (dolist (child children)
                       (cl-graph:delete-edge-between-vertexes
                        clg splice child))
                     ;; add the new edges from the new-leaves to the children.
                     (add-cross-product-edges
                      clg
                      (annotate-splices splice-leaves lookedup-gdef)
                      children)
                     ;; Then finally, delete the expanding splice vertex
                     (cl-graph:delete-vertex clg splice)))))))



    #++(format t "Single graph expansion....~%")
    #++(dolist (edge (cl-graph:edges clg))
      (let ((*print-right-margin* most-positive-fixnum))
        (format t "Expanded edge: from: ~A to: ~A~%"
                (cl-graph:element (cl-graph:source-vertex edge))
                (cl-graph:element (cl-graph:target-vertex edge)))))

    #++(format t "Graph roots for clg are: ~A~%" (graph-roots clg))
    #++(format t "Graph leaves for clg are: ~A~%" (graph-leaves clg))

    ;; check for cycles.  TODO: We should have a flag for this in a
    ;; graph definition that saturates all definitions of that
    ;; category to T (maybe? Should there be another way to specify
    ;; this?). Currently, we just hard code it here.
    (let ((contains-cycles-p
            (cl-graph:find-vertex-if clg (lambda (vert)
                                           (cl-graph:in-cycle-p clg vert)))))

      ;; compute/store toposort, can only do if no cycles.
      (unless contains-cycles-p
        (let ((tsort (mapcar #'cl-graph:element
                             (cl-graph:topological-sort clg))))
          #++(format t "toposort: ~A~%" tsort)
          (setf (toposort angph) tsort)))

      ;; Compute an annotation for the graph category. This is graph
      ;; category specific. Probably should be a GF...
      (cond
        ((eql/package-relaxed 'component-dependency (category angph))

         (let ((annotation (make-graph-annotation/component-dependency
                            :unknown-type-id (gensym "UNKNOWN-TYPE-ID-"))))

           ;; collect all referenced component-types in the graph.
           (cl-graph:iterate-vertexes
            clg (lambda (v)
                  (let ((elem-v (cl-graph:element v)))
                    (when (is-syntax-form-p '(component-type) elem-v)
                      (setf (gethash (second elem-v)
                                     (referenced-types annotation))
                            T)))))

           (setf (annotation angph) annotation)))

        (t
         ;; TODO: need to figure out a good way to handle these cases
         ;; as they grow.
         nil))

      ;; and we're done with this analyzed-graph.
      (setf (graph angph) clg))))
