;; The graph forms go through this transformation:
;; 1) define-graph -> metagraph storage of forms only
;; 2) parsing of raw forms into AST 0
;; 3) .. N) Repeated transforming AST X to AST X+1
;; N+1) Production of concrete categorical class from last AST.



;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Meta graph management
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The single database of all define-graph forms
(defclass metagraph ()
  ((%metagraphs-by-category :reader metagraphs-by-category
                            :initarg :metagraphs-by-category
                            ;; key: category type-name
                            ;; value: hash-table
                            ;; key2: metagraph form name
                            ;; value2: metagraph-definition
                            :initform (au:dict #'eq))))

;; A raw and unprocessed define-graph form and the timestamp when it was
;; stored in one of these class types.
(defclass metagraph-form ()
  ((%timestamp :reader timestamp
               :initarg timestamp)
   (%form :read form
          :initarg :form)))

;; A stack of metagraph-forms (with implicitly the same categroy) for the same
;; name.  The top form is always the active form for this metagraph-definition.
(defclass metagraph-definition ()
  ((%name :read name
          :initarg :name)
   (%metagraph-forms :reader metagraph-forms
                     :initarg :metagraph-forms
                     :initform nil)))

;; ;;;;;;;;;;;;;
;; metagraph protocol for interacting with the global metagraph database
;; ;;;;;;;;;;;;;

;; If category is :all and name is :all, then wipe clean the entire metagraph db
;; of any metagraph-definitions. If category is a defined symbol and name is
;; :all, the entire set of metagraph-definitions for that category are
;; removed. If category is a defined symbol and name is a defined name, then all
;; entries for that name are removed.
(defgeneric clear-metagraph-definition (category name))

;; push the result of a define-graph form into the global metagraph db in
;; such a manner as to preserve undo status. This form becomes the active
;; metagraph-definition for that name.
(defgeneric push-metagraph-form (category name))

;; pop the specified category/name form from the metagraph-definition. The new
;; top becomes the active form. Popping what would be the last
;; metagraph-definition form results in that metagraph-definition being removed
;; from the metagraph db.
(defgeneric pop-metagraph-form (category name))

;; Get a list of all defined categories
(defgeneric all-metagraph-categories ())

;; Return a list of all active (and raw) metagraph-forms for the specified
;; category (not the metagraph form types, but the forms themselves). The result
;; is COPY-SEQed and so is editable by the user of this function.
(defgeneric all-active-metagraph-forms (category))

;; This is a user defined method (and core defines some too). It takes a list of
;; active and raw forms from the category and MUST return a single object that
;; represents the analyzed and concrete form of that graph that is ready for
;; use. The result is stored in core-state prolly in a hash table with the
;; category-name as the key and the result of this function as the value.
(defgeneric realize-metagraph-category (category definition-form-list))


;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Toplevel AST graph forms for generic graphs that all graph categories will
;; use. You can derive or add more to these.
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass graph-ast ())

(defclass graph-ast/graph (graph-ast)
  ((%definitions :reader definitions :initarg :definitions)))

(defclass graph-ast/definition (graph-ast)
  ((%category :reader category :initarg :category)
   (%depends-on :reader depends-on :initarg :depends-on)
   (%roots :reader roots :initarg :roots)
   (%weak-roots :reader roots :initarg :weak-roots)
   (%subforms :reader subforms :initarg :subforms)))

(defclass graph-ast/category (graph-ast))
(defclass graph-ast/roots (graph-ast))
(defclass graph-ast/weak-roots (graph-ast))
(defclass graph-ast/subforms (graph-ast))
(defclass graph-ast/subform (graph-ast))
(defclass graph-ast/depends-on (graph-ast))
(defclass graph-ast/depform (graph-ast))



;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; When the metagraphs are realized into
;; Concrete category types down to leaves.
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass graph-category ())

;; We care only about directed graphs so far in FL
(defclass directed-graph-category (graph-category) ())

;; And, some of those graphes are acyclic
(defclass directed-acyclic-graph-category (directed-graph-category) ())

;; Some leaf types: dags like textures/materials/components resolution paths
(defclass texture-resolution (directed-acyclic-graph-category) ())
(defclass material-resolution (directed-acyclic-graph-category) ())
(defclass component-resolution (directed-acyclic-graph-category) ())

;; Some leaf types: dags like component execution order.
(defclass component-execution (directed-acyclic-graph-category) ())


;; And, some of them are cyclic
(defclass directed-cyclic-graph-category (directed-graph-category) ())
;; All types of state machines, could possibly be terminal child depending
;; how the state is abstracted.
(defclass state-machine directed-graph-category ())
;; Some leaf-types: Animation state machines.
(defclass animation-machine (state-machine) ())


;; ;;;;;;;;;;;;;
;; metagraph realization protocol.
;; ;;;;;;;;;;;;;

(defgeneric parse-graph (category definition-form-list))
(defgeneric parse-graph-definition (category definition-form))
(defgeneric parse-graph-depends-on (category depends-on-form))

;; Transformation pass
;; -------------------
;; 1) Canonicalize all dependency-forms
;; 2) Segment all dependency-forms
;;
;; Analysis pass
;; -------------




























;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Core graphs
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Core graphs, each of a different category
;; a specific graph for finding textures.

;; The names in the first position all must be exported.
(define-graph core
    (:category fl:texture-resolution
     :depends-on nil
     :weak-roots (core))
  (search-path core
               (:first-light.textures)))

(define-graph core
    (:category fl:material-resolution
     :depends-on nil
     :weak-roots (core))
  (search-path core
               (:first-light.materials)))

(define-graph core
    (:category fl:component-resolution
     :depends-on nil
     :weak-roots (core))
  (search-path core
               (:first-light.components)))

;; component execution order
(define-graph core
    (:category fl:component-execution
     :depends-on nil
     :weak-roots (core)) ;; if not referenced, becomes a root.

  (execution-order all-unknown-types
                   ((unknown-types))) ;; (unknown-types) is special token

  (execution-order meshes
                   (fl.comp:mesh -> fl.comp:mesh-renderer))

  (execution-order core
                   (fl.comp:transform -> (splice meshes))))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Project graphs (that use the above)
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; and now, what the user specifies in their project, one for each category
(define-graph project
    (:category fl:texture-resolution
     :depends-on ((fl:core (core))) ;; fl:core is in context of category!
     :roots (all-textures))

  (search-path all-textures
               (:first-light.example -> core)))

(define-graph project
    (:category fl:material-resolution
     :depends-on ((fl:core (core)))
     :roots (all-materials))

  (search-path all-materials
               (:first-light.example -> core)))

(define-graph project
    (:category fl:component-resolution
     :depends-on ((fl:core (core)))
     :roots (all-component))

  (search-path all-components
               (:first-light.example.comp.*
                -> :first-light.example
                -> core)))

;; component execution order
(define-graph project
    (:category fl:component-execution
     :depends-on ((fl:core (all-unknown-types core)))
     :roots (start))

  (execution-order project ;; placeholder
                   nil)

  (execution-order start
                   ((splice core)
                    -> (splice project)
                    -> (splice all-unknown-types))))
