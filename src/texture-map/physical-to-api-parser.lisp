(in-package #:colony.texture-map)

(defgeneric physical->api (name model style store body))
(defgeneric physical-form-classifer (form-type model style store))

(defgeneric dslobjsym->constructor (sym)
  (:documentation
   "Map a DSL object symbol to another symbol which is the constructor
function for that object."))

(defgeneric gen-texture-map-form (name model style store
                                  &key anonymous-p data-elements-var-name
                                    mipmaps-var-name cube-var-name
                                    phys/attrs phys/cattrs phys/sattrs))

;;; ---------------------------------------------------------------------------
;; helper functions and methods.
;;; ---------------------------------------------------------------------------

(defun varname (prefix sid &key (suffix "") (pkg nil))
  (u:format-symbol pkg "~A~A~A"
                   (string-upcase prefix) sid (string-upcase suffix)))

(defun gen-data-span-varname (mipvar msvar suffix)
  (u:format-symbol nil "~A-~A-~A" mipvar msvar suffix))

(defmethod physical-form-classifier ((form-type (eql :body))
                                     model style store)
  (lambda (item)
    (cond
      ((and (listp item)
            (member (car item) '(sattrs cattrs attrs data-elements)))
       (car item))
      ;; squish all :mipmap-*d forms into a single 'mipmap bucket.
      ((and (listp item)
            (member (car item) '(mipmap-1d mipmap-2d mipmap-3d)))
       'mipmap)
      (t
       :unknown))))

(defmethod physical-form-classifier ((form-type (eql :mipmap))
                                     model style store)
  (lambda (item)
    (cond
      ((and (listp item)
            (member (car item) '(sattrs cattrs attrs)))
       (car item))
      ;; squish all :mapping-span-*d forms into a single 'mapping-span bucket.
      ((and (listp item)
            (member (car item) '(mapping-span-1d mapping-span-2d
                                 mapping-span-3d)))
       'mapping-span)
      ;; TODO: A hack to deal with a keyword arg.
      ;; squish all span-* forms into :extent
      ;; This only works because (span-* ...) only exist as an option to
      ;; :extent.
      ;; FIX sieve to take key specifications which can gather additional args.
      ((and (listp item)
            (member (car item) '(span-1d span-2d span-3d)))
       :extent)
      ;; TODO: A hack to deal with a keyword arg.
      ((eq item :extent)
       :extent-symbol)
      (t
       :unknown))))

(defmethod dslobjsym->constructor ((sym (eql 'image-element)))
  'make-image-element)
(defmethod dslobjsym->constructor ((sym (eql 'texture-map-element)))
  'make-texture-map-element)
(defmethod dslobjsym->constructor ((sym (eql 'data-elements)))
  'make-data-elements)
(defmethod dslobjsym->constructor ((sym (eql 'data-span-1d)))
  'make-data-span-1d)
(defmethod dslobjsym->constructor ((sym (eql 'data-span-2d)))
  'make-data-span-2d)
(defmethod dslobjsym->constructor ((sym (eql 'data-span-3d)))
  'make-data-span-3d)
(defmethod dslobjsym->constructor ((sym (eql 'mapping-span-1d)))
  'make-mapping-span-1d)
(defmethod dslobjsym->constructor ((sym (eql 'mapping-span-2d)))
  'make-mapping-span-2d)
(defmethod dslobjsym->constructor ((sym (eql 'mapping-span-3d)))
  'make-mapping-span-3d)
(defmethod dslobjsym->constructor ((sym (eql 'implicit/mapping-spans)))
  'make-mapping-spans)
(defmethod dslobjsym->constructor ((sym (eql 'span-1d)))
  'make-span-1d)
(defmethod dslobjsym->constructor ((sym (eql 'span-2d)))
  'make-span-2d)
(defmethod dslobjsym->constructor ((sym (eql 'span-3d)))
  'make-span-3d)
(defmethod dslobjsym->constructor ((sym (eql 'mipmap-1d)))
  'make-mipmap-1d)
(defmethod dslobjsym->constructor ((sym (eql 'mipmap-2d)))
  'make-mipmap-2d)
(defmethod dslobjsym->constructor ((sym (eql 'mipmap-3d)))
  'make-mipmap-3d)
(defmethod dslobjsym->constructor ((sym (eql 'implicit/mipmaps)))
  'make-mipmaps)
(defmethod dslobjsym->constructor ((sym (eql :1d)))
  'make-texture-map-1d)
(defmethod dslobjsym->constructor ((sym (eql :2d)))
  'make-texture-map-2d)
(defmethod dslobjsym->constructor ((sym (eql :3d)))
  'make-texture-map-3d)


;; ------- data-element PHYS->API generation


(defun gen-data-element-form (phys/element-form)
  "Convert the physical syntax form of a PHYS/ELEMENT-FORM into a API syntax
 form and return it."
  (destructuring-bind (sym &key logloc physloc element) phys/element-form
    ;; TODO: The quoted logloc form indicates to me that the asset form needs
    ;; evaluable specification forms.
    `(,(dslobjsym->constructor sym)
      ,@(when logloc `(:logloc ',logloc))
      ,@(when physloc `(:physloc ,physloc))
      ,@(when element `(:element ,element)))))

(defun gen-data-element-binding-form (var phys/element-form)
  "Convert the physical syntax form of the PHYS/ELEMENT-FORM into an API
syntax form. Produce a LET binding form with the VAR as the variable name and
the API form as the value and return that new form."
  `(,var ,(gen-data-element-form phys/element-form)))

(defun gen-data-elements-form (sym &rest element-var-names)
  "Given the physical name of the data-elements form in SYM and a list of
previously created ELEMENT-VAR-NAMES, return a LET* binding form which
constructe the data-elements container and initializes it with the
ELEMENT-VAR-NAMES."
  `(,(dslobjsym->constructor sym) :encode ,@element-var-names))

(defun gen-data-elements-binding-form (phys/data-elements)
  "Convert the physical syntax form of PHYS/DATA-ELEMENTS into two values.
The first value is a list of appropriate LET* bindings of the individual
physical data-element forms which have been converted to the API
representation.  The second value will be the data-element container encoding
the binding variables for each binding form in the first value."
  (destructuring-bind (sym . elems) phys/data-elements
    (let* ((element-bindings
             (loop
               :for (sid elem) :in elems
               :collect (gen-data-ele2ment-binding-form
                         (varname "de" sid) elem)))
           (de-form
             `(,(varname "data-elements" 0)
               (,@(apply #'gen-data-elements-form sym
                         (mapcar 'first element-bindings))))))
      (values element-bindings de-form))))

;; ------- data-span & mapping-span PHYS->API generation

(defun gen-data-span-form (phys/data-span-form)
  "Return a single form which is the programmatic API translation of the
physical PHYS/DATA-SPAN-FORM representation. Any nonsupplied initargs are left
unspecified in the translation."
  (destructuring-bind (sym &key origin extent elidx) phys/data-span-form
    `(,(dslobjsym->constructor sym)
      ,@(when origin `(:origin ,origin))
      ,@(when extent `(:extent ,extent))
      ,@(when elidx `(:elidx ,elidx)))))

(defun gen-data-span-binding-form (var phys/data-span-form)
  "Perform a conversion of the physical syntax form of the PHYS/DATA-SPAN-FORM
argument to the API syntax form. Then produce a LET binding of that API
form to the VAR. If the PHYS/DATA-SPAN-FORM is NIL, then return NIL."
  (when phys/data-span-form
    `(,var ,(gen-data-span-form phys/data-span-form))))

(defun gen-mapping-span-form (mipvar msvar phys/mapping-span-form)
  "Return two values. The first value is a list of let-bindings which construct
any specified data spans. The second value is the single PHYS/MAPPING-SPAN-FORM
form converted to the programmatic API. The mapping span form will have
references to the variables in the first value's let-binding forms if they are
present."
  (destructuring-bind (sym &key to from) phys/mapping-span-form
    (let ((to-binding-form
            (gen-data-span-binding-form
             (gen-data-span-varname mipvar msvar "TO") to))
          (from-binding-form
            (gen-data-span-binding-form
             (gen-data-span-varname mipvar msvar "FROM") from)))
      (values
       ;; The bindings for any data-spans present.
       (remove-if #'null (list to-binding-form from-binding-form))
       ;; The actual mapping-span form.
       `(,(dslobjsym->constructor sym)
         ,@(when to-binding-form `(:to ,(first to-binding-form)))
         ,@(when from-binding-form `(:from ,(first from-binding-form))))))))

(defun gen-mapping-span-binding-form (mipvar msvar phys/mapping-span)
  "Return two values. The first value is the list of LET bindings which bind
the API syntax forms of the data-spans found (if any) in the single
PHYS/MAPPING-SPAN form. The second value is a LET binding form of the api
syntax form of the PHYS/MAPPING-SPAN itself. The variable to which is it bound
is named MIPVAR-MSVAR."
  (let ((var (u:format-symbol nil "~A-~A" mipvar msvar)))
    (multiple-value-bind (api/data-span-bindings api/mapping-span)
        (gen-mapping-span-form mipvar msvar phys/mapping-span)
      (values api/data-span-bindings
              `(,var ,api/mapping-span)))))

;; ------- mapping-spans PHYS->API generation

(defun gen-mapping-spans-form (sym &rest mapping-span-var-names)
  "Construct an API syntax form of the collection of mapping spans represented
by the MAPPING-SPAN-VAR-NAMES. SYM in this context must be the symbol
TEXMAP:IMPLICIT/MAPPING-SPANS. Return the API syntax form."
  `(,(dslobjsym->constructor sym) :encode ,@mapping-span-var-names))


(defun gen-mapping-spans-binding-form (mipvar &rest mapping-span-var-names)
  "Return a the LET binding form of the mapping-spans container encoding the
MAPPING-SPAN-VAR-NAMES and bound to a variable MIPVAR with '-MSPANS' appended
to it."
  (let ((var (u:format-symbol nil "~A-MSPANS" mipvar)))
    `(,var ,(apply #'gen-mapping-spans-form 'implicit/mapping-spans
                   mapping-span-var-names))))

(defun gen-mapping-spans-binding-group (mipvar phys/mapping-span-forms)
  "Return two values. The first value is all data-span and mapping-span
binding forms in order of declaration. The second form is the form for the
mapping spans container form which references all of the the mapping-span
variables in the first value."

  (let ((all-bindings ())
        (ms-binding-vars ()))
    (loop :for phys/mapping-span-form :in phys/mapping-span-forms
          :for index :by 1
          :for msvar = (varname "ms" index :pkg nil)
          :do (multiple-value-bind (ds-bindings ms-form)
                  (gen-mapping-span-binding-form mipvar msvar
                                                 phys/mapping-span-form)
                ;; keep track of the data-span bindings.
                (dolist (dsb ds-bindings)
                  (push dsb all-bindings))

                ;; keep track of the mapping-span bindings.
                (push ms-form all-bindings)

                ;; ALSO keep track of the mapping span variable names!
                ;; We need these to produce the final mapping-spans container.
                (push (car ms-form) ms-binding-vars)))
    (values
     (nreverse all-bindings)
     (apply #'gen-mapping-spans-binding-form mipvar
            (nreverse ms-binding-vars)))))

;; ------- span PHYS->API generation

(defun gen-span-form (phys/span-form)
  "Convert the physical syntax form of PHYS/SPAN-FORM into an API syntax form
and return it."
  (destructuring-bind (sym &key origin extent) phys/span-form
    `(,(dslobjsym->constructor sym)
      ,@(when origin `(:origin ,origin))
      ,@(when extent `(:extent ,extent)))))

(defun gen-span-binding-form (var phys/span-form)
  "Convert the physical syntax form of PHYS/SPAN-FORM to the API syntax form
And produce a LET binding form for it with VAR as the variable name."
  `(,var ,(gen-span-form phys/span-form)))


;; ------- mipmap PHYS->API generation

(defun gen-mipmap-form (sym &key extent mapping-spans)
  `(,(dslobjsym->constructor sym)
    ,@(when extent `(:extent ,extent))
    ,@(when mapping-spans `(:mapping-spans ,mapping-spans))))

(defun gen-mipmap-binding-group (mipvar phys/mipmap model style store)
  "Return five values. The first value is any ATTRS form if present. The
second value is any CATTRS form if present. The third value is any SATTRS form
if present, the fourth value is the list of LET binding forms for any
data-spans, mapping-spans, etc, which this mipmap requires. The fifth form is
the mipmap creation form itself which uses ultimately everything in LET binding
forms."
  (let* ((mipmap-key-pool '(attrs cattrs sattrs :extent :extent-symbol
                            mapping-span :unknown)))
    (destructuring-bind (sym &rest mipmap-body) phys/mipmap
      (multiple-value-bind (attrs cattrs sattrs extent extent-symbol
                            phys/mapping-spans unknown)
          ;; TODO CLUNKY (misuse of current SIEVE functionality)
          ;; Partition the body into chunks
          ;;
          ;; TODO: Add &key handling to the key pool to pick off &key args.
          (partition-a-dsl-form mipmap-key-pool
                                ;; TODO CLUNKY (misuse of current SIEVE
                                ;; functionality)
                                (physical-form-classifier
                                 :mipmap model style store)
                                mipmap-body)
        (declare (ignore extent-symbol))
        (when (plusp (length unknown))
          (error "Unknown mipmap physical form: ~A" unknown))

        (multiple-value-bind (mapping-spans-api-group
                              mapping-spans-binding-form)
            (gen-mapping-spans-binding-group mipvar phys/mapping-spans)
          (let* ((extent-var (u:format-symbol nil "~A-EXTENT" mipvar))
                 (extent-binding-form
                   (gen-span-binding-form extent-var (car extent)))
                 (mapping-span-var (car mapping-spans-binding-form))
                 (mipmap-form
                   (gen-mipmap-form sym :extent extent-var
                                        :mapping-spans mapping-span-var)))
            (values attrs
                    cattrs
                    sattrs
                    ;; NOTE: This next form is the binding-group required for
                    ;; the mipmap form to exist.
                    (append mapping-spans-api-group
                            (list mapping-spans-binding-form)
                            (list extent-binding-form))
                    ;; finally the mipmap creation form.
                    mipmap-form)))))))

(defun gen-mipmaps-form (sym &rest mipmap-var-names)
  "Construct an API syntax form of the collection of mipmaps represented
by the mipmap-var-names. SYM in this context must be the symbol
TEXMAP:IMPLICIT/MIPMAPS. Return the API syntax form."
  `(,(dslobjsym->constructor sym) :encode ,@mipmap-var-names))

(defun gen-mipmaps-binding-form (mipmaps-container-var sym mipmap-var-names)
  "Construct a LET binding form with MIPMAPS-CONTAINER-VAR as the variable and
the a mipmaps container form built from SYM and the MIPMAP-VAR-NAMES variable
list."
  `(,mipmaps-container-var
    ,(apply #'gen-mipmaps-form sym mipmap-var-names)))

(defun gen-mipmaps-binding-group (mipmaps-var phys/mipmaps model style store)
  "Return four values. The first value is a list of bindings to create all the
mipmaps in the MIPMAPS form (including bindings for the mapping-spans and any
required data-spans).

The second value is the let bindings form which constructs the mipmap container
into which the individual mipmap variables are encoded. The first element of
that list is the variable binding and the second the mipmaps container
construction form.

The third value is a hashtable of attribute bag absorption forms to implement
the attribute inheritance correctly for each mipmap. The hash key is the mipmap
variable and the hash value is the list
 (:ATTRS <attrs-form> :CATTRS <cattrs-form> :SATTRS <sattrs-form>)
with each form being NIL if there aren't any in that category.

 The fourth value is all the mipvars in order of encoding (so they can be used
as hash keys in the right order)."

  (let ((atbl (u:dict #'eq))
        (mipvars nil)
        (all-binding-groups nil))
    (loop :for phys/mipmap :in phys/mipmaps
          :for id :from 0
          :for mipvar = (varname "mip" id)
          :do (multiple-value-bind (attrs cattrs sattrs bindings mipmap-form)
                  (gen-mipmap-binding-group mipvar phys/mipmap
                                            model style store)
                ;; Store the mipvar for later encoding into mipmap container.
                (push mipvar mipvars)
                ;; Associate the mipvar with the attrs it might need.
                (setf (u:href atbl mipvar)
                      (list :attrs attrs :cattrs cattrs :sattrs sattrs))
                ;; construct a binding group and give a varname to the mipmap
                ;; binding form, store in list.
                (push (append bindings `((,mipvar ,mipmap-form)))
                      all-binding-groups)))
    (let* ((rev-mipvars (nreverse mipvars))
           (v0 (mapcan #'list* (nreverse all-binding-groups)))
           (v1 (gen-mipmaps-binding-form mipmaps-var
                                         'implicit/mipmaps rev-mipvars))
           (v2 atbl)
           (v3 rev-mipvars))
      (values v0 v1 v2 v3))))

(defun gen-attribute-eval-form (attr-forms)
  "When given a list of attribute key/value pairs, return a
transformation that rebuilds it via list forms where both the key and the value
are both evaluated."
  `(list ,@(loop :for (k v) :in attr-forms
                 :collect `(list ,k ,v))))

;; Used for 1d, 2d, 3d texture map forms.
(defmethod gen-texture-map-form (name model style store
                                 &key anonymous-p data-elements-var-name
                                   mipmaps-var-name cube-var-name
                                   phys/attrs phys/cattrs phys/sattrs)
  (when cube-var-name
    (error
     "gen-texture-map-form: Cannot use a cube map with a ~S texture model."
     model))

  `(,(dslobjsym->constructor model)
    :name ',name
    ,@(when anonymous-p `(:anonymous-p ,anonymous-p))
    :model ',model
    :style ',style
    :store ',store
    ;; TODO: The cdar forms probably should be fixed up. They are a little
    ;; dirty.
    ,@(when data-elements-var-name
        `(:data-elements ,data-elements-var-name))
    ,@(when mipmaps-var-name
        `(:mipmaps ,mipmaps-var-name))
    ,@(when phys/attrs
        `(:attrs ,(gen-attribute-eval-form (cdar phys/attrs))))
    ,@(when phys/cattrs
        `(:cattrs ,(gen-attribute-eval-form (cdar phys/cattrs))))
    ,@(when phys/sattrs
        `(:sattrs ,(gen-attribute-eval-form (cdar phys/sattrs))))))


;; Used for :cube maps (gotta analyze faces and mipmaps in a special way)
(defmethod gen-texture-map-form (name (model (eql :cube)) style store
                                 &key anonymous-p data-elements-var-name
                                   mipmaps-var-name cube-var-name
                                   phys/attrs phys/cattrs phys/sattrs)
  nil)


;; Used for :1d, :2d, :3d textures.
(defmethod gen-texture-map-binding-group (name model style store body)
  "Return three values. The first value is the complete binding group to build
the texture using the texmap API. The second value is the symbol of the
variable of the bound texture-map. The third value is all of the absorption
forms for any mipmap attributes."

  (let* ((texture-map-key-pool '(attrs cattrs sattrs data-elements
                                 mipmap :unknown))
         (mipmaps-var (varname "mipmaps" 0))
         (texture-var (varname "texture" 0)))
    (multiple-value-bind (phys/attrs phys/cattrs phys/sattrs
                          phys/data-elements phys/mipmaps unknown)
        ;; Partition the body into chunks
        (partition-a-dsl-form texture-map-key-pool
                              (physical-form-classifier
                               :body model style store)
                              body)
      ;; Validation
      (when (plusp (length unknown))
        (error "Unknown texture-map physical form: ~A : ~A" name unknown))
      (unless (= (length phys/data-elements) 1)
        (error "Need ONE data-elements form for texture-map physical form: ~A"
               name))

      (multiple-value-bind (de-let-bindings de-container-binding)
          ;; NOTE: There is only one data-elements form, and this function
          ;; call wants ONLY that form.
          (gen-data-elements-binding-form (first phys/data-elements))
        (multiple-value-bind (mip-let-bindings mip-container-binding
                              mip-attrs mipvars)
            (gen-mipmaps-binding-group mipmaps-var phys/mipmaps model
                                       style store)
          (let ((texmap-form
                  (gen-texture-map-form
                   name model style store
                   :phys/sattrs phys/sattrs
                   :phys/cattrs phys/cattrs
                   :phys/attrs phys/attrs
                   :data-elements-var-name (first de-container-binding)
                   :mipmaps-var-name mipmaps-var)))

            (let ((texture-binding-group
                    ;; Produce the complete binding group
                    (append de-let-bindings
                            (list de-container-binding)
                            mip-let-bindings
                            (list mip-container-binding)
                            (list (list texture-var texmap-form))))

                  ;; Now, produce the absorption forms, if any
                  (mipmap-absorption-forms
                    (let ((forms nil))
                      ;; TODO FIXME. Do this in order of mipvars
                      (loop :for var :in mipvars
                            :for attr-spec = (u:href mip-attrs var)
                            :do
                               (destructuring-bind (&key sattrs cattrs attrs)
                                   attr-spec
                                 (push
                                  `(abag:absorb
                                    ,var
                                    :bags ,texture-var
                                    ;; TODO: the cdar is a little dirty. Mayhap
                                    ;; have to fixup how the dataflow works in
                                    ;; the partitioning concerning the
                                    ;; attributes.
                                    ,@(when sattrs
                                        `(:sattrs ,(gen-attribute-eval-form
                                                    (cdar sattrs))))
                                    ,@(when cattrs
                                        `(:cattrs ,(gen-attribute-eval-form
                                                    (cdar cattrs))))
                                    ,@(when attrs
                                        `(:attrs ,(gen-attribute-eval-form
                                                   (cdar attrs)))))
                                  forms)))
                      (nreverse forms))))

              (values texture-binding-group
                      texture-var
                      mipmap-absorption-forms))))))))


;; Used for :cube textures.
(defmethod gen-texture-map-binding-group (name (model (eql :cube)) style
                                          store body)
  nil)

;;; ---------------------------------------------------------------------------
;; 1d, 2d, 3d logical texture conversion.
;;; ---------------------------------------------------------------------------

(defmethod physical->api (name model style store body)

  ;; TODO: Figure out how to wedge a context into the dsl, then
  ;; wrap form in lambda to specify context.

  (multiple-value-bind (all-bindings texture-var absorption-forms)
      (gen-texture-map-binding-group name model style store body)
    `(let* ,all-bindings
       ,@absorption-forms
       ,texture-var)))

;;; ---------------------------------------------------------------------------
;; Sort of unit tests.
;;; ---------------------------------------------------------------------------

(defun test-phys-to-api/g000-1d-log-inf-one-non ()
  (let* ((name 'g000-1d-log-inf-one-non)
         (model :1d)
         (style :unique)
         (store nil))
    (physical->api
     name model style store
     ;; body
     '((cattrs ("foo" 100) ('qux 12))
       (data-elements
        (0 (image-element :logloc (textures 1d-64x1))))

       (mipmap-1d
        (cattrs ("foo" 42) ('bar 100))
        :extent (span-1d :origin 0 :extent 64)
        (mapping-span-1d :to (data-span-1d :origin 0 :extent 64)
                         :from (data-span-1d :origin 0 :extent 64
                                             :elidx 0)))))))
