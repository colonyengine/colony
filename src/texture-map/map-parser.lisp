(in-package #:virality.texture-map)

;; The texture-map internal protocol. Prolly needs some improvement!
(defgeneric parse-data-model (data-model))
(defgeneric parse-name (name model style store))
(defgeneric parse-attribute-form (model style store bag form))
(defgeneric parse-attribute-forms (model style store bag attribute-forms))
(defgeneric parse-data-element (model style store tag element))
(defgeneric parse-data-elements (model style store tag dstore elements))
(defgeneric parse-data-form (model style store tag attributes elements
                             tmap-var))
(defgeneric parse-data-forms (model style store data-store-forms dstore-place
                              tmap-var))
(defgeneric parse-body (model style store tmap-var body))
(defgeneric parse-texture-map (name model style store body))
(defgeneric default-model-attributes (model style store))
(defgeneric data-store-form-p (model style store form))
(defgeneric texture-map-class-name (model style store))
(defgeneric data-store-class-name (model style store tag))
(defgeneric data-store-after-initialize (model style store dstore))

;;; -----------------------------------------------------------------------
;; Methods for processing of any texture-map type
;;; -----------------------------------------------------------------------

;; return a form which additionally initializes the dstore after its creation.
(defmethod data-store-after-initialize (model style store dstore)
  nil)

;; Return a form which sets the attribute data into the bag treating the bag
;; like an attribute-bag instance.
(defmethod parse-attribute-form (model style store bag form)
  (destructuring-bind (attr-name value) form
    `(setf (abag:sattr ,bag ,attr-name) ,value)))

;; Return a list of setf forms which set each attribute into the appropriate
;; slot on the var.
(defmethod parse-attribute-forms (model style store bag attribute-forms)
  (loop :for attr-form :in attribute-forms
        :collect (parse-attribute-form model style store bag attr-form)))

;;; -----------------------------------------------------------------------
;; For any texture-maps that use the (tag (prop*) data-elements*) data-store
;; form, they are mostly processed the same way. For any texture-map which
;; deviates from this generalized parsing, we implement a specialization.
;;; -----------------------------------------------------------------------

;; A data element is often a form interpreted later at runtime. It is usually a
;; symbol or asset form and it is expected to be quoted. The second expression
;; in the values is the AST of the element, if one can be computed. Currently
;; there is only one place where thic can happen--when processing the
;; data-elements for cube maps which contains a define-texture-map definition.
;; Please see the appropriate parse-data-element method for this scenario.
(defmethod parse-data-element (model style store tag element)
  `(values (make-data-element :original ',element) nil))

;; Return a form that stores each data-element into the appropriate index in
;; the data store. When evaluated this form should produce a list of the AST
;; forms which may have been generated when processing the elements.
(defmethod parse-data-elements (model style store tag dstore elements)
  (u:with-gensyms (asts)
    `(let ((,asts nil))
       ,@(loop
           :for elem :in elements
           :for index :from 0
           :collect (u:with-gensyms (e ast)
                      `(multiple-value-bind (,e ,ast)
                           ,(parse-data-element model style store tag elem)
                         (setf (aref (data-elements ,dstore) ,index) ,e)
                         (when ,ast
                           (push ,ast ,asts)))))
       (nreverse ,asts))))

;; return a lisp form which sets up and initializes the data-store and puts the
;; data-elements into it.
(defmethod parse-data-form (model style store tag attributes elements tmap-var)
  (u:with-gensyms (dstore asts)
    `(let ((,dstore (make-data-store
                     ',(data-store-class-name model style store tag)
                     ,(length elements))))

       ,@(data-store-after-initialize model style store dstore)

       ;; Overlay all known attributes from tmap into data-store.
       (abag:overlay ,dstore ,tmap-var)

       ;; Fill in the attributes specific to this data-store form.
       ,@(parse-attribute-forms model style store dstore attributes)

       ;; Finally, store all the discovered data-elements into the data-store
       ;; and percolate the collected ASTs to the evaluator of this form.
       (let ((,asts
               ,(parse-data-elements model style store tag dstore elements)))
         (values ,dstore ,asts)))))

;; Return a form which parses the data-store-forms (whatever they loo like)
;; and stores them into the tmap-var. The returned form must return two values
;; when it is evaluated. The first value is the tmap-var, and the second is
;; a list of collected ASTs from data-elements that specifically defined
;; new texture-maps.

(defmethod parse-data-forms (model style store data-store-forms
                             dstore-place tmap-var)
  (u:with-gensyms (all-asts)
    `(let ((,all-asts nil))
       ,@(loop :for dsform :in data-store-forms
               :for index :from 0
               :collect
               (u:with-gensyms (dstore ast asts)
                 `(multiple-value-bind (,dstore ,asts)
                      ,(destructuring-bind (ds-tag ds-attributes
                                            . ds-elements)
                           dsform
                         (parse-data-form model style store
                                          ds-tag ds-attributes
                                          ds-elements
                                          tmap-var))
                    (u:comment "Store specific data store into tmap's dstore")
                    (setf (aref (data-elements
                                 ,dstore-place)
                                ,index)
                          ,dstore)
                    (u:comment "Save off any ASTs we computed.")
                    (dolist (,ast ,asts)
                      (push ,ast ,all-asts)))))
       (values ,tmap-var ,all-asts))))

;; dstore-place -> (data-store ,tmap-var)


(defmethod parse-body (model style store tmap-var body)
  ;; We separate out the attribute-forms from the data-form since we must
  ;; process the attribute forms first.
  (multiple-value-bind (attribute-forms data-store-forms)
      (loop :for form :in body
            :for data-form-p = (data-store-form-p model style store form)
            :when data-form-p
              :collect form :into data-forms
            :when (not data-form-p)
              :collect form :into attr-forms
            :finally
               (return (values attr-forms data-forms)))

    (format t "attribute-forms: ~S~%" attribute-forms)
    (format t "data-store-forms: ~S~%" data-store-forms)

    (let ((default-attr-setter-forms
            (parse-attribute-forms model style store tmap-var
                                   (default-model-attributes model style
                                                             store)))
          (user-attr-setter-forms
            (parse-attribute-forms model style store tmap-var
                                   attribute-forms)))

      (format t "default-attr-setter-forms: ~S~%" default-attr-setter-forms)
      (format t "user-attr-setter-forms: ~S~%" user-attr-setter-forms)

      ;; The list of store forms which side effect the DSL's data into the
      ;; texture-map and its data store.
      `(
        (u:comment "Allocate the texture-map general data-store instance.")
        (setf (data-store ,tmap-var)
              (make-data-store 'data-store ,(length data-store-forms)))

        (u:comment "Default texture-map attributes.")
        ,@default-attr-setter-forms

        (u:comment "User specified texture-map attributes.")
        ,@user-attr-setter-forms

        (u:comment "Individual data-store forms."
          "Store each one into the appropriate spot in the texture-map's"
          "data-store slot.")
        ,(parse-data-forms model style store data-store-forms
                           `(data-store ,tmap-var) tmap-var)

        ))))


;; Perform a transformation on the texture-map name.
;; Return two values: First value is the canonical name as a transformation of
;; the passed in name. When name is NIL, this will be a gensym'ed name.
;; Currently it must be a symbol. The second value is T if the name was the
;; anonymous name of NIL or the result if NIL otherwise.
(defmethod parse-name (name model style store)
  (if name
      (values name nil)
      (values (gensym "ANON-TEX-MAP") t)))

;; This basically must be for all texture-map kinds.
(defmethod parse-data-model (data-model)
  (destructuring-bind (model style . store)
      (or data-model '(:single :combined))
    (values model style store)))

(defmethod parse-texture-map (name model style store body)
  ;; Generate the texture-map object and add attribute information.
  (u:with-gensyms (canon-name anonymous-p tmap)
    `(multiple-value-bind (,canon-name ,anonymous-p)
         (parse-name ',name ',model ',style ',store)
       (let ((,tmap
               ;; Create the specific texture map instance we need that
               ;; represents the processing of this texture-map.
               (make-texture-map ',(texture-map-class-name model style store)
                                 :name ,canon-name
                                 :anonymous-p ,anonymous-p
                                 :model ',model
                                 :style ',style
                                 :store ',store)))

         ;; Inline whatever I get back from parsing the body.
         ;; The toplevel form of this will be a VALUES that must conform
         ;; to the requirements of parse-texture-map.
         ,@(parse-body model style store tmap body)))))


;;; -----------------------------------------------------------------------
;; Processing of :single texture-maps
;;; -----------------------------------------------------------------------

;; given the arguments, return a symbol which is the class name of the
;; texture-map instance we must create.
(defmethod texture-map-class-name ((model (eql :single)) style store)
  'texture-map-single)

(defmethod data-store-class-name ((model (eql :single)) style store
                                  (tag (eql :mipmap)))
  'mipmap-store)

;; Return an alist-big of attributes and their default values.
(defmethod default-model-attributes ((model (eql :single)) style store)
  '((:context nil)
    (:origin :top-left)
    (:channel-layout :combined)))

;; Return generalized boolean if form is a data-form, or NIL otherwise.
(defmethod data-store-form-p ((model (eql :single)) style store form)
  (when (listp form)
    (member (first form) '(:mipmap))))

;;; -----------------------------------------------------------------------

(defmacro with-tmap-test-form (&body body)
  `(values :macroform ',@body :expansion (macroexpand ',@body)))

(defun test-single-unique-0 ()
  (with-tmap-test-form
    (v:define-texture-map foo (:single :unique)
      (:mipmap () (textures foo-0))
      (:mipmap () (textures foo-1))
      (:mipmap () (textures foo-2))
      (:mipmap () (textures foo-3))
      (:mipmap () (textures foo-4))
      (:mipmap () (textures foo-5))
      (:mipmap () (textures foo-6))
      (:mipmap () (textures foo-7))
      (:mipmap () (textures foo-8))
      (:mipmap () (textures foo-9))
      (:mipmap () (textures foo-10)))))

(defun test-single-combined-0 ()
  (with-tmap-test-form
    (v:define-texture-map foo () ;; defaults to (:single :combined)
      (:mipmap () (textures foo-mip)))))

(defun test-single-combined-1 ()
  (with-tmap-test-form
    (v:define-texture-map foo (:single :combined)
      (:mipmap () (textures foo-mip)))))

;;; -----------------------------------------------------------------------
;; Processing of :rect texture-maps
;;; -----------------------------------------------------------------------

(defmethod texture-map-class-name ((model (eql :rect)) style store)
  'texture-map-rect)

(defmethod data-store-class-name ((model (eql :rect)) style store
                                  (tag (eql :image)))
  'image-store)

(defmethod default-model-attributes ((model (eql :rect)) style store)
  `((:context nil)
    (:origin :top-left)
    (:channel-layout :combined)))

(defmethod data-store-form-p ((model (eql :rect)) style store form)
  (when (listp form)
    (member (first form) '(:image))))

;;; -----------------------------------------------------------------------

(defun test-rect-unique-0 ()
  (with-tmap-test-form
    (v:define-texture-map foo (:rect :unique)
      (:image () (textures height-field)))))

;;; -----------------------------------------------------------------------
;; Processing of :buffer texture-maps
;;; -----------------------------------------------------------------------

(defmethod texture-map-class-name ((model (eql :buffer)) style store)
  'texture-map-buffer)

(defmethod data-store-class-name ((model (eql :buffer)) style store
                                  (tag (eql :name)))
  'buffer-name-store)

(defmethod default-model-attributes ((model (eql :buffer)) style store)
  `())

(defmethod data-store-form-p ((model (eql :buffer)) style store form)
  (when (listp form)
    (member (first form) '(:name))))

;;; -----------------------------------------------------------------------

(defun test-buffer-unique-0 ()
  (with-tmap-test-form
    (v:define-texture-map foo (:buffer :unique)
      (:name () :foobar))))

;;; -----------------------------------------------------------------------
;; Processing of :buffer texture-maps
;;; -----------------------------------------------------------------------

(defmethod texture-map-class-name ((model (eql :voxel)) style store)
  'texture-map-voxel)

(defmethod data-store-class-name ((model (eql :voxel)) style store
                                  (tag (eql :mipmap)))
  'mipmap-store)

(defmethod default-model-attributes ((model (eql :voxel)) style store)
  `((:context nil)
    (:origin :bottom-left-back)
    (:channel-layout :combined)))

(defmethod data-store-form-p ((model (eql :voxel)) style store form)
  (when (listp form)
    (member (first form) '(:mipmap))))

;;; -----------------------------------------------------------------------

(defun test-voxel-unique-0 ()
  (with-tmap-test-form
    (v:define-texture-map 3d (:voxel :unique (:slices :back-to-front))
      ;; mipmap level 0
      (:mipmap ()
               (textures 3d-slice-0-0)
               (textures 3d-slice-1-0)
               (textures 3d-slice-2-0)
               (textures 3d-slice-3-0)
               (textures 3d-slice-4-0)
               (textures 3d-slice-5-0)
               (textures 3d-slice-6-0)
               (textures 3d-slice-7-0))

      ;; mipmap level 1
      (:mipmap ()
               (textures 3d-slice-0-1)
               (textures 3d-slice-1-1)
               (textures 3d-slice-2-1)
               (textures 3d-slice-3-1))

      ;; mipmap level 2
      (:mipmap ()
               (textures 3d-slice-0-2)
               (textures 3d-slice-1-2))

      ;; mipmap level 3
      (:mipmap ()
               (textures 3d-slice-0-3)))))


;;; -----------------------------------------------------------------------
;; Processing of :cube texture-maps
;;; -----------------------------------------------------------------------

;;; ----
;; These method support general cube maps of any style or model
;;; ----

(defmethod texture-map-class-name ((model (eql :cube)) style store)
  'texture-map-cube)

(defmethod data-store-class-name ((model (eql :cube)) style store tag)
  'cube-store)

(defmethod data-store-after-initialize ((model (eql :cube)) style store
                                        (dstore cube-store))
  `((setf (style ,dstore) ,style
          (store ,dstore) ,store)))

;;; ----
;; These methods support specialized :unique style cube maps that use :face
;; data-store forms.
;;; ----

(defmethod default-model-attributes ((model (eql :cube)) (style (eql :unique))
                                     store)
  `())

(defmethod data-store-form-p ((model (eql :cube)) (style (eql :unique))
                              store form)
  (when (listp form)
    (member (first form) '(:face))))

(defmethod data-store-class-name ((model (eql :cube)) (style (eql :unique))
                                  store (tag (eql :face)))
  'face-store)

;; A :cube :unique data-element can be a name for a previously defined
;; texture-map or an entire anonymous texture-map defined in place. So we
;; specialize on it to handle that functionality here.
(defmethod parse-data-element ((model (eql :cube)) (style (eql :unique))
                               store (tag (eql :face)) element)

  (u:with-gensyms (data-element sub-tmap sub-extra-tmap-list)
    `(let* ((,data-element (make-data-element)))
       ,@(cond
           ;; Just a simple texture-map name, we're done!
           ((symbolp element)
            `((setf (original ,data-element) ',element)
              (values ,data-element nil)))

           ;; A complete define-texture-map form, handle appropriately.
           ((and (listp element)
                 (eq (first element) 'define-texture-map))

            ;; Process the define-texture-map and handle the results of it.
            (format t "About to destructure: ~S~%" element)
            (destructuring-bind (sub-tag sub-name
                                 (sub-model sub-style . sub-store)
                                 . sub-body)
                element
              (declare (ignore sub-tag))
              `((u:comment "Parse the sub texture-map for this face.")
                (multiple-value-bind (,sub-tmap ,sub-extra-tmap-list)
                    ,(parse-texture-map
                      (parse-name sub-name sub-model sub-style
                                  sub-store)
                      sub-model sub-style sub-store
                      sub-body)

                  (u:comment "Store reference to texture map name.")
                  (setf (original ,data-element) (texmap::name ,sub-tmap))
                  ;; NOTE: In this situation, a cube map cannot recursively
                  ;; define cube maps or other such things. So we're expecting
                  ;; the sub-extra-tmap-list to be empty here.
                  (assert (null ,sub-extra-tmap-list))
                  (values ,data-element ,sub-tmap)))))

           (t (error "parse-data-element: Unknown element form: ~A"
                     element))))))

(defmethod parse-body ((model (eql :cube)) (style (eql :unique)) store
                       tmap-var body)
  ;; We separate out the attribute-forms from the data-form since we must
  ;; process the attribute forms first.
  (multiple-value-bind (attribute-forms data-store-forms)
      (loop :for form :in body
            :for data-form-p = (data-store-form-p model style store form)
            :when data-form-p
              :collect form :into data-forms
            :when (not data-form-p)
              :collect form :into attr-forms
            :finally
               (return (values attr-forms data-forms)))

    ;; TODO: Error handling needs to be better, but basically, a :unique style
    ;; requires 6 face data-store forms.
    (assert (= (length body) 6))

    ;; Process the locally specified attribute forms first.
    (let ((default-attr-setter-forms
            (parse-attribute-forms model style store tmap-var
                                   (default-model-attributes model style
                                                             store)))
          (user-attr-setter-forms
            (parse-attribute-forms model style store tmap-var
                                   attribute-forms)))

      (u:with-gensyms (cube-store)
        `((u:comment "Allocate the texture-map general data-store instance.")
          (setf (data-store ,tmap-var)
                (make-data-store 'data-store 1))

          (u:comment "Allocate a single cube-store to hold all faces.")
          (let ((,cube-store (make-data-store 'cube-store 6)))
            ,@(data-store-after-initialize model style store cube-store)

            (u:comment "Arrange to store the cube-store instance.")
            (setf (aref (data-elements (data-store ,tmap-var)) 0)
                  ,cube-store)

            (u:comment "Default attributes.")
            ,@default-attr-setter-forms

            (u:comment "User specified attributes.")
            ,@user-attr-setter-forms

            (u:comment "Store each of the 6 faces into the cube-store.")
            ,(parse-data-forms model style store data-store-forms
                               cube-store
                               tmap-var)
            ))))))

;;`(setf (aref (data-elements ,cube-store) ,index)

#|
;;; ----
;; These methods support specialized :combined cube maps where all faces are
;; encoded into a single image (which may have mipmaps). These data-forms use
;; the :mipmap form.
;;; ----

;; TODO: Should this just exist for all kinds of :cubes?
(defmethod default-model-attributes ((model (eql :cube))
(style (eql :combined))
store)
(u:dict))

;; Return generalized boolean if form is a data-form, or NIL otherwise.
(defmethod data-store-form-p ((model (eql :cube))
(style (eql :combined))
store form)
(when (listp form)
(member (first form) '(:mipmap))))
|#


;;; -----------------------------------------------------------------------

(defun test-cube-unique-six-0 ()
  (with-tmap-test-form
    (v:define-texture-map cube-map (:cube :unique :six)
      ;; A single cube-store instance holds the array of data-elements and each
      ;; element is a cube-face instance.  The source of the data in the DSL is
      ;; a symbol of another texture-map, or a (:texture-map ...) anonmyous
      ;; definition.
      (:face ((:dir :+x)) cube-map-right)
      (:face ((:dir :-x)) cube-map-left)
      (:face ((:dir :+y)) cube-map-top)
      (:face ((:dir :-y)) cube-map-bottom)
      (:face ((:dir :+z)) cube-map-back)
      (:face ((:dir :-z)) cube-map-front))))

(defun test-cube-unique-six-1 ()
  (with-tmap-test-form
    (v:define-texture-map cube-map (:cube :unique :six)
      ;; A single cube-store instance holds the array of data-elements and each
      ;; element is a cube-face instance.  The source of the data in the DSL is
      ;; a symbol of another texture-map, or a (:texture-map ...) anonmyous
      ;; definition.
      (:face ((:dir :+x))
             (v:define-texture-map nil (:single :unique)
               (:mipmap () (textures map-right))))
      (:face ((:dir :-x))
             (v:define-texture-map nil (:single :unique)
               (:mipmap () (textures map-left))))
      (:face ((:dir :+y))
             (v:define-texture-map nil (:single :unique)
               (:mipmap () (textures map-top))))
      (:face ((:dir :-y))
             (v:define-texture-map nil (:single :unique)
               (:mipmap () (textures map-bottom))))
      (:face ((:dir :+z))
             (v:define-texture-map nil (:single :unique)
               (:mipmap () (textures map-back))))
      (:face ((:dir :-z))
             (v:define-texture-map nil (:single :unique)
               (:mipmap () (textures map-front)))))))


(defun test-cube-unique-opengl-0 ()
  (with-tmap-test-form
    (v:define-texture-map cube-map (:cube :unique :opengl)
      ;; The source of the data is a symbol of another texture-map, or a
      ;; (:texture-map ...) anonmyous definition.
      (:face ((:dir :texture-cube-map-positive-x)) cube-map-right)
      (:face ((:dir :texture-cube-map-negative-x)) cube-map-left)
      (:face ((:dir :texture-cube-map-positive-y)) cube-map-top)
      (:face ((:dir :texture-cube-map-negative-y)) cube-map-bottom)
      (:face ((:dir :texture-cube-map-positive-z)) cube-map-back)
      (:face ((:dir :texture-cube-map-negative-z)) cube-map-front))))


(defun test-cube-combined-vcross-top-0 ()
  (with-tmap-test-form
    (v:define-texture-map cube-map (:cube :combined :vcross-top)
      ;; A single cube-store hold a set of mipmaps-stores.
      ;; NOTE: all six faces are encoded into each mipmap image. Then each
      ;; image is a scaled mipmap going down to each image being a 1x1 image
      ;; embedded into a vcross-top image. Or however many mipmaps are
      ;; provided as long as they are in a contiguous set in mipmap levels.
      ;; There may NOT be a (:texture-map ...) form in here.
      (:mipmap () (envs town-0))
      (:mipmap () (envs town-1))
      (:mipmap () (envs town-2))
      (:mipmap () (envs town-3))
      (:mipmap () (envs town-4))
      (:mipmap () (envs town-5))
      (:mipmap () (envs town-6))
      (:mipmap () (envs town-7))
      (:mipmap () (envs town-8))
      (:mipmap () (envs town-9))
      (:mipmap () (envs town-10)))))
