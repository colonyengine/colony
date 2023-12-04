(in-package #:virality.texture-map)

(defgeneric parse-texture-map (name model style store body))
(defgeneric parse-body (model style store tmap-var propdefs body))
(defgeneric parse-data-form (model style store tag properties elements
                             default-channel-layout))
(defgeneric parse-property-form (model style store propdefs form))
(defgeneric default-model-properties (model style store))
(defgeneric property-form-p (model style store form))
(defgeneric data-store-form-p (model style store form))
(defgeneric property-name-to-slot-symbol (model style store property-name))
(defgeneric texture-map-class-name (model style store))
(defgeneric data-store-class-name (model style store tag))

;;; -----------------------------------------------------------------------
;; Methods for processing of any texture-map type
;;; -----------------------------------------------------------------------

;; So far, all textures have these two properties.
(defmethod property-form-p (model style store form)
  (when (listp form)
    (member (first form) '(:origin :channel-layout))))

(defmethod parse-body (model style store tmap-var propdefs body)
  ;; We separate out the property-forms from the data-form since we must
  ;; process the property forms first.
  (multiple-value-bind (data-store-forms property-forms)
      (loop :for form :in body
            :when (property-form-p model style store form)
              :collect form :into pforms
            :when (data-store-form-p model style store form)
              :collect form :into dforms
            :finally
               (return (values dforms pforms)))

    ;; Process the locally specified property forms first.
    (let ((properties (u:dict)))

      ;; Build a table of the manually specified properties defined in
      ;; the body form.
      (dolist (prop-form property-forms)
        (parse-property-form model style store properties prop-form))

      ;; Produce a list of the store forms for the propdefs into the
      ;; texture-map provided there isn't a properties form that changes its
      ;; value.
      `((u:comment "Allocate the texture-map general data-store instance.")
        (setf (data-store ,tmap-var)
              (make-data-store 'data-store ,(length data-store-forms)))

        (u:comment "Default properties.")
        ,@(when propdefs
            (loop :for prop-name :being :the :hash-keys :in propdefs
                    :using (hash-value value)
                  :when (or (null properties)
                            (not (nth-value 1 (u:href properties
                                                      prop-name))))
                    :collect `(setf (,(property-name-to-slot-symbol
                                       model style store prop-name)
                                     ,tmap-var)
                                    ,value)))

        (u:comment "Manually specified properties.")
        ,@(loop :for prop-name :being :the :hash-keys :in properties
                  :using (hash-value value)
                :collect `(setf (,(property-name-to-slot-symbol
                                   model style store prop-name)
                                 ,tmap-var)
                                ,value))

        (u:comment "Individual data-store forms."
          "and store it into the appropriate spot in the texture-map's"
          "data-store slot.")
        ,@(when data-store-forms
            ;; TODO: This hash merge on mprops is a little cheeky since I
            ;; don't do it above and elect to maintain the difference
            ;; between the defprops and properties hash for the texture-map.
            (loop :with mprops = (u:hash-merge (u:dict) propdefs properties)
                  :for dsform :in data-store-forms
                  :for index :from 0
                  :collect `(setf (aref (data-elements (data-store ,tmap-var))
                                        ,index)
                                  ,(destructuring-bind
                                       (tag properties . elements) dsform
                                     (parse-data-form
                                      model style store tag properties
                                      elements mprops)))))))))


;; parse the form into a key/value pair and store it in properties.
(defmethod parse-property-form (model style store properties form)
  (setf (u:href properties (first form)) (second form)))

;; The returned symbol will be used as a slot-name to set the property in the
;; texture-map instance.
(defmethod property-name-to-slot-symbol (model style store property-name)
  (ecase property-name
    (:origin 'origin)
    (:channel-layout 'channel-layout)))

(defmethod parse-texture-map (name model style store body)
  (let ((propdefs (default-model-properties model style store)))
    ;; Generate the texture-map object and add property information.
    (u:with-gensyms (tmap)
      `(let ((,tmap
               ;; Create the specific texture map instance we need.
               (make-texture-map
                ',(texture-map-class-name model style store)
                :name ',name
                :model ,model
                :style ,style
                :store ',store)))

         ;; Inline whatever I get back from parsing the body.
         ,@(parse-body model style store tmap propdefs body)

         ,tmap))))

;;; -----------------------------------------------------------------------
;; For any texture-maps that use the (tag (prop*) data-elements*) data-store
;; form, they are mostly processed the same way. For any texture-map which
;; deviates from this generalized parsing, we implement a specialization.
;;; -----------------------------------------------------------------------

;; return a lisp form which sets up and initializes the data-store and puts the
;; data-elements into it.
(defmethod parse-data-form (model style store tag properties elements propdefs)
  (let ((data-store-properties (u:dict)))
    (dolist (prop-form properties)
      (parse-property-form model style store data-store-properties prop-form))

    (u:with-gensyms (dstore)

      `(let ((,dstore (make-data-store
                       ',(data-store-class-name model style store tag)
                       ,(length elements))))

         ;; TODO: Parse the data-store form specific properties into a hash.

         ;; Store the propdef defaults into the data-store first unless the
         ;; properties table doesn't exist or doesn't already has something for
         ;; that property.
         ,@(when propdefs
             (loop :for prop-name :being :the :hash-keys :in propdefs
                     :using (hash-value value)
                   :when (or (null properties)
                             (not (nth-value 1 (u:href data-store-properties
                                                       prop-name))))
                     :collect `(setf (,(property-name-to-slot-symbol
                                        model style store prop-name)
                                      ,dstore)
                                     ,value)))

         ;; Now fill in the properties specific to this data-store form.
         ,@(when properties
             (loop :for prop-name :being :the :hash-keys
                     :in data-store-properties
                       :using (hash-value value)
                   :collect `(setf (,(property-name-to-slot-symbol
                                      model style store prop-name)
                                    ,dstore)
                                   ,value)))

         ;; Finally, store all the discovered data-elements into the
         ;; data-store.
         ,@(when elements
             (loop
               :for elem :in elements
               :for index :from 0
               :collect `(setf (aref (data-elements ,dstore) ,index)
                               (make-data-element :original ',elem))))
         ,dstore))))

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

;; Return a newly constructed hash table holding property names as keys
;; and default values as values.
(defmethod default-model-properties ((model (eql :single)) style store)
  (u:dict :origin :top-left
          :channel-layout :combined))

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

(defmethod default-model-properties ((model (eql :rect)) style store)
  (u:dict :origin :top-left
          :channel-layout :combined))

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

(defmethod default-model-properties ((model (eql :buffer)) style store)
  (u:dict))

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

(defmethod default-model-properties ((model (eql :voxel)) style store)
  (u:dict :origin :bottom-left-back
          :channel-layout :combined))

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
      (:mipamp ()
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


;;; ----
;; These methods support specialized :unique style cube maps that use :face
;; data-store forms.
;;; ----

(defmethod property-name-to-slot-symbol ((model (eql :cube))
                                         (style (eql :unique))
                                         store property-name)
  ;; So far, we only support this one as a property.
  (ecase property-name
    (:dir 'dir)))

(defmethod default-model-properties ((model (eql :cube)) (style (eql :unique))
                                     store)
  (u:dict))

(defmethod data-store-form-p ((model (eql :cube)) (style (eql :unique))
                              store form)
  (when (listp form)
    (member (first form) '(:face))))

;; Returrn a form which initializes a cube face with the right property and
;; name.
(defmethod parse-data-form ((model (eql :cube)) (style (eql :unique))
                            store (tag (eql :face)) properties elements
                            propdefs)
  (let ((data-store-properties (u:dict)))
    (dolist (prop-form properties)
      (parse-property-form model style store data-store-properties prop-form))

    ;; TODO: If the eleemnts is a (:texture-map ...) form, process it right
    ;; here.

    (assert (= (length elements) 1))

    (u:with-gensyms (face)
      `(let ((,face (make-cube-face)))

         ;; Now fill in the properties specific to this sube-face.
         ,@(when properties
             (loop :for prop-name :being :the :hash-keys
                     :in data-store-properties
                       :using (hash-value value)
                   :collect `(setf (,(property-name-to-slot-symbol
                                      model style store prop-name)
                                    ,face)
                                   ,value)))

         ;; Fow now, we just support a single thing in the :face elements.
         ;; TODO: Call parse-texture-map here of there is a (:texture-map ...)
         ;; form.
         (setf (name ,face) ',(first elements))
         (make-data-element :original ,face)))))

;; TODO: Convert to cube map to handle :unique situations.
(defmethod parse-body ((model (eql :cube)) (style (eql :unique)) store
                       tmap-var propdefs body)
  ;; We separate out the property-forms from the data-form since we must
  ;; process the property forms first.
  (multiple-value-bind (data-store-forms property-forms)
      (loop :for form :in body
            :when (property-form-p model style store form)
              :collect form :into pforms
            :when (data-store-form-p model style store form)
              :collect form :into dforms
            :finally
               (return (values dforms pforms)))

    ;; TODO: Error handling needs to be better, but basically, a :unique style
    ;; requires 6 face data-store forms.
    (assert (= (length body) 6))

    ;; Process the locally specified property forms first.
    (let ((properties (u:dict)))

      ;; Build a table of the manually specified properties defined in
      ;; the body form.
      (dolist (prop-form property-forms)
        (parse-property-form model style store properties prop-form))

      (u:with-gensyms (cube-store cs-data-elements)
        `((u:comment "Allocate the texture-map general data-store instance.")
          (setf (data-store ,tmap-var)
                (make-data-store 'data-store 1))

          (u:comment "Allocate a single cube-store to hold all faces.")
          (let ((,cube-store (make-data-store 'cube-store 6
                                              :style ,style
                                              :store ,store)))
            (setf (aref (data-elements (data-store ,tmap-var)) 0)
                  ,cube-store)

            (u:comment "Default properties.")
            ,@(when propdefs
                (loop :for prop-name :being :the :hash-keys :in propdefs
                        :using (hash-value value)
                      :when (or (null properties)
                                (not (nth-value 1 (u:href properties
                                                          prop-name))))
                        :collect `(setf (,(property-name-to-slot-symbol
                                           model style store prop-name)
                                         ,tmap-var)
                                        ,value)))

            (u:comment "Manually specified properties.")
            ,@(loop :for prop-name :being :the :hash-keys :in properties
                      :using (hash-value value)
                    :collect `(setf (,(property-name-to-slot-symbol
                                       model style store prop-name)
                                     ,tmap-var)
                                    ,value))

            (u:comment "Store each of the 6 faces into the cube-store.")
            ;; TODO: This hash merge on mprops is a little cheeky since I
            ;; don't do it above and elect to maintain the difference
            ;; between the defprops and properties hash for the texture-map.
            (let ((,cs-data-elements (data-elements ,cube-store)))
              ,@(loop :with mprops = (u:hash-merge (u:dict) propdefs
                                                   properties)
                      :for dsform :in data-store-forms
                      :for index :from 0 :by 1
                      :collect
                      `(setf (aref ,cs-data-elements ,index)
                             ,(destructuring-bind
                                  (tag properties . elements) dsform
                                (parse-data-form
                                 model style store tag properties
                                 elements mprops)))))))))))

;;; ----
;; These methods support specialized :combined cube maps where all faces are
;; encoded into a single image (which may have mipmaps). These data-forms use
;; the :mipmap form.
;;; ----

;; TODO KEEP GOING



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
