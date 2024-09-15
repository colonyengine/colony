(in-package #:colony.texture-map)

(defgeneric logical->physical (name model style store body))
(defgeneric logical-form-classifer (form-type model style store))

;;; ---------------------------------------------------------------------------
;; helper functions and methods.
;;; ---------------------------------------------------------------------------

(defun partition-a-dsl-form (key-pool classifer-func form)
  "Partition the FORM using U:SIEVE with a classifier function of
CLASSIFIER-FUNC into a set of values sorted by KEY-POOL. Return as values only
the lists of each category in the order of KEY-POOL."
  (u:sieve classifer-func
           form
           :initial-key-pool key-pool
           :pred-range-sort (u:sort-by-symbols key-pool)
           :collector #'second))



(defun gen-inferred-image-element-form (elidx logloc)
  `(,elidx (image-element :logloc ,logloc)))

(defun gen-inferred-texture-map-element-form (elidx logloc)
  `(,elidx (texture-map-element :logloc ,logloc)))

(defun gen-inferred-mapping-span-form (model elidx)
  "Used only in LOGICAL->PHYSICAL to generate template MAPPING-SPAN-* forms."
  (let* ((mapping-sym
           (u:format-symbol t "MAPPING-SPAN-~A" (symbol-name model)))
         (data-span-sym
           (u:format-symbol t "DATA-SPAN-~A" (symbol-name model))))
    `(,mapping-sym :to (,data-span-sym) :from (,data-span-sym :elidx ,elidx))))

(defun gen-inferred-mipmap-form (model attrs cattrs sattrs &rest mapping-spans)
  "Used only in LOGICAL->PHYSICAL to generate template MIPMAP-* forms."
  (let* ((mipmap-sym
           (u:format-symbol t "MIPMAP-~A" (symbol-name model)))
         (span-sym
           (u:format-symbol t "SPAN-~A" (symbol-name model))))
    `(,mipmap-sym :extent (,span-sym)
                  ,@attrs ,@cattrs ,@sattrs ,@mapping-spans)))

(defun gen-inferred-face-form (attrs cattrs sattrs dir elidx)
  "Used only in LOGICAL->PHYSICAL to generate a template FACE form."
  `(face ,@attrs ,@cattrs ,@sattrs :dir ,dir :elidx ,elidx))


;; For :1d, :2d, :3d, :cube/:envmap
(defmethod logical-form-classifier ((form-type (eql :body))
                                    model style store)
  (lambda (item)
    (if (and (listp item)
             (member (car item) '(sattrs cattrs attrs mipmap)))
        (car item)
        :unknown)))

;; For :cube/:unique only
(defmethod logical-form-classifier ((form-type (eql :body))
                                    (model (eql :cube))
                                    (style (eql :unique)) store)
  (lambda (item)
    (if (and (listp item)
             (member (car item) '(sattrs cattrs attrs face)))
        (car item)
        :unknown)))

(defmethod logical-form-classifier ((form-type (eql :mipmap))
                                    model style store)
  (lambda (item)
    (if (and (listp item)
             (member (car item) '(attrs cattrs sattrs)))
        (car item)
        :image-elements)))

(defmethod logical-form-classifier ((form-type (eql :face))
                                    model style store)
  (lambda (item)
    (if (and (listp item)
             (member (car item) '(attrs cattrs sattrs dir)))
        (car item)
        :texture-map-elements)))

;;; ---------------------------------------------------------------------------
;; 1d, 2d, 3d logical texture conversion.
;;; ---------------------------------------------------------------------------

(defmethod logical->physical (name model style store body)
  (when (physicalp body)
    ;; No error checking, we trust the body is in the right physical form.
    (return-from logical->physical body))

  (let* ((texture-map-key-pool '(attrs cattrs sattrs mipmap :unknown)))
    (multiple-value-bind (attrs cattrs sattrs mipmaps unknown)
        ;; Partition the body into chunks
        (partition-a-dsl-form texture-map-key-pool
                              (logical-form-classifier
                               :body model style store)
                              body)

      (when (plusp (length unknown))
        (error "Unknown texture-map logical form: ~A : ~A" name unknown))

      ;; Iterate the mipmaps, converting to the physical form and collecting
      ;; the data-elements from the mipmaps. We make very strong assumptions
      ;; about correctness and structure of the logical form.

      (let ((mipmap-key-pool '(sattrs cattrs attrs :image-elements))
            (data-elements nil)
            (physical-image-elements nil)
            (physical-mipmaps nil)
            (elidx 0))
        (loop
          :for (mipmap-name . mipmap-body) :in mipmaps
          :do (multiple-value-bind (mipmap-attrs mipmap-cattrs
                                    mipmap-sattrs image-elements)
                  (partition-a-dsl-form mipmap-key-pool
                                        (logical-form-classifier
                                         :mipmap model style store)
                                        mipmap-body)
                (let* ((physical-mapping-spans nil))
                  ;; Super basic texture-map type checking.
                  (u:when-let (num-elems (length image-elements))
                    (cond
                      ((zerop num-elems)
                       (error "Logical mipmap has zero image-elements!"))
                      ((and (> num-elems 1)
                            (or (member model '(:1d :2d))
                                (eq style :combined)))
                       (error "Logical mipmap has too many image-elements!"))))

                  ;; process image-elements and mapping spans for this mipmap
                  (dolist (image-element image-elements)
                    (push (gen-inferred-image-element-form elidx image-element)
                          physical-image-elements)
                    ;; NOTE: combined textures get figured out when the image
                    ;; is read.
                    (unless (eq style :combined)
                      (push (gen-inferred-mapping-span-form model elidx)
                            physical-mapping-spans))
                    (incf elidx))

                  ;; finally produce the physical mipmap form
                  (unless (eq style :combined)
                    (push (apply #'gen-inferred-mipmap-form
                                 model mipmap-attrs mipmap-cattrs
                                 mipmap-sattrs
                                 (nreverse physical-mapping-spans))
                          physical-mipmaps)))))

        ;; Order everything as we found it in the original form
        (setf data-elements `(data-elements
                              ,@(nreverse physical-image-elements))
              physical-mipmaps (nreverse physical-mipmaps))

        ;; Finally produce the new canonical physical body form.
        `(,@attrs ,@cattrs ,@sattrs ,data-elements ,@physical-mipmaps)))))


;;; ---------------------------------------------------------------------------
;; :cube logical texture conversion.
;;; ---------------------------------------------------------------------------

;; For :cube :unique textures
(defmethod logical->physical (name (model (eql :cube)) (style (eql :unique))
                              store body)
  (when (physicalp body)
    ;; No error checking, we trust the body is in the right physical form.
    (return-from logical->physical body))

  (let* ((texture-map-key-pool '(attrs cattrs sattrs face :unknown)))
    (multiple-value-bind (attrs cattrs sattrs faces unknown)
        ;; 1 partition the body into chunks
        (partition-a-dsl-form texture-map-key-pool
                              (logical-form-classifier
                               :body model style store)
                              body)

      (when (plusp (length unknown))
        (error "Unknown texture-map logical form: ~A : ~A" name unknown))

      ;; 3. Iterate the faces, converting to the physical form and collecting
      ;; the data-elements from the face form. We make very strong assumptions
      ;; about correctness and structure of the logical form.

      (let ((face-key-pool '(sattrs cattrs attrs dir :texture-map-elements))
            (data-elements nil)
            (physical-texture-map-elements nil)
            (physical-faces nil)
            (elidx 0))
        (loop
          :for (face-name . face-body) :in faces
          :do (multiple-value-bind (face-attrs face-cattrs face-sattrs
                                    face-dirs texture-map-elements)
                  (partition-a-dsl-form face-key-pool
                                        (logical-form-classifier
                                         :face model style store)
                                        face-body)
                ;; Super basic texture-map type checking.
                (u:when-let (num-elems (length texture-map-elements))
                  (cond
                    ((zerop num-elems)
                     (error "Logical face has zero texture-map-elements!"))
                    ((> num-elems 1)
                     (error "Logical face has too many texture-map-elements!"))))
                ;; Super basic texture-map type checking.
                (unless (= (length face-dirs) 1)
                  (error "Logical face needs exactly 1 DIR form!"))

                ;; process texture-map-elements for this face
                (dolist (texture-map-element texture-map-elements)
                  (push (gen-inferred-texture-map-element-form
                         elidx texture-map-element)
                        physical-texture-map-elements)

                  (push (gen-inferred-face-form
                         face-attrs face-cattrs face-sattrs
                         (cadar face-dirs) elidx)
                        physical-faces)
                  (incf elidx))))

        ;; Order everything as we found it in the original form
        (setf data-elements `(data-elements
                              ,@(nreverse physical-texture-map-elements))
              physical-faces (nreverse physical-faces))

        ;; Finally produce the new canonical physical body form.
        `(,@attrs ,@cattrs ,@sattrs ,data-elements
                  (cube (faces ,@physical-faces)))))))

(defmethod logical->physical (name (model (eql :cube)) (style (eql :envmap))
                              store body)
  (when (physicalp body)
    ;; No error checking, we trust the body is in the right physical form.
    (return-from logical->physical body))

  (let* ((texture-map-key-pool '(attrs cattrs sattrs mipmap :unknown)))
    (multiple-value-bind (attrs cattrs sattrs mipmaps unknown)
        ;; 1 partition the body into chunks
        (partition-a-dsl-form texture-map-key-pool
                              (logical-form-classifier
                               :body model style store)
                              body)

      (when (plusp (length unknown))
        (error "Unknown texture-map logical form: ~A : ~A" name unknown))

      ;; 3. Iterate the mipmaps, converting to the physical form and collecting
      ;; the data-elements from the mipmaps. We make very strong assumptions
      ;; about correctness and structure of the logical form.

      (let ((mipmap-key-pool '(sattrs cattrs attrs :image-elements))
            (data-elements nil)
            (physical-image-elements nil)
            (physical-mipmaps nil)
            (elidx 0))
        (loop
          :for (mipmap-name . mipmap-body) :in mipmaps
          :do (multiple-value-bind (mipmap-attrs mipmap-cattrs
                                    mipmap-sattrs image-elements)
                  (partition-a-dsl-form mipmap-key-pool
                                        (logical-form-classifier
                                         :mipmap model style store)
                                        mipmap-body)
                (let* ((physical-mapping-spans nil))
                  ;; Super basic texture-map type checking.
                  (u:when-let (num-elems (length image-elements))
                    (cond
                      ((zerop num-elems)
                       (error "Logical mipmap has zero image-elements!"))
                      ((and (> num-elems 1)
                            (or (member model '(:1d :2d))
                                (eq style :combined)))
                       (error "Logical mipmap has too many image-elements!"))))

                  ;; process image-elements and mapping spans for this mipmap
                  (dolist (image-element image-elements)
                    (push (gen-inferred-image-element-form elidx image-element)
                          physical-image-elements)
                    ;; NOTE: combined textures get figured out when the image
                    ;; is read.
                    (unless (eq style :combined)
                      (push (gen-inferred-mapping-span-form :2d elidx)
                            physical-mapping-spans))
                    (incf elidx))

                  ;; finally produce the physical mipmap form
                  (unless (eq style :combined)
                    (push (apply #'gen-inferred-mipmap-form
                                 :2d mipmap-attrs mipmap-cattrs
                                 mipmap-sattrs
                                 (nreverse physical-mapping-spans))
                          physical-mipmaps)))))

        ;; Order everything as we found it in the original form
        (setf data-elements `(data-elements
                              ,@(nreverse physical-image-elements))
              physical-mipmaps (nreverse physical-mipmaps))

        ;; Finally produce the new canonical physical body form.
        `(,@attrs ,@cattrs ,@sattrs ,data-elements
                  (cube (envmap ,@physical-mipmaps)))))))


;;; ---------------------------------------------------------------------------
;; Sort of unit tests.
;;; ---------------------------------------------------------------------------

(defun test-log-to-phys/g000-1d-log-inf-one-non ()
  (let* ((name 'g000-1d-log-inf-one-non)
         (model :1d)
         (style :unique)
         (store nil)
         (data-model (list model style store)))
    (list name
          data-model
          (logical->physical name model style store
                             ;; body
                             '((mipmap (textures 1d-64x1)))))))

(defun test-log-to-phys/g001-1d-log-inf-all-non ()
  (let* ((name 'g001-1d-log-inf-all-non)
         (model :1d)
         (style :unique)
         (store nil)
         (data-model (list model style store)))
    (list name
          data-model
          (logical->physical name model style store
                             ;; body
                             '((mipmap (textures 1d-64x1))
                               (mipmap (textures 1d-32x1))
                               (mipmap (textures 1d-16x1))
                               (mipmap (textures 1d-8x1))
                               (mipmap (textures 1d-4x1))
                               (mipmap (textures 1d-2x1))
                               (mipmap (textures 1d-1x1)))))))

(defun test-log-to-phys/g003-1d-log-inf-all-non ()
  (let* ((name 'g003-1d-log-inf-all-non)
         (model :1d)
         (style :combined)
         (store :common)
         (data-model (list model style store)))
    (list name
          data-model
          ;; NOTE: No body gets produce beyond the data-elements form.  This is
          ;; because we don't know how many mipmaps are available until we
          ;; inspect the image header to determine it.
          (logical->physical name model style store
                             ;; body
                             '((mipmap (textures 1d-all-127x1)))))))

(defun test-log-to-phys/g004-1d-log-inf-one-ovr ()
  (let* ((name 'g004-1d-log-inf-one-ovr)
         (model :1d)
         (style :unique)
         (store :common)
         (data-model (list model style store)))
    (list name
          data-model
          ;; NOTE: No body gets produce beyond the data-elements form.  This is
          ;; because we don't know how many mipmaps are available until we
          ;; inspect the image header to determine it.
          (logical->physical
           name model style store
           ;; body
           '((cattrs ('foo "tmap attr 0")
              ('bar "tmap attr 1"))
             (mipmap
              (cattrs
               ('bar "mipmap attr 0 overlays onto tmap attr 0")
               ('qux "mipmap attr 1"))
              (textures 1d-64x1)))))))

(defun test-log-to-phys/g000-cube-log-inf-one-non ()
  (let* ((name 'g000-cube-log-inf-one-non)
         (model :cube)
         (style :unique)
         (store :six)
         (data-model (list model style store)))
    (list name
          data-model
          (logical->physical
           name model style store
           ;; body
           '((face (dir :-x) cube-negx)
             (face (cattrs ('foo 42)) (dir :+x) cube-posx)
             (face (dir :-y) cube-negy)
             (face (dir :+y) cube-posy)
             (face (dir :-z) cube-negz)
             (face (dir :+z) cube-posz))))))

(defun test-log-to-phys/g001-cube-log-inf-one-non ()
  (let* ((name 'g001-cube-log-inf-one-non)
         (model :cube)
         (style :envmap)
         (store :hcross)
         (data-model (list model style store)))
    (list name
          data-model
          (logical->physical
           name model style store
           ;; body
           '((mipmap (textures cube-hcross-256x192)))))))

(defun test-log-to-phys/g002-cube-log-inf-all-non ()
  (let* ((name 'g002-cube-log-inf-all-non)
         (model :cube)
         (style :envmap)
         (store :hcross)
         (data-model (list model style store)))
    (list name
          data-model
          (logical->physical
           name model style store
           ;; body
           '((mipmap (textures cube-hcross-256x192))
             (mipmap (textures cube-hcross-128x96))
             (mipmap (textures cube-hcross-64x48))
             (mipmap (textures cube-hcross-32x24))
             (mipmap (textures cube-hcross-16x12))
             (mipmap (textures cube-hcross-8x6))
             (mipmap (textures cube-hcross-4x3)))))))

(defun run-log-to-phys-tests ()
  (let ((tests (list (test-log-to-phys/g000-1d-log-inf-one-non)
                     (test-log-to-phys/g001-1d-log-inf-all-non)
                     (test-log-to-phys/g003-1d-log-inf-all-non)
                     (test-log-to-phys/g004-1d-log-inf-one-ovr)
                     (test-log-to-phys/g000-cube-log-inf-one-non)
                     (test-log-to-phys/g001-cube-log-inf-one-non)
                     (test-log-to-phys/g002-cube-log-inf-all-non))))
    (dolist (test tests)
      (destructuring-bind (name data-model inf-phys-body) test
        (format t "~S~%"
                ;; The logical form transformed into the physical form.
                `(define-texture-map ,name ,data-model
                   ,@inf-phys-body))))))
