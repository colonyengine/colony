(in-package #:virality.models)

;; TODO: In progress of sketching out this code. It does not function yet.

(defclass model-profile ()
  ((%name :reader name
          :initarg :name)
   (%attributes :reader attributes
                :initarg :attributes
                :initform (u:dict))))

(defclass model-descriptor ()
  ((%name :reader name
          :initarg :name)
   (%profile-overlay-name :reader profile-overlay-names
                          :initarg :profile-overlay-names
                          :initform nil)
   (%attributes :reader attributes
                :initarg :attributes
                :initform (u:dict))))

(defun make-model-descriptor (&rest args)
  (apply #'make-instance 'model-descriptor args))

(defun parse-model-profile (name body-form)
  (declare (ignore name body-form))
  nil)

(defmacro define-model-profile (name &body body)
  (a:with-gensyms (profile)
    (let ((definition '(v::meta 'model-profiles)))
      `(let ((,profile ,(parse-model-profile name body)))
         (unless ,definition
           (setf ,definition (u:dict)))
         (setf (u:href ,definition (name ,profile)) ,profile)))))

(defun update-model/interactively (old-model-descriptor new-model-descriptor)
  (declare (ignore old-model-descriptor new-model-descriptor))
  nil)

(defmacro define-model (name (&rest profile-overlay-names) &body body)
  "Construct a semantic MODEL-DESCRIPTOR. "
  (a:with-gensyms (desc-lookup old-desc new-desc)
    (let ((definition '(v::meta 'models)))
      `(symbol-macrolet ((,desc-lookup (u:href ,definition ',name)))
         (unless ,definition
           (setf ,definition (u:dict)))
         (let ((,new-desc (make-model-descriptor
                           :name ',name
                           :profile-overlay-names ',profile-overlay-names))
               (,old-desc ,desc-lookup))
           ;; Record the parameters we'll overlay on the profile at use time.
           (setf ,@(loop :for (key value) :in body
                         :append `((u:href (attributes ,new-desc) ,key)
                                   ,value)))
           (setf ,desc-lookup ,new-desc)

           (update-model/interactively ,old-desc ,new-desc)

           (export ',name))))))

(defun load-model-descriptors (core)
  (declare (ignore core))
  nil)

#|
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; assuming in options I have (:damaged-helmet "/some/path/to/damaged-helmet")

;;;; Example 1: Default Forms
;;;;
(define-model "damaged-helmet" (:library examples)
  ;; The model in which I am interested, cannot be default.
  (:model #(:damaged-helmet "damaged-helmet.glb"))

  ;; If the model has unresolved paths for things like texture, this is where I
  ;; look them up first. If this is unspecified, then attempt to look in the
  ;; directory the :model file is in.
  (:model-data #(:damaged-helmet))

  ;; Should I always load materials and textures from this model no matter what,
  ;; or try and find similarly named materials and textures (from either
  ;; define-texture, define-material, or OTHER models) and use those instead.
  (:coalesce-texture-policy :self)
  (:coalesce-material-policy :self)

  ;; If the root of the scene in the gltf2 file is a SINGLE actor, then lift it
  ;; to be the actual root instead of existing underneath the encapsulating
  ;; prefab actor. If there is more than one root, this is ignored.
  (:lift-single-root t)

  ;; TODO: At the end of processing the define-model, we emit expand to a
  ;; define-prefab form (including any transform overrides detailed in the scene
  ;; portion of the gltf file). We convert the scene information in the gltf
  ;; file to a define-prefab form.

  ;; TODO: If a define-prefab uses the same name as a previous define-prefab,
  ;; emit a warning that an overwrite has happened. The name of the prefab
  ;; is the name of the model and the library it is in.
  )

;; Now, the gamedev can just use the model normally.

(define-prefab "damaged-helmet-0" (:library examples)
  (("damaged-helmet-0" :copy "/damaged-helmet")
   (c/foobar :init 42)
   ("mask"
    (c/voo :ooo 82))))

(define-prefab "damaged-helmet-mask-0" (:library examples)
  (("damaged-helmet-mask-0" :copy "/damaged-helmet/mask")
   (c/voo :ooopppp 82)))
|#
