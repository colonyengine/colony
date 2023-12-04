(in-package #:virality.texture)

;; TODO: Does the cache system need a cache-bucket which holds a cache item?
;; Cause then people can keep references to the cache bucket and if the value
;; changes when they look it up it'll just have the correct thing when they
;; look it up (if it gets changed in the background, like via live-coding).
;; Or, do I keep references to who is useing the item and then manually go and
;; update the stuff using it (which will drop things the engine doesn't know
;; about). Hrm, maybe the first option is better. Still though, the real
;; solution is that whoever needs the caches item simply looks it up all the
;; time instead of themselves caching the value they saw somewhere.


;; TODO: Candidate for public API
(defclass texture-profile ()
  ((%name :reader name
          :initarg :name)
   (%attributes :reader attributes
                :initarg :attributes
                :initform (u:dict #'eq))))

;; TODO: Candidate for public API
(defclass texture-descriptor ()
  ((%name :accessor name
          :initarg :name)
   ;; Procedural textures have :procedural as a texture-type!
   (%texture-type :accessor texture-type
                  :initarg :texture-type)
   (%profile-overlay-names :accessor profile-overlay-names
                           :initarg :profile-overlay-names)
   ;; Attribute specified in the define-texture form
   (%attributes :accessor attributes
                :initarg :attributes
                :initform (u:dict #'eq))
   ;; Up to date attributes once the profiles have been applied.
   (%applied-attributes :accessor applied-attributes
                        :initarg :applied-attributes
                        :initform (u:dict #'eq))))

;; --------------------------------------------------------------------------

;; The texture descriptor as read from the define-texture DSL form. This
;; records the original values for that texture definition which may include
;; that it is procedural or not. This texdesc is a reference to the only copy
;; of it as read from the DSL and stored in the texture-table in core.
(defclass texture ()
  ((%semantic-texdesc :accessor semantic-texdesc
                      :initarg :semantic-texdesc)
   ;; The texture descriptor as finally computed from the semantic texture
   ;; descriptor before we load/procedurally create the texture data or storage
   ;; on the GPU. Here, the texture-type must be valid among other aspects of
   ;; the texdesc. In the case of a non procedural texture, this can be
   ;; computed automatically, but in the case of a procedural texture, the user
   ;; ultimately must set the slots to valid things. This texdesc is unique to
   ;; this texture instance. This means, suppose in the case of a procedural
   ;; texture, that each texture instance contains a unique computed copy of
   ;; that the semantic-texdesc that directly describes this version of that
   ;; texture.
   (%computed-texdesc :accessor computed-texdesc
                      :initarg :computed-texdesc)
   ;; The name of the texture might be generated off the texdesc name for
   ;; procedural textures.
   (%name :reader name
          :initarg :name)
   ;; The allocated opengl texture id.
   (%texid :accessor texid
           :initarg :texid)))

;; The interface to reason about each individual texture type with a protocol.
;; Ensure the class symbol names are exported!
(defclass texture-1d (texture) ())
(defclass texture-2d (texture) ())
(defclass texture-2dms (texture) ())
(defclass texture-3d (texture) ())
(defclass texture-1d-array (texture) ())
(defclass texture-2d-array (texture) ())
(defclass texture-2dms-array (texture) ())
(defclass texture-rectangle (texture) ())
(defclass texture-buffer (texture) ())
(defclass texture-cube-map (texture) ())
(defclass texture-cube-map-array (texture) ())



(in-package #:virality.texture.texture-table)


;; This datatype is used in the CORE type to maintain the texture state.
(defclass texture-table ()
  ((%profiles :reader profiles
              :initarg :profiles
              :initform (u:dict #'eq))
   ;; All texture-descriptors from DEFINE-TEXTURE are stored here.
   (%semantic-texture-descriptors :reader semantic-texture-descriptors
                                  :initarg :semantic-texture-descriptors
                                  :initform (u:dict #'eq))
   ;; If a material requires a texture, but it was procedural, we mark a note
   ;; of it in here so the gamedev can find it later and generate them before
   ;; the game starts.
   (%unrealized-procedural-textures :reader unrealized-procedural-textures
                                    :initarg :unrealized-procedural-textures
                                    :initform (u:dict #'equal))))
