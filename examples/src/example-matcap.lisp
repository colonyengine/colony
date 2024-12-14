(in-package #:colony-examples)

;; TODO: The matcap-lookup shader had the (- 1 y) removed to be in favor
;; of just the y coord. Normally, it means that these two texture would have
;; to be flipped. However, the gltf2 uv coords have an upper left origin
;; instead of an upper right one. So that just complicates everything until
;; we finish proper cannonicalization of uv coords for different model vendors.
;; Also, the regular matcap shader still has the (- 1 y). So confusion all
;; around for a little bit.

(c:define-texture-map matcap/matcap-map (:2d :unique)
  (texmap:mipmap (textures matcap-map)))

(c:define-texture matcap/matcap-map (:texture-2d x:clamp-all-edges)
  ;; TODO: This is commented out because the gltf2 uvs have a different origin
  ;;(:flip-y t) ;; removed 1 - y from shader.
  (:texture-min-filter :nearest)
  (:texture-mag-filter :nearest)
  ;; TODO: TMAP Convert :data to reference texture-map name.
  (:data #((textures matcap-map))))

;; TODO: possibly set :origin for these images to deal with the matcap shader.
(c:define-texture-map matcap/basic-dark (:2d :unique)
  (texmap:mipmap (c:matcaps c::basic-dark)))

(c:define-texture-map matcap/metal-carpaint (:2d :unique)
  (texmap:mipmap (c:matcaps c::metal-carpaint)))

(c:define-texture-map matcap/jade (:2d :unique)
  (texmap:mipmap (c:matcaps c::jade)))

(c:define-texture-map matcap/basic-1 (:2d :unique)
  (texmap:mipmap (c:matcaps c::basic-1)))

(c:define-texture-map matcap/metal-shiny (:2d :unique)
  (texmap:mipmap (c:matcaps c::metal-shiny)))

(c:define-texture-map matcap/metal-lead (:2d :unique)
  (texmap:mipmap (c:matcaps c::metal-lead)))

;; TOOD: make basic-dark an RGB greyscale image. Check the others too.
(c:define-texture matcap/lookup (:texture-2d-array x:clamp-all-edges)
  ;; TODO: This we keep to "undo" the origin of the gltf coords.
  ;; Confusing, no? I think the "upside downness" of the matcap/matcap-map
  ;; and these are actually different on disk....
  (:flip-y t) ;; removed 1 - y from shader.
  ;; TODO: TMAP Convert :data to use the texture-map name.
  (:data #(#((c:matcaps c::basic-dark)) ;; inside engine
           #((c:matcaps c::metal-carpaint)) ;; body base color
           #((c:matcaps c::jade)) ;; windshield
           #((c:matcaps c::basic-1))
           #((c:matcaps c::metal-shiny)) ;; engines
           #((c:matcaps c::basic-1))
           #((c:matcaps c::metal-lead)) ;; strip on wings, trim
           #((c:matcaps c::basic-1)))))

(c:define-material matcap-lookup
  (:shader ex/shd::matcap-lookup
   :profiles (x:u-mvp)
   :uniforms ((:matcaps 'matcap/lookup)
              (:normal-matrix (m3:id))
              (:id-map 'matcap/matcap-map))))


(c:define-prefab "default-artblob" (:library examples)
  "A base description of the damaged helmet so we can use it easily elsewhere."
  (comp:transform :rotate (q:orient :local :x o:pi/2))
  (comp:mesh :asset '(meshes art-blob)
             :name "ArtBlob")
  (comp:render :material '(x:matcap ceramic-dark-copy
                           :uniforms ((:sampler x:matcap/ceramic-dark)))
               :slave (c:ref :self :component 'comp:mesh)))

(c:define-prefab "artblob" (:library examples)
  (("camera" :copy "/cameras/perspective")
   (comp:camera (:policy :new-args)
                :free-look t))
  (("artblob" :copy "/default-artblob")
   (comp:transform :rotate/velocity (v3:velocity v3:+forward+ (- o:pi/6))
                   :scale 4f0)))

;; TODO: This prefab has a bug with the material copying and it cannot be
;; instantiated properly. But I'll solve that in a later checkin.
(c:define-prefab "artblob-group" (:library examples)
  (("camera" :copy "/cameras/perspective"))
  (("artblob1" :copy "/default-artblob")
   (comp:transform :rotate/velocity (v3:velocity (v3:ones) (- o:pi/3))
                   :translate (v3:vec -15f0 0f0 0f0)
                   :scale 4f0))
  (("artblob2" :copy "/default-artblob")
   (comp:transform :translate (v3:vec 15f0 0f0 0f0)
                   :scale 4f0)))

(c:define-prefab "artblob-interactive" (:library examples)
  (("camera" :copy "/cameras/perspective")
   (comp:camera (:policy :new-args)))
  (("artblob" :copy "/default-artblob")
   (comp:transform :scale 4f0)
   (simple-mouse-rotator :clamp-p t)))


;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(c:define-prefab "default-ship" (:library examples)
  "A base description of the damaged helmet so we can use it easily elsewhere."
  (comp:transform :rotate (q:orient :local :x o:pi/2))
  (comp:mesh :asset '(meshes ships)
             :name "Ship_001") ;; 001 to 024
  ;; TODO: When this material name is nonexistant, it doesn't draw anything.
  (comp:render :material 'matcap-lookup
               :slave (c:ref :self :component 'comp:mesh)))


;; ;;;;;;;;;;;;;;;;;;;
;; The test prefabs
;; ;;;;;;;;;;;;;;;;;;;

(c:define-prefab "ship01" (:library examples)
  (("camera" :copy "/cameras/perspective")
   (comp:camera (:policy :new-args)
                :free-look t))
  (("ship01" :copy "/default-ship")
   (comp:transform :rotate/velocity (v3:velocity v3:+forward+ (- o:pi/6))
                   :scale 4f0)))

(c:define-prefab "ship01-interactive" (:library examples)
  (("camera" :copy "/cameras/perspective")
   (comp:camera (:policy :new-args)))
  (("ship01" :copy "/default-ship")
   (comp:transform :scale 4f0)
   (comp:sphere :on-layer :ground)
   (simple-mouse-rotator :clamp-p t)))

(c:define-prefab "ship-group" (:library examples)
  (("camera" :copy "/cameras/perspective"))
  (("ship011" :copy "/default-ship")
   (comp:mesh :name "Ship_011")
   (comp:transform :rotate/velocity (v3:velocity (v3:ones) (- o:pi/3))
                   :translate (v3:vec -15f0 0f0 0f0)
                   :scale 4f0))
  (("ship012" :copy "/default-ship")
   (comp:mesh :name "Ship_012")
   (comp:transform :translate (v3:vec 15f0 0f0 0f0)
                   :scale 4f0)))
