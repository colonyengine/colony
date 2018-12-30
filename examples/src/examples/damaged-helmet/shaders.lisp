(in-package :first-light.gpu.user)

(define-struct pbr-info
  ;; cos angle between normal and light direction
  (n-dot-l :float :accessor n-dot-l)
  ;; cos angle between normal and view direction
  (n-dot-v :float :accessor n-dot-v)
  ;; cos angle between normal and half vector
  (n-dot-h :float :accessor n-dot-h)
  ;; cos angle between light vector and half vector
  (l-dot-h :float :accessor l-dot-h)
  ;; cos angle between view direction and half vector
  (v-dot-h :float :accessor v-dot-h)
  ;; roughness value, as authored by model creator (input to shader)
  (perceptual-roughness :float :accessor perceptual-roughness)
  ;; metallic value at surface
  (metalness :float :accessor metalness)
  ;; full reflectance color (normal incidence angle)
  (reflectance-0 :vec3 :accessor reflectance-0)
  ;; reflectance color at graxing scale
  (reflectance-90 :vec3 :accessor reflectance-90)
  ;; roughness mapp to a more linear change in the roughness
  ;; proposed by [2]
  (alpha-roughness :float :accessor alpha-roughness)
  ;; color contribution from the diffuse lighting
  (diffuse-color :vec3 :accessor diffuse-color)
  ;; color contribution from the specular lighting
  (specular-color :vec3 :accessor specular-color))

;; PBR vertex shader stage:
;;
;; created specifically for the damaged-helmet model:
;;
;; USE_IBL HAS_NORMALS HAS_UV

(define-function vert/damaged-helmet ((pos :vec3)
                                      (normal :vec3)
                                      (tangent :vec3)
                                      (color :vec4)
                                      (uv1 :vec2)
                                      (uv2 :vec2)
                                      (joints :vec4)
                                      (weights :vec4)
                                      &uniform
                                      (model :mat4)
                                      (normmat :mat4)
                                      (view :mat4)
                                      (proj :mat4))
  (let* ((pvm (* proj view model))
         (homo-world-pos (* model (vec4 pos 1.0)))
         (world-pos (/ (.xyz homo-world-pos) (.w homo-world-pos)))
         (vert-normal (normalize (.xyz (* model (vec4 (.xyz normal) 0.0))))))
    (values (* pvm (vec4 pos 1.0))
            vert-normal
            uv1
            world-pos)))

(define-function pbr/get-normal ((world-pos :vec3)
                                 (vert-normal :vec3)
                                 (uv1 :vec2)
                                 (normal-sampler :sampler-2d)
                                 (normal-scale :float))
  ;; I'm assuming that we don't have tangents, but we have normals at the
  ;; vertex and also a normal map.
  (let* ((pos-dx (d-fdx world-pos))
         (pos-dy (d-fdy world-pos))
         (tex-dx (d-fdx (vec3 uv1 0.0)))
         (tex-dy (d-fdy (vec3 uv1 0.0)))
         (tv (/ (- (* (.t tex-dy) pos-dx)
                   (* (.t tex-dx) pos-dy))
                (- (* (.s tex-dx) (.t tex-dy))
                   (* (.s tex-dy) (.t tex-dx)))))

         (ngv (normalize vert-normal)) ; assume we have a normal vertex attribute
         (tv (normalize (- tv (* ngv (dot ngv tv)))))
         (bv (normalize (cross ngv tv)))
         (tbn (mat3 tv bv ngv))
         (n (.rgb (texture normal-sampler uv1))) ; assume we have a normal map
         (n (normalize (* tbn (- (* 2.0 n) (vec3 1.0))
                          (vec3 normal-scale normal-scale 1.0)))))
    n))

;; The following equation models the Fresnel reflectance term of the spec
;; equation (aka F()) Implementation of fresnel from [4], Equation 15
(define-function pbr/specular-reflection ((pbr-inputs pbr-info))
  (+ (reflectance-0 pbr-inputs)
     (* (- (reflectance-90 pbr-inputs) (reflectance-0 pbr-inputs))
        (pow (saturate (- 1.0 (v-dot-h pbr-inputs))) 5.0))))

;; This calculates the specular geometric attenuation (aka G()), where rougher
;; material will reflect less light back to the viewer.  This implementation is
;; based on [1] Equation 4, and we adopt their modifications to alphaRoughness
;; as input as originally proposed in [2].
(define-function pbr/geometric-occlusion ((pbr-inputs pbr-info))
  (with-accessors ((n-dot-l n-dot-l) (n-dot-v n-dot-v) (r alpha-roughness)) pbr-inputs
    (let* ((attenuation-l (/ (* 2.0 n-dot-l)
                             (+ n-dot-l
                                (sqrt (+ (* r r)
                                         (* (- 1.0 (* r r))
                                            (* n-dot-l n-dot-l)))))))
           (attenuation-v (/ (* 2.0 n-dot-v)
                             (+ n-dot-v
                                (sqrt (+ (* r r)
                                         (* (- 1.0 (* r r))
                                            (* n-dot-v n-dot-v))))))))
      (* attenuation-l attenuation-v))))

;; The following equation(s) model the distribution of microfacet normals across
;; the area being drawn (aka D()) Implementation from "Average Irregularity
;; Representation of a Roughened Surface for Ray Reflection" by
;; T. S. Trowbridge, and K. P. Reitz Follows the distribution function
;; recommended in the SIGGRAPH 2013 course notes from EPIC Games [1], Equation
;; 3.
(define-function pbr/microfacet-distribution ((pbr-inputs pbr-info))
  (with-slots (alpha-roughness n-dot-h) pbr-inputs
    (let* ((roughness-squared (* alpha-roughness alpha-roughness))
           (f (+ (* (- (* n-dot-h roughness-squared)
                       n-dot-h)
                    n-dot-h)
                 1.0)))
      (/ roughness-squared (* +pi+ f f)))))

(define-function pbr/diffuse ((pbr-inputs pbr-info))
  (/ (diffuse-color pbr-inputs) +pi+))

(define-function frag/damaged-helmet ((vert-normal :vec3)
                                      (uv1 :vec2)
                                      (world-pos :vec3)
                                      &uniform
                                      (view :mat4)
                                      (metallic-roughness-values :vec2)
                                      (metallic-roughness-sampler :sampler-2d)
                                      (base-color-sampler :sampler-2d)
                                      (base-color-factor :vec4)
                                      (normal-sampler :sampler-2d)
                                      (normal-scale :float)
                                      (light-direction :vec3)
                                      (light-color :vec3)
                                      (occlusion-sampler :sampler-2d)
                                      (occlusion-strength :float)
                                      (emissive-sampler :sampler-2d)
                                      (emissive-factor :float))
  ;; Metallic and Roughness material properties are packed together
  ;; In glTF, these factors can be specified by fixed scalar values
  ;; or from a metallic-roughness map
  (let* ((min-roughness 0.04)
         (perceptual-roughness (.y metallic-roughness-values))
         (metallic (.x metallic-roughness-values))
         (mr-sample (texture metallic-roughness-sampler uv1))
         (perceptual-roughness (* (.g mr-sample) perceptual-roughness))
         (metallic (* (.b mr-sample) metallic))
         (perceptual-roughness (clamp perceptual-roughness min-roughness 1.0))
         (metallic (saturate metallic))
         ;; roughness is authored as perceptual roughness, as is convention
         ;; convert to material roughness by squaring the perceptual roughness.
         (alpha-roughness (* perceptual-roughness perceptual-roughness))
         ;; we also have a basecolor map
         (base-color (* (fl.gpu.color:srgb->rgb (texture base-color-sampler uv1))
                        base-color-factor))
         (f0 (vec3 0.04))
         (diffuse-color (* (.rgb base-color) (- (vec3 1.0) f0)))
         (diffuse-color (* diffuse-color (- 1.0 metallic)))
         (specular-color (mix f0 (.rgb base-color) metallic))
         ;; compute reflectance
         (reflectance (max (max (.r specular-color) (.g specular-color))
                           (.b specular-color)))
         ;; For typical incident reflections range (between 4% and 100%) set
         ;; the grazing reflectance to 100% for typical fresnel effect.
         ;; For very low reflectance range on highly diffuse objects (below 4%)
         ;; incrementally reduce grazing reflectance to 0%.
         (reflectance-90 (clamp (* reflectance 25.0) 0.0 1.0))
         (specular-environment-r0 (.rgb specular-color))
         (specular-environment-r90 (* (vec3 1.0) reflectance-90))
         ;; normal at surface point
         (n (pbr/get-normal world-pos vert-normal uv1
                            normal-sampler normal-scale))
         ;; camera pos is taken from view transform.
         ;; TODO confirm this undo of the translation is right.
         ;; I may have to pass the actual camera world-pos into here.
         (camera-pos (- (.xyz (aref view 3))))
         ;; vector from surface point to camera
         (v (normalize (- camera-pos world-pos)))
         ;; vector from surface point to light.
         (l (normalize light-direction))
         ;; half vector between both h andl
         (h (normalize (+ l v)))
         (reflection (- (normalize (reflect v n))))
         (n-dot-l (clamp (dot n l) 0.001 1.0))
         (n-dot-v (+ (abs(dot n v)) 0.001))
         (n-dot-h (saturate (dot n h)))
         (l-dot-h (saturate (dot l h)))
         (v-dot-h (saturate (dot v h)))
         ;; package it up.
         (pbr-inputs (make-pbr-info n-dot-l n-dot-v n-dot-h l-dot-h v-dot-h
                                    perceptual-roughness metallic
                                    specular-environment-r0 specular-environment-r90
                                    alpha-roughness
                                    diffuse-color specular-color))
         ;; Calculate shading terms for the microfacet specular shading model
         (f (pbr/specular-reflection pbr-inputs))
         (g (pbr/geometric-occlusion pbr-inputs))
         (d (pbr/microfacet-distribution pbr-inputs))
         ;; Calculate of analytical lighting contribution
         (diffuse-contrib (* (- (vec3 1.0) f)
                             (pbr/diffuse pbr-inputs)))
         (spec-contrib (/ (* f g d) (* 4.0 n-dot-l n-dot-v)))
         ;; Obtain final intensity as reflections (BRDF) scaled by the energy of
         ;; the light (cosine law)
         (color (* n-dot-l light-color (+ diffuse-contrib spec-contrib)))
         ;; TODO: Skip IBL computation until I get cube maps in.
         ;; We assume we have an occlusion map
         (ao (.r (texture occlusion-sampler uv1)))
         (color (mix color (* color ao) occlusion-strength))
         ;; We assume we have an emissive map, too
         (emissive (* (.rgb (fl.gpu.color:srgb->rgb (texture emissive-sampler uv1)))
                      emissive-factor))
         (color (+ color emissive)))
    (vec4 (pow color (vec3 (/ 2.2))) (.a base-color))))

(define-shader damaged-helmet ()
  (:vertex (vert/damaged-helmet :vec3 :vec3 :vec3 :vec4 :vec2 :vec2 :vec4 :vec4))
  (:fragment (frag/damaged-helmet :vec3 :vec2 :vec3)))
