

;; Texture Map Symbol Name: A-B-C-D-E-F
;; A: GNNN for Group Id NNN
;; B: Name, no dashes
;; C: LOG for Logical
;;    PHY for Physical
;;    IMP for manual implementation using the texture-map programming API.
;; D: INF for anything at all being inferred
;;    GND for no inference required
;; E: ONE for exactly one base mipmap (required for things with one image only)
;;    ALL for all mipmaps specified.
;;    CON for partial list, but continuous set of mipmaps.
;;      Example: mipmaps 2 3 4 5 out of 0 1 2 3 4 5 6 7
;;    DIS for partial list, but discontinuitous (should be an error for now)
;;      Example: mipmaps 0 2 3 6 7 out of 0 1 2 3 4 5 6 7
;; F: NON for no attributes specified anywhere
;;    TMP (c:attributes ...) form specified for the texture map
;;    MIP (c:attributes ...) form specified on any mipmap/image/etc.
;;    OVR Some of the attributes override higher level attributes.
;;
;; The :physical-location and :data fields of the *-store instances are
;; expected to be filled in by the engine. Printing out a completed form will
;; result in them being filled in. A human MAY specify them and they will be
;; taken for granted as error free by the engine. Stuff like the names assigned
;; to the stores will all be converted to indices starting at 0 and growing
;; positively.
;;
;; --------------------------
;; NOTE: Multiple texture maps are always defined in the order of decreasing
;; size whether one is in logcal or physical form.
;;
;; NOTE: All c:mipmap forms are collected in order and stored in an array in
;; slot MIPMAPS inside of TEXTURE-MAP-* object. The in memory form may reorder
;; them into the largest size/area/volume first down to the smallest.
;;
;; NOTE: mapping-span-X :to MUST be X, the :from could be something else, but
;; must select a X fim thing. So, a :from for a 1d texture is usually 1d, but
;; could be a 2D 0,10 to 100,10 for example that is actually 1d.

;;
;; NOTE: All c:mapping-span are collected in order and stored in an array in
;; slot MAPPING-SPANS inside of MIPMAP-* object.
;;
;; CONFLICT: In a (:mipmap (textures foo)) form the c:attributes symbol cannot
;; be used in an asset form. Maybe this is just fine since it will be
;; exceedingly unlikely to happen due to packages.
;;
;;
;; Use a progn method to determine valid symbol k/v names for a texture and
;; how to process those k/v pairs.
;;
;; NOTES:
;;
;; mipmaps with rainbow colors. 7 levels
;; 64 32 16 8 4 2 1
;;  R  O  Y G B I V
;;
;; NOTE: span-* may have a state of :unsupported as opposed to :supported which
;; means, respectively, that the :start and :end are considered to be
;; quantities that must be inferred from the data-store's data and are not
;; currently meaningful, OR that :start and :end are supplied and correct.
;; There is a default value of :contextual for :store. This is figured out
;; almost certainly at runtime or procedural generation time.
;; --------------------------
