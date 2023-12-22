(in-package #:virality.texture-map)

;; NIL is a reserved name which means "anonymous texture-map".
;; Return three values:
;; First: the name of the texture (even if assigned due to it being anonymous).
;; Second: The tmap AST instance for this texture map.
;; Third: If there were additional texture-maps defined, like say faces of a
;;        cube map, then return all the ASTs for those additional texture maps
;;        in a list.
(defmacro define-texture-map (name data-model &body body)
  (u:with-gensyms (tmap extra-tmaps-list context anonymous-p canon-name)
    (u:mvlet ((model style store (parse-data-model data-model)))
      ;;; TODO
      ;; Make parse-texture-map returns two values: The first value is the
      ;; texture map AST instance described by the define-texture-map form.
      ;; The second value is a list of any additional texture-map AST instances
      ;; that were created as a result of processing this form.  In the case of
      ;; non-cube texture-maps, there will be only the first value and a nil
      ;; second value, but a cube map *may* return 6 additional texture maps
      ;; that might need to go into the meta storage!
      `(multiple-value-bind (,tmap ,extra-tmaps-list)
           ,(parse-texture-map name model style (car store) body)

         (u:comment "Attempt to put this texture-map into the right place.")
         (with-accessors ((,anonymous-p texmap::anonymous-p)
                          (,canon-name texmap::name)) ,tmap
           (let ((,context (abag:sattr ,tmap :context)))

             ;; TODO: Fix this below to figure out what to do with the extra
             ;; texture-map instances if they are present. We need to figure
             ;; out how, or when, they should get put into the metaspace or if
             ;; it should be put there at all.

             ;; Figure out if we can automatically book keep this texture-map
             ;; (and any extra-tmaps) under the conditions of its creation.  We
             ;; check in a particular order for performance reasons.
             (cond
               ;;; CASE 0
               ;; This means we likely expanded into runtime code and we're
               ;; making an anonymous texture-map which we'll poke into the
               ;; context.
               ((and ,context ,anonymous-p)
                ;; TODO: Reify texture-map AST(s) into CORE. Implement me very
                ;; soon.
                (error "define-texture-map: Handle case 0"))

               ;;; CASE 1
               ;; TODO: It is possible to assign a meaning here of "just return
               ;; the unreified AST and let the appdev deal with it by reifying
               ;; the tmaps manually". But for now, we just error.
               ((and (not ,context) ,anonymous-p)
                (error "define-texture-map: Handle case 1."))

               ;;; CASE 2
               ;; We don't have a meaning for this situation yet.
               ((and ,context (not ,anonymous-p))
                (error "define-texture-map: Handle case 2."))

               ;;; CASE 3
               ;; This means we generally evaluated at toplevel, are in a
               ;; live-coding situation, or it was typed into the REPL.  So
               ;; insert it into the metaspace and/or update interactively.
               ((and (not ,context) (not ,anonymous-p))
                ;; TODO: deal with updating interactively too in a live coding
                ;; situation--including the extra-tmaps.
                (setf (u:href v::=meta/texture-maps= ,canon-name) ,tmap)))

             (values ,canon-name ,tmap ,extra-tmaps-list)))))))
