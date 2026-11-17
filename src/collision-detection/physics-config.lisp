(in-package #:colony)

(defun make-physics-config (name physics-layers collision-plan)
  "Create a PHYSICS-CONFIG instance. PHYSICS-LAYERS is a list of symbols
each representing a valid physics layer. COLLISION-PLAN is a list of
lists in the form:
 ((A (A B C ...))
  (B (D E F ...))
  (G ())
  ...)
where A collides with (A B C ...) and B collides with (D E F ...) and so on.
The example form for G represents a layer that collides with nothing.
Return the PHYSICS-CONFIG instance."
  (make-instance 'physics-config
                 :name name
                 :physics-layers (copy-seq physics-layers)
                 :collision-plan
                 (let ((table (u:dict #'eq)))
                   (loop :for (layer collides-with-layers)
                           :in (copy-seq collision-plan)
                         :do (setf (u:href table layer)
                                   (copy-seq collides-with-layers)))
                   table)))

(defun make-physics-config-descriptor (name original-form constructor-form
                                       constructor)
  (make-instance 'physics-config-descriptor
                 :name name
                 :original-form original-form
                 :constructor-form constructor-form
                 :constructor constructor))

;; -----------------------------------------------------------------------

(defun gen-physics-config-constructor (name def-layers plan)
  `(lambda ()
     (make-physics-config ',name ',def-layers ',plan)))

;; TODO: This is rough draft code. Redo this processing so we check for
;; syntax error or other problems. Currently this code only accepts the
;; exactly right format of the DSL and checks for semantic errors only.
(defun parse-physics-config (name options body)
  (declare (ignore options))
  (flet ((syntax-error (name fmt &rest args)
           (apply #'error
                  (concatenate 'string
                               "Syntax for define-physics-config ~(~S~): "
                               fmt)
                  name args)))
    (let ((physics-layers (find 'physics-layers body :key #'first)))
      (unless physics-layers
        (syntax-error name "Missing physics-layer form!"))

      ;; TODO: This form is fragile to signaling a condition if it can't
      ;; destructure a wrong form.
      (destructuring-bind (pl-sym (defs-sym &rest defs) (cplan-sym spec))
          physics-layers
        ;; NOTE: pl-sym has already been found by FIND
        (declare (ignore pl-sym))

        (unless (eq defs-sym 'definitions)
          (syntax-error name "Bad definitions form!"))

        (unless (eq cplan-sym 'collision-plan)
          (syntax-error name "Bad collision-plan form!"))

        (let ((def-vars (mapcar #'first defs))
              (def-layers (mapcar #'second defs)))
          (cond
            ;; Process the TABLE-SPEC syntax
            ((eq (first spec) 'table-spec)
             (let* ((header-vars (caadr spec))
                    (header-len (length header-vars))
                    (rows (cdadr spec))
                    (row-vars (mapcar #'first rows)))
               (unless (equal header-vars def-vars)
                 (syntax-error name
                               (u:cat "Header vars ~(~S~) are not equal "
                                      "in length or order to definition "
                                      "vars: ~(~S~)")
                               header-vars def-vars))
               (unless (equal row-vars header-vars)
                 (syntax-error name
                               (u:cat "Row vars ~(~S~) do not specify an "
                                      "individual entry in the order of "
                                      "each header var: ~(~S~)")
                               row-vars header-vars))
               (loop :for idx :from 1 :upto header-len
                     :for row :in rows
                     :for row-len = (length row)
                     :do (unless (= (1- row-len) idx)
                           (syntax-error name
                                         (u:cat "Row ~A must have ~A entries, "
                                                "but instead has ~A entries.")
                                         idx row-len))
                         (unless (null (remove-if (lambda (item)
                                                    ;; Test for name of
                                                    ;; symbol, not
                                                    ;; actual symbol.
                                                    (member (symbol-name item)
                                                            '("Y" "N")
                                                            :test #'string=))
                                                  (cdr row)))
                           (syntax-error name
                                         (u:cat "Row ~(~S~) can only have "
                                                "y or n as entries!")
                                         row)))
               ;; Finally, we can assemble the required data
               (let ((adj-list
                       (loop :for (row-var . choices) :in rows
                             :collect
                             (list (cadr (assoc row-var defs))
                                   ;; Truncates according to choices
                                   (remove-if #'null
                                              (mapcar
                                               (lambda (choice header-var)
                                                 (when (u:symbol-name= choice
                                                                       'y)
                                                   (cadr (assoc header-var
                                                                defs))))
                                               choices header-vars))))))
                 ;; Generate the thunk to construct the physics-config
                 ;; instance.
                 (gen-physics-config-constructor name def-layers adj-list))))

            ;; Process the LIST-SPEC syntax
            ((eq (first spec) 'list-spec)
             (let* ((rows (cdr spec))
                    (row-vars (mapcar #'first rows)))
               (unless (equal row-vars def-vars)
                 (syntax-error name
                               (u:cat "Row vars ~(~S~) do not specify an "
                                      "individual entry in the order of "
                                      "the definition vars: ~(~S~)")
                               row-vars def-vars))
               (loop :for row :in rows
                     :for row-var = (car row)
                     :for arrow = (cadr row)
                     :for layer-vars = (caddr row)
                     :for layer-vars-len = (length layer-vars)
                     :do (unless (>= (length row) 3)
                           (syntax-error name (u:cat "Row ~(~S~) must be at "
                                                     "least 3 elements long.")
                                         row))
                         (unless (member row-var def-vars)
                           (syntax-error name (u:cat "Row ~(~S~) must have a "
                                                     "row-var out of "
                                                     "definition vars: "
                                                     "~(~S~)")
                                         row defs))
                         (unless (eq arrow :->)
                           (syntax-error name
                                         (u:cat "Row ~(~S~) is missing a :->!")
                                         row))
                         (unless (or (null layer-vars)
                                     (>= layer-vars-len 1))
                           (syntax-error name
                                         (u:cat "Row ~(~S~) must have at least "
                                                "a row-var, an arrow, and "
                                                "nil or a list of layer vars.")
                                         row))
                         (loop :for layer-var :in layer-vars
                               :do (unless (cadr (assoc layer-var defs))
                                     (syntax-error name
                                                   (u:cat "Row ~(~S~): var "
                                                          "~(~S~) is not a "
                                                          "known layer var: "
                                                          "~(~S~)!")
                                                   row layer-var defs))))
               ;; Finally, we can assemble the required data
               (let ((adj-list
                       (loop :for (row-var arrow layer-vars) :in rows
                             :collect
                             (list (cadr (assoc row-var defs))
                                   ;; Truncates according to choices
                                   (mapcar (lambda (item)
                                             (cadr (assoc item defs)))
                                           layer-vars)))))
                 ;; Generate the thunk to construct the physics-config
                 ;; instance.
                 (gen-physics-config-constructor name def-layers adj-list))))
            (t
             (syntax-error name "Unknown spec form: ~(~S~)" spec))))))))

(defun update-physics-config/interactively (old-desc new-desc)
  (unless (eq *core-debug* :no-core-available)
    (format t (u:cat "IGNORABLE ERROR: Implement interactive update of "
                     "physics-configs: OLD[~(~S~)] -> NEW[~(~S~)]")
            (when old-desc (name old-desc))
            (when new-desc (name new-desc)))))



(defun test-define-physics-config-table ()
  (parse-physics-config
   'xxx
   nil
   '((physics-layers
      (definitions
       (A :ground)
       (B :player)
       (C :player-bullet)
       (D :enemy)
       (E :enemy-bullet)
       (F :scenery)
       (G :planet)
       (H :playfield))
      (collision-plan
       (table-spec
        ;; rows and columns are REQUIRED to be in same order.
        ((  A B C D E F G H)
         (A y)
         (B y n)
         (C y n n)
         (D y y y n)
         (E y y y n n)
         (F n n n n n n)
         (G n n n y n n n)
         (H n n y n y n n n))))))
   ))

(defun test-define-physics-config-list ()
  (parse-physics-config
   'yyy
   nil
   '((physics-layers
      (definitions
       (A :ground)
       (B :player)
       (C :player-bullet)
       (D :enemy)
       (E :enemy-bullet)
       (F :scenery)
       (G :planet)
       (H :playfield))
      (collision-plan
       (list-spec
        ;; all layers must be represented on LHS
        ;; The symbol :-> means "collides with"
        (A :-> (A))
        (B :-> (A))
        (C :-> (A))
        (D :-> (A B C))
        (E :-> (A B C))
        (F :-> ())
        (G :-> (D))
        (H :-> (C E))))))))

;; Example of table-desc format.
#++
(c:define-physics-config gun-janitor-physics ()
  (c:physics-layers
   (c:definitions
    (A :ground)
    (B :player)
    (C :player-bullet)
    (D :enemy)
    (E :enemy-bullet)
    (F :scenery)
    (G :planet)
    (H :playfield))
   (c:collision-plan
    (c:table-spec
     ;; rows and columns are REQUIRED to be in same order.
     ((  A B C D E F G H)
      (A y)
      (B y n)
      (C y n n)
      (D y y y n)
      (E y y y n n)
      (F n n n n n n)
      (G n n n y n n n)
      (H n n y n y n n n))))))

;; Example of list-desc format.

#++
(c:define-physics-config gun-janitor-physics ()
  (c:physics-layers
   (c:definitions
    (A :ground)
    (B :player)
    (C :player-bullet)
    (D :enemy)
    (E :enemy-bullet)
    (F :scenery)
    (G :planet)
    (H :playfield))
   (c:collision-plan
    (c:list-spec
     ;; all layers must be represented on LHS
     ;; The symbol :-> means "collides with"
     (A :-> (A))
     (B :-> (A))
     (C :-> (A))
     (D :-> (A B C))
     (E :-> (A B C))
     (F :-> ())
     (G :-> (D))
     (H :-> (C E))))))
