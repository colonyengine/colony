(in-package :fl.core)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Handling shader-stages extension
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod extension-file-type ((extension-type (eql 'shader-stages)))
  "shader-stages")

(defmethod prepare-extension ((extension-type (eql 'shader-stages)) owner path)
  ;; These extensions are simply CL files containing shader stages and
  ;; structures such as DEFSTRUCT-GPU and DEFUN-GPU.
  (load-extensions extension-type path))


;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Handling shader-programs extension
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod extension-file-type ((extension-type (eql 'shader-programs)))
  "shader-programs")

(defmethod prepare-extension ((extension-type (eql 'shader-programs))
                              owner path)
  ;; These extensions are simply CL files containing MAKE-SHADER-PROGRAM forms
  ;; which should now have all available shader stages available to them.
  (load-extensions extension-type path))
