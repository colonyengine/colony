(in-package :%fl)

(defmethod extension-file-type ((extension-type (eql :settings)))
  "cfg")

(defmethod prepare-extension ((extension-type (eql :settings)) core-state)
  (let ((%temp-settings (fl.util:dict #'eq)))
    (declare (special %temp-settings))
    (flet ((%prepare ()
             (map-extensions (context core-state) extension-type)
             %temp-settings))
      (maphash
       (lambda (key value)
         (setf (fl.util:href (settings core-state) key) value))
       (%prepare)))))

(defun cfg (context key)
  (fl.util:href (settings context) key))

(defun (setf cfg) (value context key)
  (check-type key symbol)
  (setf (fl.util:href (settings context) (fl.util:make-keyword key)) value))

(defmacro with-cfg (options context &body body)
  `(symbol-macrolet
       (,@(loop :for option :in options
                :collect `(,option (cfg ,context ,(fl.util:make-keyword option)))))
     ,@body))

(defmacro define-settings (() &body body)
  `(locally (declare (special %temp-settings))
     (loop :for (key value) :on ',@body :by #'cddr
           :do (setf (fl.util:href %temp-settings key) value))))
