(in-package :%fl)

(defmethod extension-file-type ((extension-type (eql :settings)))
  "cfg")

(defmethod prepare-extension ((extension-type (eql :settings)) core-state)
  (let ((%temp (fl.util:dict #'eq)))
    (declare (special %temp))
    (flet ((%prepare ()
             (map-extensions (context core-state) extension-type)
             %temp))
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
  `(locally (declare (special %fl::%temp))
     (loop :for (key value) :on ',@body :by #'cddr
           :do (setf (fl.util:href %fl::%temp key) value))))
