(in-package :fl.core)

(defmethod extension-file-type ((extension-type (eql 'settings)))
  "cfg")

(defmethod prepare-extension ((extension-type (eql 'settings)) owner path)
  (let ((%temp-settings (make-hash-table)))
    (declare (special %temp-settings))
    (flet ((%prepare ()
             (load-extensions extension-type path)
             %temp-settings))
      (maphash
       (lambda (key value)
         (setf (gethash key (settings owner)) value))
       (%prepare)))))

(defun cfg (context key)
  (gethash key (settings context)))

(defun (setf cfg) (value context key)
  (check-type key symbol)
  (setf (gethash (make-keyword key) (settings context)) value))

(defmacro with-cfg (options context &body body)
  `(symbol-macrolet
       (,@(loop :for option :in options
                :collect `(,option (cfg ,context ,(make-keyword option)))))
     ,@body))

(defmacro define-settings (() &body body)
  `(let ()
     (declare (special %temp-settings))
     (loop :for (key value) :on ',@body :by #'cddr
           :do (setf (gethash key %temp-settings) value))))
