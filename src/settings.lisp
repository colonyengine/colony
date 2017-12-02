(in-package :first-light)

(defmethod extension-file-type ((extension-type (eql 'settings)))
  "cfg")

(defmethod prepare-extension ((extension-type (eql 'settings)) owner path)
  (loop :with forms = (collect-extension-forms extension-type path)
        :for form :in forms
        :do (loop :for (key value) :on form :by #'cddr
                  :do (setf (gethash key (settings owner)) value))))

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
