(in-package #:vutils)

(defun collect-symbols (package &key key test)
  "Collect a list of all symbols of `PACKAGE`."
  (let ((key (or key #'identity))
        (test (or test (constantly t)))
        (symbols nil))
    (do-symbols (symbol package)
      (when (funcall test symbol)
        (push (funcall key symbol) symbols)))
    (nreverse symbols)))

(defun collect-external-symbols (package &key key test)
  "Collect a list of all external symbols of `PACKAGE`."
  (let ((key (or key #'identity))
        (test (or test (constantly t)))
        (symbols nil))
    (do-external-symbols (symbol package)
      (when (funcall test symbol)
        (push (funcall key symbol) symbols)))
    (nreverse symbols)))

(defun make-keyword (object)
  "Interns `OBJECT`, a string designator or number, into the keyword package."
  (values
   (alexandria:make-keyword
    (etypecase object
      ((or string symbol)
       object)
      (number
       (format nil "~a" object))))))
