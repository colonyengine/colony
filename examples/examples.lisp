(in-package #:virality.examples)

(defun start (scene-name)
  (let ((scene (a:format-symbol :virality.examples "~:@(~a~)" scene-name)))
    (v:start :project :virality.examples
             :scene scene)))
