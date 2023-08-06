(in-package #:virality.uuid)

(defstruct (uuid
            (:constructor %make-uuid)
            (:conc-name nil)
            (:predicate nil)
            (:copier nil))
  (version 4 :type fixnum)
  (variant :rfc-4122)
  (low 0 :type u:ub64)
  (high 0 :type u:ub64))
