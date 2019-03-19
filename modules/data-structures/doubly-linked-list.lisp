(in-package :first-light.data-structures)

(defstruct (dlist (:constructor %make-dlist (&key test)))
  "A doubly linked list that holds sequential nodes with links to the previous and next node."
  head
  tail
  test)

(defstruct dlist-node
  "A doubly linked list node with references to the previous and next node in the list."
  key
  value
  previous
  next)

(defmethod print-object ((object dlist) stream)
  (print-unreadable-object (object stream :type t)
    (format stream "~s" (dlist-elements object))))

(defmethod print-object ((object dlist-node) stream)
  (print-unreadable-object (object stream :type t)
    (format stream "(~s ~s)" (dlist-node-key object) (dlist-node-value object))))

(defun make-dlist (&key (test #'eql))
  "Create a new doubly linked list. TEST is a function used for comparing the keys of nodes."
  (%make-dlist :test test))

(defun find-dlist-node (dlist key &key from-end)
  "Find the first node in DLIST with the given KEY. If FROM-END is non-nil, then DLIST is searched
in reverse, from end to start."
  (when (dlist-head dlist)
    (let ((start-func (if from-end #'dlist-tail #'dlist-head))
          (hop-func (if from-end #'dlist-node-previous #'dlist-node-next)))
      (do ((node (funcall start-func dlist) (funcall hop-func node)))
          ((or (null node) (funcall (dlist-test dlist) (dlist-node-key node) key))
           node)))))

(defun %insert-dlist-node (dlist before after key value)
  (let ((node (make-dlist-node :key key :value value :previous before :next after)))
    (if before
        (setf (dlist-node-next before) node)
        (setf (dlist-head dlist) node))
    (if after
        (setf (dlist-node-previous after) node)
        (setf (dlist-tail dlist) node))
    node))

(defgeneric insert-dlist-node (where dlist key value &key &allow-other-keys)
  (:documentation "Insert a new node constructed from KEY and VALUE into DLIST. WHERE specifies
  where in DLIST to place the new node."))

(defmethod insert-dlist-node ((where (eql :head)) dlist key value &key)
  "Insert a new node constructed from KEY and VALUE into DLIST. The node is placed at the beginning
of DLIST."
  (%insert-dlist-node dlist nil (dlist-head dlist) key value))

(defmethod insert-dlist-node ((where (eql :tail)) dlist key value &key)
  "Insert a new node constructed from KEY and VALUE into DLIST. The node is placed at the end of
DLIST."
  (%insert-dlist-node dlist (dlist-tail dlist) nil key value))

(defmethod insert-dlist-node ((where (eql :before)) dlist key value &key from-end target-key)
  "Insert a new node constructed from KEY and VALUE into DLIST. The node is placed before the node
having a key of TARGET-KEY. If FROM-END is non-nil, then DLIST is searched in reverse, from end to
start."
  (au:when-let ((node (find-dlist-node dlist target-key :from-end from-end)))
    (%insert-dlist-node dlist (dlist-node-previous node) node key value)))

(defmethod insert-dlist-node ((where (eql :after)) dlist key value &key from-end target-key)
  "Insert a new node constructed from KEY and VALUE into DLIST. The node is placed after the node
having a key of TARGET-KEY. If FROM-END is non-nil, then DLIST is searched in reverse, from end to
start."
  (au:when-let ((node (find-dlist-node dlist target-key :from-end from-end)))
    (%insert-dlist-node dlist node (dlist-node-next node) key value)))

(defun remove-dlist-node (dlist key &key from-end)
  "Remove the first node found having KEY from DLIST. If FROM-END is non-nil, then DLIST is searched
in reverse, from end to start."
  (au:when-let ((node (find-dlist-node dlist key :from-end from-end)))
    (let ((before (dlist-node-previous node))
          (after (dlist-node-next node)))
      (if before
          (setf (dlist-node-next before) after)
          (setf (dlist-head dlist) after))
      (if after
          (setf (dlist-node-previous after) before)
          (setf (dlist-tail dlist) before)))
    node))

(defun remove-dlist-nodes (dlist &rest keys)
  "Remove all nodes from DLIST with the given KEYS."
  (dolist (key keys)
    (remove-dlist-node dlist key))
  dlist)

(defun update-dlist-node-key (node new-key)
  (setf (dlist-node-key node) new-key))

(defun dlist-elements (dlist)
  "Get an association list of node keys and values of DLIST."
  (loop :for node = (dlist-head dlist) :then (dlist-node-next node)
        :while node
        :collect (cons (dlist-node-key node) (dlist-node-value node))))
