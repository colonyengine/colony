(in-package :defpackage+-user-1)

(defpackage+ #:first-light.data-structures
  (:nicknames #:fl.dst)
  (:use #:cl)
  ;; queues
  (:inherit-from
   #:queues
   #:make-queue
   #:qpush
   #:qpop)
  ;; doubly-linked lists
  (:export
   #:dlist-elements
   #:dlist-head
   #:dlist-node-key
   #:dlist-node-next
   #:dlist-node-previous
   #:dlist-node-value
   #:dlist-tail
   #:find-dlist-node
   #:insert-dlist-node
   #:make-dlist
   #:remove-dlist-node
   #:remove-dlist-nodes
   #:update-dlist-node-key))
