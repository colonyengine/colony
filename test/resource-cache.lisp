(in-package #:colony.test)

(define-test suite/resource-cache)

(define-test suite/resource-cache/cache-item)
(define-test suite/resource-cache/cache-domain)
(define-test suite/resource-cache/warming-protocol)

(define-test suite/resource-cache
  :depends-on (suite/resource-cache/cache-item
               suite/resource-cache/cache-domain
               suite/resource-cache/warming-protocol))
;; ---------------------------------------------------------------------------
;; CACHE-ITEM tests
;; ---------------------------------------------------------------------------
(define-test cache-item/smoke :parent suite/resource-cache/cache-item
  (let ((ci (rc:make-cache-item
             :opaque-data 42
             :policy :locked
             :state :cached
             :location :cl-heap
             :size 1024
             :value (make-array 1024))))

    (is = (rc:opaque-data ci) 42)
    (is eq (rc:policy ci) :locked)
    (is eq (rc:state ci) :cached)
    (is eq (rc:location ci) :cl-heap)
    (is = (rc:size ci) 1024)
    (true (arrayp (rc:value ci)))))


;; ---------------------------------------------------------------------------
;; CACHE-DOMAIN tests
;; ---------------------------------------------------------------------------
(define-test cache-domain/smoke :parent suite/resource-cache/cache-domain
  (let ((key0 '(abc . 0))
        (key1a '(ijk . 0))
        (key1b '(xyz . 0))
        (val0 "path/to/file.png")
        (val1 "another/path/to/file.png")
        ;; TODO: Make the API cognizant if I try to use more keys than
        ;; possible. Should we error? Should we just extend using EQUAL by
        ;; default? Maybe put a flag in the cache-domain to let us do one or
        ;; the other by choice?
        (cd (rc:make-cache-domain :texture (list #'equal #'equal))))

    (is eq (rc:cdref cd key0) nil)
    (setf (rc:cdref cd key0) val0)
    (is string= (rc:cdref cd key0) val0)

    (is eq (rc:cdref cd key1a key1b) nil)
    (setf (rc:cdref cd key1a key1b) val1)
    (is string= (rc:cdref cd key1a key1b) val1)

    (rc:cdrem cd key0)
    (is eq (rc:cdref cd key0) nil)

    (rc:cdrem cd key1a key1b)
    (is eq (rc:cdref cd key1a key1b) nil)))


;; ---------------------------------------------------------------------------
;; RESOURCE-CACHE tests
;; ---------------------------------------------------------------------------
(define-test resource-cache/smoke :parent suite/resource-cache
  (let ((val1 "a/b/c")
        (val2 "/file/path")
        (val3 100)
        (rc (rc:make-resource-cache `((:foo (,#'eql ,#'eql))))))

    (rc:ensure-cache-domain rc :bar `(,#'equal))

    ;; Check inserting into a cache-domain is legit.
    (is eq (rc:rcref rc :foo :a 10) nil)
    (setf (rc:rcref rc :foo :a 10) val1)
    (is string= (rc:rcref rc :foo :a 10) val1)
    ;; Check removing from a cache-domain is legit.
    (rc:rcrem rc :foo :a 10)
    (is eq (rc:rcref rc :foo :a 10) nil)

    ;; Check removing the cache-domain itself is legit.
    (true (rc:rcrefd rc :foo))
    (rc:rcremd rc :foo)
    (false (rc:rcrefd rc :foo))

    ;; Check inserting into a cache-domain is legit again.
    (is eql (rc:rcref rc :bar val2) nil)
    (setf (rc:rcref rc :bar val2) val3)
    (is eql (rc:rcref rc :bar val2) val3)
    ;; Chech removing from a cache-domain is lefit again.
    (rc:rcrem rc :bar val2)
    (is eql (rc:rcref rc :bar val2) nil)

    ;; Check removing the cache-domain itself is legit.
    (true (rc:rcrefd rc :bar))
    (rc:rcremd rc :bar)
    (false (rc:rcrefd rc :bar))
    ))

;; ---------------------------------------------------------------------------
;; WARMING-PROTOCOL tests
;; ---------------------------------------------------------------------------

;; The caching task test is to convert a key which is a string to a value which
;; is the length of the string.
(defclass warmer-test-caching-task (rc:caching-task) ())

;; We forgo actually using the resource-cache.
(defmethod rc:consider-caching-task ((caching-task warmer-test-caching-task)
                                    (resource-cache-scheduler
                                     rc:resource-cache-scheduler))

  (values :reserved caching-task))

(defmethod rc:compute-caching-task ((caching-task warmer-test-caching-task)
                                    (resource-cache-scheduler
                                     rc:resource-cache-scheduler))
  (setf (rc:value caching-task) (length (rc:key caching-task)))
  (values :computed caching-task))

(defmethod rc:synchronize-from-caching-task ((caching-task
                                              warmer-test-caching-task)
                                             (resource-cache-scheduler
                                              rc:resource-cache-scheduler))
  (assert (= (rc:value caching-task) (length (rc:key caching-task))))
  (values :synchronized caching-task))

(defmethod rc:dispose-caching-task ((caching-task warmer-test-caching-task)
                                    (resource-cache-scheduler
                                     rc:resource-cache-scheduler))
  (values :disposed caching-task))

(define-test warming-protocol/smoke
  :parent suite/resource-cache/warming-protocol
  (let* ((num-tasks 10)
         (scheduler (rc:make-resource-cache-scheduler :core nil))
         (executor (rc:make-resource-cache-executor :sequential :core nil))
         (db #("hi-there-" "stuff-" "foo-"))
         (db-len (length db)))
    ;; allocate the tasks
    (loop :repeat num-tasks
          :do (rc:acquire-caching-task
               scheduler 'warmer-test-caching-task :test-domain
               :key (string-downcase
                     (symbol-name
                      (gensym (aref db (random db-len)))))))

    ;; Then schedule and execute them
    (let ((total (rc:execute executor scheduler)))
      (is eql total num-tasks))))
