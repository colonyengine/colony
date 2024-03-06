(in-package #:virality.test)

;;; Test suites.
(define-test suite/attribute-value)
(define-test suite/attribute-bag
  :depends-on (suite/attribute-value))


;;; --------------------------------------------------------------------------
;; Attribute Value tests
;;; --------------------------------------------------------------------------

(define-test attribute-value-make-0
  :parent suite/attribute-value
  (let ((v (abag:make-attribute-value)))

    (false (abag:semantic-value-bound-p v))
    (true (abag:dirty v))
    (false (abag:computed-value-bound-p v))))

;;; --------------------------------------------------------------------------
;; Attribute Bag tests
;;; --------------------------------------------------------------------------

;; TODO: This is quick and dirty and not nearly complete.
(define-test attribute-bag-overlay-0
  :parent suite/attribute-bag

  (let ((bag0 (abag:make-attribute-bag))
        (bag1 (abag:make-attribute-bag)))
    (setf (abag:sattr bag0 :foo) 42
          (abag:sattr bag0 :bar) (list 1 2 3 4)
          (abag:sattr bag0 :qux) 32.5)
    (setf (abag:cattr bag0 :foo) 420
          (abag:cattr bag0 :bar) (list 10 20 30 40)
          (abag:cattr bag0 :qux) 320.5)

    ;;(format t "Attribute Bag 0~%")
    ;;(abag:dump-attribute-bag bag0)

    (setf (abag:sattr bag1 :foo) 200
          (abag:sattr bag1 :bar) (list 2 3 4 5)
          (abag:sattr bag1 :quux) 32.5)
    (setf (abag:cattr bag1 :foo) 800
          (abag:cattr bag1 :bar) (list 20 30 40 50)
          (abag:cattr bag1 :quux) 320.5)
    ;;(format t "Attribute Bag 1~%")
    ;;(abag:dump-attribute-bag bag1)

    (let ((bag2 (abag:make-attribute-bag bag0 bag1)))
      ;;(format t "Attribute Bag 2~%")
      ;;(abag:dump-attribute-bag bag2)

      ;; Check no attribute-value objects are shared between any bags
      (loop :for at :in '(:foo :bar :qux :quux)
            :do (loop :for (bl br) :in `((,bag0 ,bag1)
                                         (,bag0 ,bag2)
                                         (,bag1 ,bag2))
                      :do (isnt eq (abag:attr bl at) (abag:attr br at))))

      ;; Check no attribute-value semantic or computed objects are shared
      ;; between any bags for appropriate attr-names.
      (loop :for at :in '(:bar)
            :do (loop :for (bl br) :in `((,bag0 ,bag1)
                                         (,bag0 ,bag2)
                                         (,bag1 ,bag2))
                      :do (isnt eq (abag:sattr bl at) (abag:sattr br at))
                          (isnt eq (abag:cattr bl at) (abag:cattr br at))))
      )))

(define-test attribute-bag-clone-0
  :parent suite/attribute-bag

  (let ((bag0 (abag:make-attribute-bag)))
    (setf (abag:sattr bag0 :foo) 42
          (abag:sattr bag0 :bar) (list 1 2 3 4)
          (abag:sattr bag0 :qux) 32.5)
    (setf (abag:cattr bag0 :foo) 420
          (abag:cattr bag0 :bar) (list 10 20 30 40)
          (abag:cattr bag0 :qux) 320.5)
    ;;(format t "Attribute Bag 0~%")
    ;;(abag:dump-attribute-bag bag0)

    (u:mvlet* ((eql-map (clone:make-eql-map-with-stats))
               (bag1 eql-map (clone::clone-deep bag0 eql-map)))

      ;;(format t "Attribute Bag 1~%")
      ;;(abag:dump-attribute-bag bag1)

      ;; Check no attribute-value objects are shared between any bags
      (loop :for at :in '(:foo :bar :qux)
            :do (loop :for (bl br) :in `((,bag0 ,bag1))
                      :do (isnt eq (abag:attr bl at) (abag:attr br at))))

      ;; Check no attribute-value semantic or computed objects are shared
      ;; between any bags for appropriate attr-names.
      (loop :for at :in '(:bar)
            :do (loop :for (bl br) :in `((,bag0 ,bag1))
                      :do (isnt eq (abag:sattr bl at) (abag:sattr br at))
                          (isnt eq (abag:cattr bl at) (abag:cattr br at))))

      ;;(clone:eql-map-dump-stats eql-map)
      )))
