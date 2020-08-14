(in-package #:virality)

(defun calculate-font-space-width (font-spec)
  (let ((chars (font:chars font-spec)))
    (or (getf (u:href chars #\space) :xadvance)
        (getf (u:href chars #\n) :xadvance)
        (/ (loop :for char :in (u:hash-values chars)
                 :sum (or (getf char :xadvance) 0))
           (hash-table-count chars)))))

(defun get-font-character-data (font-spec char)
  (let ((chars (font:chars font-spec)))
    (or (u:href chars char)
        (u:href chars :invalid)
        (u:href chars (code-char #xfffd))
        '(:xoffset 0 :yoffset 0 :x 0 :y 0 :width 0 :height 0 :xadvance 0))))

(defun map-font-glyphs (font-spec func string)
  (loop :with w = (float (font:scale-w font-spec) 1f0)
        :with h = (float (font:scale-h font-spec) 1f0)
        :with x = 0
        :with y = (font:line-height font-spec)
        :with kernings = (font:kernings font-spec)
        :with line-height = (font:line-height font-spec)
        :with line-widths = '(0)
        :with space-width = (calculate-font-space-width font-spec)
        :for i :from 0 :below (length string)
        :for char = (aref string i)
        :for char-data = (get-font-character-data font-spec char)
        :for p = nil :then char
        :for kerning = (or (u:href kernings (cons p char)) 0)
        :do (case char
              (#\newline
               (push x line-widths)
               (setf x 0)
               (incf y line-height))
              (#\space
               (incf x space-width))
              (#\tab
               (incf x (* 8 space-width)))
              (t
               (incf x kerning)
               (let ((x- (+ x (getf char-data :xoffset)))
                     (y- (+ y (getf char-data :yoffset)))
                     (x+ (+ x
                            (getf char-data :xoffset)
                            (getf char-data :width)))
                     (y+ (+ y
                            (getf char-data :yoffset)
                            (getf char-data :height)))
                     (u- (/ (getf char-data :x) w))
                     (v+ (/ (getf char-data :y) h))
                     (u+ (/ (+ (getf char-data :x)
                               (getf char-data :width))
                            w))
                     (v- (/ (+ (getf char-data :y)
                               (getf char-data :height))
                            h)))
                 (psetf y- (- line-height y+)
                        y+ (- line-height y-))
                 (funcall func x- y- x+ y+ u- v- u+ v+))
               (incf x (getf char-data :xadvance))))
        :finally (return (values (float (/ (max x (apply #'max line-widths)) 2)
                                        1f0)
                                 (float (/ y 2) 1f0)))))

(defun calculate-font-position (spec position dimensions offset)
  (let* ((line-height (float (font:line-height spec) 1f0))
         (viewport-size (v2:vec 1920 1080))
         (dims (v2:/ (v2:scale dimensions 2f0) viewport-size))
         (offset (v2:scale (v2:/ offset viewport-size) line-height)))
    (case position
      (:top-left
       (v2:+ (v2:vec -1 1) offset))
      (:top-right
       (v2:+ (v2:vec (- 1 (v2:x dims)) 1) offset))
      (:bottom-left
       (v2:+ (v2:vec -1 (1- (v2:y dims))) offset))
      (:bottom-right
       (v2:+ (v2:vec (- 1 (v2:x dims)) (1- (v2:y dims))) offset))
      (:center-left
       (v2:+ (v2:vec -1 (/ (v2:y dims) 2)) offset))
      (:center-right
       (v2:+ (v2:vec (- 1 (v2:x dims)) (/ (v2:y dims) 2)) offset))
      (:center-top
       (v2:+ (v2:vec (/ (v2:x dims) -2) 1) offset))
      (:center-bottom
       (v2:+ (v2:vec (/ (v2:x dims) -2) (1- (v2:y dims))) offset))
      (t (v2:+ (v2:/ dims (v2:vec -2 2)) offset)))))
