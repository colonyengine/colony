(in-package #:first-light.binary-formats)

(defun %uncompress-octets (octet-vector compression-scheme)
  (chipz:decompress nil compression-scheme octet-vector
                    :buffer-size (* (length octet-vector) 2)))

(defun %string-length (bytes null-terminated-p)
  (with-slots (%sequence) *buffer*
    (let* ((max-length (or bytes (length %sequence)))
           (start (buffer-position))
           (end (min (length %sequence) (+ start max-length)))
           (index (if null-terminated-p
                      (position 0 %sequence :start start :end end)
                      end)))
      (- index start))))

(defun split-string (string delimiter)
  (let ((pos (position delimiter string)))
    (list (subseq string 0 pos)
          (subseq string (1+ pos)))))

(defun uncompress-bzip2 (octet-vector)
  (%uncompress-octets octet-vector 'chipz:bzip2))

(defun uncompress-gzip (octet-vector)
  (%uncompress-octets octet-vector 'chipz:gzip))

(defun uncompress-zlib (octet-vector)
  (%uncompress-octets octet-vector 'chipz:zlib))

(defun uncompress-deflate (octet-vector)
  (%uncompress-octets octet-vector 'chipz:deflate))
