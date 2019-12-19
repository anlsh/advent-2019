(in-package #:advent2019)

(defun solve-day5 (fname)
  (let (numvec)
    (with-open-file (strm fname)
      (let ((numls (map 'list #'parse-integer
                        (split-sequence:split-sequence #\, (read-line strm)))))
        (setf numvec (make-array (length numls) :initial-contents numls))))
    (format t "Part (a): ~a~%"
            (simulate (make-string-input-stream "1") t numvec))
    (format t "Part (b): ~a~%"
            (simulate (make-string-input-stream "5") t numvec))))
