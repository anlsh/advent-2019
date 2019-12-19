(in-package #:advent2019)

(defun sim-with-nv (fname n v)
  (let (numvec)
    (with-open-file (strm fname)
      (let ((numls (map 'list #'parse-integer
                        (split-sequence:split-sequence #\, (read-line strm)))))
        (setf numvec (make-array (length numls) :initial-contents numls))))
    (setf (aref numvec 1) n
          (aref numvec 2) v)
    (simulate (make-string-input-stream "1") t numvec)
    (aref numvec 0)))

(defun solve-day2b (fname)
  (loop for n from 0 to 99 do
    (loop for v from 0 to 99 do
      (when (= (sim-with-nv fname n v) 19690720)
        (return-from solve-day2b (+ v (* 100 n)))))))

(defun solve-day2 (fname)
  (format t "Part (a): ~a~%" (sim-with-nv fname 12 2))
  (format t "Part (b): ~a" (solve-day2b fname)))
