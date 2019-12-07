(defun sim-on-numvec (in-strm out-strm num-vec)
  (let ((opinfo `((1 (3 ,(lambda (n1 n2 loc)
                           (setf (aref num-vec loc) (+ n1 n2)))))
                  (2 (3 ,(lambda (n1 n2 loc)
                           (setf (aref num-vec loc) (* n1 n2)))))
                  (3 (1 ,(lambda (loc)
                           (setf (aref num-vec loc)
                                 (parse-integer (read-line in-strm))))))
                  (4 (1 ,(lambda (loc)
                           (format out-strm "~a~%" (aref num-vec loc))))))))
    (labels
        ((digit (num exp)
           (cond ((= exp 0) (mod num 10))
                 (t (digit (floor num 10) (1- exp)))))
         (simulate (vpos)
           (multiple-value-bind (modes opcode) (floor (aref num-vec vpos) 100)
             (unless (= opcode 99)
               (destructuring-bind (num-args fn) (cadr (assoc opcode opinfo))
                 (apply fn
                        (loop for arg-i from 1 upto num-args
                              collect
                              (cond ((or (= arg-i num-args)
                                         (/= 0 (digit modes (1- arg-i))))
                                     (aref num-vec (+ vpos arg-i)))
                                    (t
                                     (aref num-vec (aref num-vec (+ vpos arg-i)))))))
                 (simulate (1+ (+ vpos num-args))))))))
      (simulate 0))))

(defun solve-day5 ()
  (let (numvec)
    (with-open-file (strm "~/Code/advent2019/i5.txt")
      (let ((numls (map 'list #'parse-integer
                        (split-sequence:split-sequence #\, (read-line strm)))))
        (setf numvec (make-array (length numls) :initial-contents numls)))
      (sim-on-numvec (make-string-input-stream "1") t numvec))))
