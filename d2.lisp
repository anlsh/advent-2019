(defun sim-with-nv (n v)
  (let* ((numls (map 'list #'parse-integer
                     (split-sequence:split-sequence #\, d2input)))
         (nv (make-array (length numls)
                         :initial-contents numls)))
    (setf (aref nv 1) n)
    (setf (aref nv 2) v)
    (labels ((simulate (vpos)
               (let ((curr (aref nv vpos)))
                 (cond ((= curr 1) (setf (aref nv (aref nv (+ 3 vpos)))
                                         (+ (aref nv (aref nv (+ 1 vpos)))
                                            (aref nv (aref nv (+ 2 vpos))))))
                       ((= curr 2) (setf (aref nv (aref nv (+ 3 vpos)))
                                         (* (aref nv (aref nv (+ 1 vpos)))
                                            (aref nv (aref nv (+ 2 vpos)))))))
                 (unless (= curr 99)
                   (simulate (+ vpos 4))))))
      (simulate 0))
    (aref nv 0)))

(defun solve-day2a () (sim-with-nv 12 2))

(defun solve-day2b ()
  (loop for n from 0 to 99 do
    (loop for v from 0 to 99 do
      (when (= (sim-with-nv n v) 19690720)
        (return-from solve-day2b (+ v (* 100 n)))))))

(defun solve-day2 ()
  (format t "Part (a): ~a" (sim-with-nv 12 2))
  (format t "Part (b): ~a" (solve-day2b)))
