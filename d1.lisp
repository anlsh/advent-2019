(in-package #:advent2019)

(defun get-fuel (mass-ls)
  (labels ((fuel-for-mass (m)
             (let ((req-fuel (- (floor (/ m 3)) 2)))
               (if (>= req-fuel 0)
                   (+ req-fuel (fuel-for-mass req-fuel))
                   0))))
    (reduce #'+ (map 'list
                     #'fuel-for-mass
                     mass-ls))))

(defun solve ()
  (with-open-file (stream "inputs/i1.txt")
    (get-fuel (loop for l = (read-line stream nil)
                    while l collect (parse-integer l)))))
