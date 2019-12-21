(in-package #:advent2019)
(declaim (optimize (debug 2)))

(defun simulate (in-strm out-strm num-vec)
  (let ((iptr 0)
        (amodes 0)
        ret)
    (labels ((get-immediate (idx) (aref num-vec idx))
             (get-position (idx) (aref num-vec (aref num-vec idx)))
             (get-arg (apos)
               (let ((amode (volt:digit amodes (1- apos))))
                 (case amode
                   (0 (get-position (+ iptr apos)))
                   (1 (get-immediate (+ iptr apos))))))

             (mach-add ()
               (let ((n1 (get-arg 1))
                     (n2 (get-arg 2))
                     (loc (get-immediate (+ iptr 3))))
                 (setf (aref num-vec loc) (+ n1 n2)))
               (incf iptr 4))

             (mach-mult ()
               (let ((n1 (get-arg 1))
                     (n2 (get-arg 2))
                     (loc (get-immediate (+ iptr 3))))
                 (setf (aref num-vec loc) (* n1 n2)))
               (incf iptr 4))

             (mach-read-into-loc ()
               (let ((loc (get-immediate (+ iptr 1))))
                 (setf (aref num-vec loc)
                       (parse-integer (read-line in-strm))))
               (incf iptr 2))

             (mach-output ()
               (let ((arg (get-arg 1)))
                 (format out-strm "~a~%" arg)
                 (setf ret arg))
               (incf iptr 2))

             (mach-jump-on-pred (predicate)
               (lambda ()
                 (let ((n (get-arg 1))
                       (loc (get-arg 2)))
                   (cond
                     ((funcall predicate n) (setf iptr loc))
                     (t (incf iptr 3))))))

             (mach-lt ()
               (let ((n1 (get-arg 1))
                     (n2 (get-arg 2))
                     (loc (get-immediate (+ iptr 3))))
                 (setf (aref num-vec loc) (if (< n1 n2) 1 0)))
               (incf iptr 4))

             (mach-eql ()
               (let ((n1 (get-arg 1))
                     (n2 (get-arg 2))
                     (loc (get-immediate (+ iptr 3))))
                 (setf (aref num-vec loc) (if (= n1 n2) 1 0)))
               (incf iptr 4)))

      (loop
        with opcode = -1
        while (/= opcode 99) do
          (multiple-value-setq (amodes opcode)
            (floor (aref num-vec iptr) 100))
          (funcall (ecase opcode
                     (1 #'mach-add)
                     (2 #'mach-mult)
                     (3 #'mach-read-into-loc)
                     (4 #'mach-output)
                     (5 (mach-jump-on-pred (lambda (x) (/= 0 x))))
                     (6 (mach-jump-on-pred (lambda (x) (zerop x))))
                     (7 #'mach-lt)
                     (8 #'mach-eql)
                     (99 (lambda ()))))))
    ret))
