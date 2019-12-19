(in-package #:advent2019)

(defun solve-day3 (input-fname)
  (labels ((l1-norm (pos) (+ (abs (car pos)) (abs (cdr pos))))
           (succ (dir pos)
             (let ((x (car pos))
                   (y (cdr pos)))
               (cons
                (cond ((equal dir "L") (1- x))
                      ((equal dir "R") (1+ x))
                      (t x))
                (cond ((equal dir "D") (1- y))
                      ((equal dir "U") (1+ y))
                      (t y)))))

           (path (startpos instr-ls set minsteps)
             (if (null instr-ls)
                 ;; then clause
                 set
                 ;; else clause
                 (let* ((ins (car instr-ls))
                        (dir (subseq ins 0 1))
                        (numsteps (parse-integer (subseq ins 1))))
                   (loop for u-u from 0 upto (1- numsteps) do
                     (setf startpos (succ dir startpos))
                     (incf minsteps)
                     (setf (gethash startpos set)
                           (or (gethash startpos set) minsteps)))
                   (path startpos (cdr instr-ls) set minsteps)))))

    (let (awire-ins bwire-ins)
      (with-open-file (strm input-fname)
        (setf awire-ins (split-sequence:split-sequence #\, (read-line strm)))
        (setf bwire-ins (split-sequence:split-sequence #\, (read-line strm))))
      (let* ((apset (path '(0 . 0) awire-ins
                          (make-hash-table :test #'equal)
                          0))
             (bpset (path '(0 . 0) bwire-ins
                          (make-hash-table :test #'equal)
                          0))
             (intersects (remove-if
                          #'null
                          (loop for aloc being the hash-keys of apset
                                collect
                                (if (gethash aloc bpset)
                                    (cons (l1-norm aloc)
                                          (+ (gethash aloc apset)
                                             (gethash aloc bpset))))))))
        (format t "Part A solution: ~a~%"
                (reduce (lambda (curr-min i-info) (min curr-min
                                                       (car i-info)))
                        intersects
                        :initial-value (caar intersects)))
        (format t "Part B solution: ~a"
                (reduce (lambda (curr-min i-info) (min curr-min
                                                       (cdr i-info)))
                        intersects
                        :initial-value (cdar intersects)))))))
