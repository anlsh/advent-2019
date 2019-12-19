(in-package #:advent2019)

(defun solve-day4 (&optional (numlen 6) (lbound 367479) (ubound 893698))
  ;; /u/oantolin has a super neat solution
  ;; https://topaz.github.io/paste/#XQAAAQAmAgAAAAAAAAAUGQimgx+p6N20AZ3Fh1ZlmBPmNJN1dOYPRMdougQgnb6bJ8fPrxNVrdHt+svDgqkKmdeLmfF7ZsC71EmypPmTARgCeOgO5LdU6xCpd1QnbFRa/O3uxYmXEJrY73kLnKabwlBjDrfXA/jyM0KYTddFygynstLRDNXZ1zp2QeJZ55sEIjfyN6fbOtqrjPp7EsWawhA4JH2v+zTVSj2aHABDuLWNZ4+I8+/vydUUTxg2tCnrykCrjqvq21d7i27HiI7MLiTScyt71XIuRmG+rDOEkDV0ETQxkmdnqjZkO7sGxV24mUGA5JKiZafPo6D8FVh2M+HoJaw64Wtrgc7MmdLU7x6yeHAAadH+JIrRSXeXyosDmlA4h6hkYjUHu//5VXiD
  ;; reddit comment here https://www.reddit.com/r/adventofcode/comments/e5u5fv/2019_day_4_solutions/f9mp192/
  (let ((count 0))
    (labels
        ((backtrack (n nlen lastdigit repeated rcount)
           (cond
             ((= nlen numlen)
              (if (and (or repeated (= rcount 2)) (<= lbound n ubound))
                  (return-from backtrack (incf count))
                  (return-from backtrack)))
             (t
              (loop
                with new-rcount = nil
                for digit from (max 1 lastdigit) upto 9 do
                  (setf new-rcount (if (= digit lastdigit) (1+ rcount) 1))
                  (backtrack (+ digit (* n 10))
                             (1+ nlen)
                             digit
                             (or repeated
                                 (and (= rcount 2) (= new-rcount 1)))
                             new-rcount))))))
      (backtrack 0 0 0 nil 0))
    count))
