(defun solve-6 (fname)
  (let ((orbit-map (make-hash-table :test #'equal))
        (nbor-map (make-hash-table :test #'equal))
        youparent sanparent)
    (with-open-file (strm fname)
      (loop for rstr = (read-line strm nil)
            while (not (null rstr)) do
              (destructuring-bind (src o) (split-sequence:split-sequence
                                           #\) rstr)
                (when (equal o "YOU")
                  (setf youparent src))
                (when (equal o "SAN")
                  (setf sanparent src))
                (setf (gethash src orbit-map)
                      (cons o (gethash src orbit-map)))
                (setf (gethash src nbor-map)
                      (cons o (gethash src nbor-map)))
                (setf (gethash o nbor-map)
                      (cons src (gethash o nbor-map))))))
    (labels
        ((tree-traverse (node-name)
           "Returns multiple values: (nodes-in-subtree, num-dependents)"
           (let ((children (gethash node-name orbit-map)))
             (when (null children)
               (return-from tree-traverse (list 1 0)))
             (destructuring-bind
                 (subtree-sizes num-deps)
                 (apply #'mapcar #'list
                        (map 'list #'tree-traverse children))
               (return-from tree-traverse
                 (list
                  (1+ (reduce #'+ subtree-sizes))
                  (+ (reduce #'+ subtree-sizes)
                     (reduce #'+ num-deps))))))))
      (format t "Part (a): ~a~%" (second (tree-traverse "COM"))))

    (labels
        ((tree-traverse (curr-node-name target-node-name plen
                         visited-map)
           (when (equal curr-node-name target-node-name)
             (return-from tree-traverse plen))
           (setf (gethash curr-node-name visited-map) t)
           (some (lambda (nbor) (tree-traverse nbor target-node-name
                                               (1+ plen) visited-map))
                 (remove-if (lambda (n) (gethash n visited-map))
                            (gethash curr-node-name nbor-map)))))
      (format t "Part (b): ~a"
              (tree-traverse youparent sanparent 0
                             (make-hash-table :test #'equal))))))
