(ql:quickload :cl-random)
(ql:quickload :alexandria)

(defun random-element (list)
  (let ((n (length list)))
    (unless (zerop n)
      (nth (random n) list))))

(defun neighbours (u G)
  (gethash u G))

(defun nodes (G)
  (alexandria:hash-table-keys G))

(defun print-hash-table (ht)
  (maphash (lambda (k v) (format t "~A: ~A~%" k v)) ht))

(defun topological-sort (G)
  (let ((in-degrees (make-hash-table :test (hash-table-test G))))

    (loop for u in (nodes G)
          do (loop for (v . w) in (neighbours u G)
                   if (gethash v in-degrees)
                   do (incf (gethash v in-degrees))
                   else
                   do (setf (gethash v in-degrees) 1))
          
          unless (gethash u in-degrees)
          do (setf (gethash u in-degrees) 0))
    
    (loop for u = (loop for u in (nodes G)
                        if (zerop (gethash u in-degrees))
                        return u)
          while u
          do (setf (gethash u in-degrees) -1)
          do (loop for (v . w) in (neighbours u G)
                   do (decf (gethash v in-degrees)))
          collect u)))

(defun edge-present-p (G u v &optional w)
  "If W is non-nil then return T if e=(u,v) is in G with w(e) = w. Otherwise return T if e is in G at all."
  ;; TODO: implement weighted edge testing
  (declare (ignorable w))
  (loop for (neighbour . weight) in (neighbours u G)
        if (funcall (hash-table-test G) neighbour v)
        return t))

(defun edge-weight (G u v)
  (loop for (neighbour . weight) in (neighbours u G)
        if (funcall (hash-table-test G) neighbour v)
        return weight))

(defun sample-path (dag)
  "Bernoulli factory for sampling a path from DAG such that each edge e is sampled with probability *exactly* p_e."
  (loop with path = (make-hash-table :test (hash-table-test dag))
        for u = (first (topological-sort dag)) then v
        until (null (neighbours u dag))
        for sum-of-outgoing-probs = (loop for (v . p) in (neighbours u dag) sum p)
        for v = (loop for (v . pe) = (random-element (neighbours u dag))
                      until (cl-random:draw-bernoulli (/ pe sum-of-outgoing-probs))
                      finally
                      (push (cons v pe) (gethash u path))
                      (return v))
        finally
        (setf (gethash u path) nil)
        (return path)))

(let ((dag (alexandria:alist-hash-table
             '((s . ((a . 3/4)
                     (b . 1/4)))
               (a . ((b . 1/4)
                     (c . 1/4)
                     (d . 1/4)))
               (b . ((d . 1/2)))
               (c . ((z . 1/2)))
               (d . ((c . 1/4)
                     (z . 1/2)))
               (z . ()))))
      (u 's)
      (v 'b))

  ;; test empirically if we sample some edge the right amount
  (format t "probability of sampling (~A,~A): ~D~%"
          u v (edge-weight dag u v))

  (loop with repetitions = 100
        repeat repetitions
        for sampled-path = (sample-path dag)
        count (edge-present-p sampled-path u v) into found
        finally (format t "empirical probability of sampling (~A,~A): ~D~%"
                        u v (/ found repetitions))))

