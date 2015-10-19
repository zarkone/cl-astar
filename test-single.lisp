(defun a-star-single (test)
  (defparameter *graph* nil)
  (graph-all::open-graph (format nil "tests/~a.txt" test))
  (graph-all::a-star 
   (graph-all::select-v 1 1)
   (graph-all::select-v (- test 1) (- test 1))))

(deftest graph-10-single
  (check
    (a-star-single 10)))

(deftest graph-16-single
  (check
    (a-star-single 16)))

(deftest single
  (combine
    (graph-10-single)
    (graph-16-single)))

(single)
