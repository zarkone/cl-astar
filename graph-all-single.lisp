(load "graph-all.fasl")
(use-package :graph-all)

(defparameter *test* 10)

(print 
 (time 
  (graph-all:process-graph 1  (- *test* 2)  (- *test* 2))))
