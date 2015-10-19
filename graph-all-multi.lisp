(load "graph-all.fasl")

(defpackage :graph-all-multi
  (:use "CL" "SB-THREAD" "SB-EXT"))
(use-package :graph-all)
(in-package :graph-all-multi)

(defparameter *barrier* 0)
(defparameter *parallel-end* (make-waitqueue))
(defparameter *lock* (make-mutex :name "lock"))
(defparameter *test* 10)

(defun create-process (from to &optional (end to))
  #'(lambda nil
      (graph-all:process-graph from to end)
      (when (>= (incf *barrier*) 2) 
        (condition-notify *parallel-end*))
      (return-from-thread nil)))

(with-mutex (*lock*)
  (make-thread (create-process 1 (floor (/ *test* 2)) (- *test* 2)))
  (make-thread (create-process (floor (/ *test* 2)) (- *test* 2)))
  (print 
   (time 
    (when (condition-wait *parallel-end* *lock*)))))
