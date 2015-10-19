(defvar *test-name* nil)
(defparameter *colorify* t)
(defmacro with-gensym ((&rest names) &body body)
  `(let ,(loop for n in names collect `(,n (gensym)))
     ,@body))

(defmacro combine (&body forms)
  (with-gensym (result)
    `(let ((,result t))
       ,@(loop for f in forms collect 
              `(unless ,f (setf ,result nil)))
       ,result)))

(defmacro color-print (is-green colorify string)
  `(cond 
    ((not ,colorify) (format t "~a~%",string))
    ( ,is-green (format t "~c[32m~a~c[0m~%" #\ESC ,string #\ESC))
    (t (format t "~c[31m~a~c[0m~%" #\ESC ,string #\ESC))))

(defun report-result (result form)
  (let ((r result))
    (color-print r *colorify*
                 (format nil "~:[FAIL~;pass~] ... ~a, ~a" 
             r *test-name* form))
    r))

(defmacro check (&body forms)
  `(combine
    ,@(loop for form in forms collect 
         `(report-result ,form ',form))))

(defmacro deftest (name &body body)
  `(defun ,name nil
     (let ((*test-name* (append *test-name* (list ',name))))
           ,@body)))

(setf *colorify* nil)

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

