(defpackage :graphs (:use "CL" "SB-THREAD" "SB-EXT"))
(in-package :graphs)

(defparameter *graph* nil)

(defparameter *x-len* 0)
(defparameter *y-len* 0)
(defparameter *barrier* 0)

(defmacro for (var start stop &body body)
  (let ((gstop (gensym)))
    `(do ((,var ,start (1+ ,var))
          (,gstop ,stop))
         ((> ,var ,gstop))
       ,@body)))
(defmacro while (test &rest body)
  `(do ()
       ((not ,test))
     ,@body))

(defun split-by-one-space (string)
  "Returns a list of substrings of string
divided by ONE space each.
Note: Two consecutive spaces will be seen as
if there were an empty string between them."
    (loop for i = 0 then (1+ j)
          as j = (position #\Space string :start i)
          collect (subseq string i j)
          while j))

(defun char-to-vertex-hander (ch y x)
  "Char to Vertex map function."
  (list :x x :y y :is-passable (is-passable-p ch)
       :symbol ch :parent nil 
       :F 0 :G 0 :H 0))
(defun is-passable-p (ch) 
  (equal ch "*"))

(defmacro get-param (v key) 
  `(getf ,v ,key))

(defmacro get-x (v) 
  `(get-param ,v :x))
(defmacro get-y (v) 
  `(get-param ,v :y))
(defmacro get-is-passable (v) 
  `(get-param ,v :is-passable))
(defmacro get-symbol (v) 
  `(get-param ,v :symbol))
(defmacro get-parent (v) 
  `(get-param ,v :parent))
(defmacro get-h (v) 
  `(get-param ,v :h))
(defmacro get-g (v) 
  `(get-param ,v :g))
(defmacro get-f (v) 
  `(get-param ,v :f))

(defun open-graph (filename)
  (let ( (i 0) (j 0))
    (with-open-file (in filename)
      (do ((line (read-line in nil)
                 (read-line in nil)))
          ((null line))
        (setq j -1)
        (dolist (ch (split-by-one-space line))
          (incf j)
          (push
           (char-to-vertex-hander ch i j)
           *graph*))
        (incf i)))
    (setf *x-len* (+ j 1))
    (setf *y-len* i))
  *graph*)

;; (defun select-v (y x &optional (graph *graph*))
;;   (cond
;;     ((not graph) nil)
;;     ((and
;;       (= x (get-x (car graph)))
;;       (= y (get-y (car graph)))) (car graph))
;;     (t (select-v y x (cdr graph)))))
(defun select-v (y x)
  (if (or (> y *y-len*) (> x *x-len*)) nil
      (let ((i (* *x-len* (- *y-len* y 1))) (j (- (- *x-len* 1)  x)))
        (nth (+ i j) *graph*))))

(defun print-graph nil 
  (dotimes (y *y-len*)
       (dotimes (x *x-len*)
            (format t "~a " 
                    (get-symbol (select-v y x))))
       (format t "~%")))


(defun count-h (vertex target)
  (setf
   (get-h vertex)
   (* 10 
      (+ (abs 
          (- (get-x vertex) (get-x target)))
         (abs 
          (- (get-y vertex) (get-y target)))))))

(defun is-diagonal-p (vertex)
  (not (or
        (eq (get-x vertex) (getf (get-parent vertex ) :x))
        (eq (get-y vertex) (getf (get-parent vertex ) :y)))))

(defun count-g (vertex)
  (if (get-parent vertex) 
      (setf (get-g vertex) 
            (+ 
             (getf (get-parent vertex) :g)
             (if (is-diagonal-p vertex) 14 10)))
      (setf (get-g vertex) 0))
  (get-g vertex))

(defun count-f (vertex) 
  (setf (get-f vertex) 
        (+ (get-g vertex) 
           (get-h vertex))))

(defun count-all (vertex target) 
  (count-h vertex target)
  (count-g vertex)
  (count-f vertex))

(defmacro push-neighbour (x y) 
  `(push (select-v 
          (- (get-y vertex) ,y) 
          (- (get-x vertex) ,x)) neighbours))

(defun get-neighbours (vertex)
  (let ((neighbours nil))

    (push-neighbour 1 1)
    (push-neighbour 0 1)
    (push-neighbour -1 1)
    (push-neighbour 1 0)
    (push-neighbour -1 0)
    (push-neighbour 1 -1)
    (push-neighbour 0 -1)
    (push-neighbour -1 -1)
    neighbours))

(defun curry (fn &rest args)
  #'(lambda (&rest rest-args)
      (apply fn (append args rest-args))))

(defun min-reduce-handler (x y prop) 
  (if (< (getf x prop) (getf y prop)) x y))

(defun min-f-reduce-handler (x y)
  (min-reduce-handler x y :f))

(defun in-list-p (x lst) 
  (cond ((equal lst nil) nil)
        ((equal (car lst) x) T)
        (t (in-list-p x (cdr lst)))))

(defparameter *open-list* nil)
(defparameter *closed-list* nil)
(defparameter *target-found* nil)

(defun get-current nil 
  (if (equal *open-list* nil) nil
  (reduce #'min-f-reduce-handler *open-list*)))

(defun remove-from-open-list (x)
  (setf 
   *open-list* 
   (remove-if #'(lambda (a) (equal x a)) *open-list*)))

(defparameter *current* nil)

(defun search-in-neighbours (n target)
  (cond 
    ((equal n nil) nil)
    ((not (equal n target))
     (cond 
       ((or 
         (in-list-p n *closed-list*) 
         (not (get-is-passable n)))
        nil)
       ((not 
         (in-list-p n *open-list*))
        (setf (get-parent n) *current*)
        (count-all n target)
        (push n *open-list*))
       (t (when (> (get-g *current*) (get-g n)) 
            (count-all n target)
            (setf (get-parent n) *current*)))) nil)
    (t (setf (get-parent n) *current*)
     t)))

(defparameter *target-found* nil)
(defparameter *target* nil)
(defparameter *begin-cell-search* (make-waitqueue))
(defparameter *cells-searched* (make-waitqueue))
(defparameter *lock* (make-mutex :name "lock"))

(defparameter *neighbours* nil)
(defparameter *io-main* *standard-output*)

(defun parallel-search nil
  #'(lambda ()
      (with-mutex (*lock*)
        (loop (condition-wait *begin-cell-search* *lock*)
           (when *target-found* (return-from-thread nil))
           (for x 1 4
             (when (search-in-neighbours (pop *neighbours*) *target*)
               (setf *target-found* t)
               (condition-notify *cells-searched*)
               (return-from-thread nil))
             (when (>= (incf *barrier* 8))
               (condition-notify *cells-searched*)))))))

(defun a-star-inner-loop nil
  (when *open-list*
    (setf *current* (get-current))
    (push *current* *closed-list*)
    (remove-from-open-list *current*)
    (setf *barrier* 0)
    (setf *target-found* nil)

    (with-mutex (*lock*)
      (setf *neighbours* (get-neighbours *current*))
      (condition-broadcast *begin-cell-search*)
      (when (condition-wait *cells-searched* *lock*) nil))


    (if (not *target-found*)
        (a-star-inner-loop)
        t)))

(defun draw-path (src trg)
  (if (or 
       (equal trg nil)
       (equal trg src))
      nil
      (progn
        (setf (get-symbol trg) " ")
        (draw-path src (get-parent trg)))))

(defun a-star (src trg)
  (count-all src trg)
  (setf *target* trg)
  (push src *open-list*)
  (for x 1 2
    (make-thread
     (parallel-search)))
  (a-star-inner-loop)
  (draw-path src trg)
  (print *target-found*)
  (print (length *open-list*))
  (setf (get-symbol src) "A")
  (setf (get-symbol trg) "B") 
  *target-found*)

;; -------------------------------------

(open-graph "tests/768.txt")
;; (print "haha.")
(time
 (a-star (select-v 63 4) (select-v 686 525)))


(list (get-y *current*)(get-x *current*))
