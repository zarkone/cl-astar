(defparameter *graph* (make-hash-table))

(defparameter *x-len* 0)
(defparameter *y-len* 0)

(defun hash-keys (hash-table)
  (loop for key being the hash-keys of hash-table collect key))

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

(defun create-key (y x)
  (intern (format nil "el~a-~a" y x)))

(defmacro select-v (y x)
  `(gethash (create-key ,y ,x) *graph*))


(defun open-graph (filename)
  (let ( (i 0) (j 0))
    (with-open-file (in filename)
      (do ((line (read-line in nil)
                 (read-line in nil)))
          ((null line))
        (setq j -1)
        (dolist (ch (split-by-one-space line))
          (incf j)
          (setf 
           (select-v i j)
           (char-to-vertex-hander ch i j)))
        (incf i)))
    (setf *x-len* (+ j 1))
    (setf *y-len* i))
  *graph*)

(defun print-graph nil 
  (dotimes (y *y-len*)
       (dotimes (x *x-len*)
            (format t "~a " 
                    (get-symbol (select-v y x))))
       (format t "~%")))
      
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

;; (reduce #'min-f-reduce-handler '((:f 2) (:f 1) (:f 11) (:f -2)))

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

(defun search-in-neighbours (neighbours target)
  (let ((n (car neighbours)))
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
         (t (if 
             (> (get-g *current*) 
                (get-g n)) 
             (progn 
               (count-all n target)
               (setf (get-parent n) *current*)))))
       (search-in-neighbours 
        (cdr neighbours) target))
      (t
       (setf 
        (get-parent n) 
        *current*)
       t))))

(defun a-star-inner-loop (target)
  (if (not (equal *open-list* nil))
      (progn 
        (setf *current* (get-current))
        (push *current* *closed-list*)
        (remove-from-open-list *current*)

        (if (not 
             (search-in-neighbours 
              (get-neighbours *current*) 
              target))
            (a-star-inner-loop target)
            t)))
  nil)

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
  (push src *open-list*)
  
  (a-star-inner-loop trg)
  (draw-path src trg)
  (setf (get-symbol src) "A")
  (setf (get-symbol trg) "B"))
;; (print "--------------------------------")

;; (setq *graph* nil)
(open-graph "generated.txt")
(count-all
 (gethash  (create-key 0 0) *graph*)
 (gethash  (create-key 5 10) *graph*))
(a-star (select-v 3 4) (select-v 20 20))
;; (car (cdr *graph*))
;; (print (reverse *graph*))
;; (print (reverse *graph*))
(print-graph)


;; (count-h (car  *graph*) (car (cdr *graph*)))

;; (print (hash-keys *graph*))
;; (print (gethash (create-key 2 2) *graph*))
(defun print-neighbours (neighbours)
  (print 
   (list
    (get-x (car neighbours))
    (get-y (car neighbours))))
  (if (not (eq neighbours nil)) (print-neighbours (cdr neighbours))))
;; (print (get-neighbours (select-v 3 4)))

;; (print (select-v 0 0))
;; (print (car (cdr *graph*)))
;; (count-g (car *graph*) )

;; (setf (getf (car  *graph*) :x) 222)
;; (car *graph*)


;; (defmacro --foo* ((bar baz)) 
;;   `(print ,baz))
;; (--foo* (1 2))

