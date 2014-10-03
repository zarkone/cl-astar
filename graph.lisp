(defvar *graph* nil)

(defun open-graph (filename)
  (with-open-file (in filename)
    (do ((line (read-line in nil)
               (read-line in nil)))
        ((null line))
      (push line *graph*))) *graph*)

(open-graph "generated.txt")
(car (cdr *graph*))
