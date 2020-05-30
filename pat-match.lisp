(defparameter *nodes*
'((runtime (162 169 148 132))
(title ("Avatar" "Spectre" "John Carter" "Lord"))
(vote_count(9106 2124 3576 1310))))

(defmacro runtime ()
` 'runtime)

(defmacro bigger ()
` "bigger")

(defmacro smaller ()
` "smaller")

(defmacro between ()
` "between")

(defvar x nil)
(defvar y nil)

(defun with-answer (x y z &optional n2)
(setq x x)
(let ((a (cadr(assoc x *nodes*))))
(when (stringp (car a))
(setf a (mapcar #'length a)
))
(with3 a (cadr(assoc 'title *nodes*)) y z n2)))

(defvar result '())

(defun with3 (lst b y z &optional n2 )
(cond ((null lst) '())
((and(or(eq '> y)(equal y "bigger")) (numberp z) (>(car lst) z))(format t "~a ~a" (car lst) (car b))(with3 (cdr lst) (cdr b) y z n2))
((and(or(eq '> y)(equal y "bigger")) (symbolp z) (mult x z n2)))
((and(or(eq '< y)(equal y "smaller")) (<(car lst) z))(format t "~a ~a" (car lst) (car b))(with3 (cdr lst) (cdr b) y z n2))
((and(equal "between" y) (and (>=(car lst) z) (<=(car lst)  n2)))(format t "~a ~a" (car lst) (car b))(with3 (cdr lst) (cdr b) y z n2))
(t (with3 (cdr lst) (cdr b) y z n2))))


(defun swt (n)
(let ((lst (list n)))
(setf (cdr lst) lst)))


(defvar *example* nil)
(defvar *titles* (cadr (assoc 'title *nodes*)))

(defun mult (x y z)
(setf *example* (mapcar #'* (cadr(assoc x *nodes*)) (swt z)))
(let ((a(mapcar #'> *example* (cadr(assoc y *nodes*)))))
(mapcan #'(lambda (x y) (if x (list y)))
a
*titles*)
))


(defun swt2 (&rest x)
(let ((result2 nil))
  (setq a x)
  (dotimes (n (length x))
    (if (stringp (caadr (assoc (car a) *nodes*))))
    (push (mapcar #'length (cadr (assoc (car a) *nodes* ))) result2)
    (if (numberp (caadr (assoc (car a) *nodes* )))(push (cadr (assoc (car a) *nodes*)) result2))
    (setf a (cdr a)))
  (sumswt result2))



(defun sumswt (x)
  (apply #'mapcar #'+ x))
