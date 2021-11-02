(defpackage #:polynomials
  (:use common-lisp)
  (:export :variations :canonicalize :extremum :solve :all))

(in-package #:polynomials)

(defun variations (alpha beta a)
  "Display the variation table for a polynomial given its α, β and 𝑎."
  (flet ((get-arrow (a f)
	   (if (= a 0)
	       "→"
	       (if (< a 0)
		   (if f "↗" "↘")
		   (if f "↘" "↗")))))
    (format t "   𝑥   | -∞     ~a     +∞~@
	    -------------------------~@
	    Var. 𝑓 |   ~a   ~a   ~a~%" alpha (get-arrow a T) beta (get-arrow a NIL))))

(defun discriminant (a b c)
  "Get the discriminant of a polynomial."
  (- (* b b) (* 4 a c)))

(defun solve (a b delta)
  "Solve a quadratic equation given its 𝑎, 𝑏 and Δ."
  (flet ((check (v) (if (subtypep (type-of v) 'complex) NIL v)))
    (let ((left (/ (- (- 0 b) (sqrt delta)) (* 2 a)))
	  (right (/ (+ (- 0 b) (sqrt delta)) (* 2 a))))
      `(,(check left) . ,(check right)))))

(defun extremum (a b c)
  "Return the coordinates of the extermum of a polynomial."
  (flet ((f (x) (+ (* (* x x) a) (* x b) c)))
    (let ((alpha (/ (- 0 b) (* 2 a))))
      (values alpha (f alpha)))))

(defun canonicalize (a alpha beta)
  "Display the canonic form of an equation given its 𝑎 and its extremum coordinates."
  (format t "𝑓: 𝑥 → ~a(𝑥 - ~a)² + ~a~%" a alpha beta))

(defun all (a b c)
  "Display the canonic form, variations tables and equation solutions of a polynomial."
  (multiple-value-bind (alpha beta) (extremum a b c)
    (let* ((delta (discriminant a b c))
	   (solved (solve a b delta)))
      (format t "α = ~a ; β = ~a~%" alpha beta)
      (canonicalize a alpha beta)
      (variations alpha beta a)
      (format t "Δ = ~a~%" delta)
      (format t "S = {~a, ~a}~%" (car solved) (cdr solved)))))
