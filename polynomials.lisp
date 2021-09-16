;; Display the variation table for a polynomial
;; Given the α, the β and 𝑎.
(defun variations (alpha beta a)
  (flet ((get-arrow (a f)
                    (if (= a 0)
                      "→"
                      (if (< a 0)
                        (if f "↗" "↘")
                        (if f "↘" "↗")))))
    (format t "   𝑥   | -∞     ~a     +∞~@
            -------------------------~@
            Var. 𝑓 |   ~a   ~a   ~a~%" alpha (funcall #'get-arrow a T) beta (funcall #'get-arrow a NIL))))

;; Return the discriminant (Δ) given
;; the terms of a polynomial.
(defun get-delta (a b c)
  (- (* b b) (* 4 a c)))

;; Solve a quadratic equation.
(defun solve (a b delta)
  (flet ((check (v) (if (subtypep (type-of v) 'complex) NIL v)))
    (let ((left (/ (- (- 0 b) (sqrt delta)) (* 2 a)))
          (right (/ (+ (- 0 b) (sqrt delta)) (* 2 a))))
      `(,(funcall #'check left) . ,(funcall #'check right)))))

;; Return α and β given the terms of 
;; a polynomial.
(defun alphabeta (a b c)
  (flet ((f (x) (+ (* (* x x) a) (* x b) c)))
    (let ((alpha (/ (- 0 b) (* 2 a))))
      `(,alpha . ,(f alpha)))))

;; Display the canonic form of a quadratic
;; polynomial given the α, the β and 𝑎.
(defun canonicalize (a alpha beta)
  (format t "𝑓: 𝑥 → ~a(𝑥 - ~a)² + ~a~%" a alpha beta))

;; Run all the functions above
;; and display their results to
;; the standard output.
(defun all (a b c)
  (let* ((_alphabeta (alphabeta a b c))
         (alpha (car _alphabeta))
         (beta (cdr _alphabeta))
         (delta (get-delta a b c))
         (solved (solve a b delta)))
    (format t "α = ~a ; β = ~a~%" alpha beta)
    (canonicalize a alpha beta)
    (variations alpha beta a)
    (format t "Δ = ~a~%" delta)
    (format t "S = {~a, ~a}~%" (car solved) (cdr solved))))

