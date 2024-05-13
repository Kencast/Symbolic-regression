#lang swindle

(define rando
  (lambda (I J)
    (+ (random (+ (- J I) 1)) I)))

(define constant (- (rando 0 40) 20))


(define po(lambda (n) (print (+ n (- (rando 0 40) 20)))))

(define ant(lambda (t) (begin (print t) (newline) (print (- (rando 0 40) 20)))))



;(print (expt -3 (/ 43 115)))
