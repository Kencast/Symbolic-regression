#lang racket
(define ε 1e-19)
(define -ε -1e-19)
(define ∞ 1e19)
(define -∞ -1e+19)

(define (diff x y) (abs (- (nrm x) (nrm y))))
(define (nrm w) (magnitude w))
(define (negative? x) (> 0 (real-part x)))
(define (positive? x) (<= 0 (real-part x)))
(define (diff-sign? x y) (or (and (negative? x) (positive? y)) (and (negative? y) (positive? x))))

(define (inf? w) (>= (nrm w) ∞))
(define (eps? w) (<= (nrm w) ε))
(define (_inf w) (cond ((>= (nrm w) ∞) (cond ((positive? w) ∞) (#t -∞))) (#t w)))
(define (_eps w) (cond ((<= (nrm w) ε) (cond ((positive? w) ε) (#t -ε))) (#t w)))


(define (_+_ x y)
  (cond
    ((eps? x) (_inf y))
    ((eps? y) (_inf x))
    (#t (_inf (+ (_inf x) (_inf y))))
    )
  )

(define (_-_ x y)
  (cond
    ((eps? x) (_inf (* -1 y)))
    ((eps? y) (_inf x))
    (#t (_inf (- (_inf x) (_inf y))))
    )
  )

(define (_*_ x y)
  (cond
    ((or (eps? x) (eps? y)) 0)
    (#t (_inf (* (_inf x) (_inf y))))
    )
  )

(define (_/_ x y)
  (cond
    ((eps? y) (_inf (* x ∞)))
    ((eps? x) 0)
    (#t (_inf (/ (_inf x) (_inf y))))
    )
  )


(define (_^_ x y)
  (cond
    ((and (eps? x) (negative? y)) ∞)
    ((eps? x) 0)
    ((or (eps? y) (<= (diff x 1) 1e-3)) 1)
    ((or (inf? x) (>= (nrm y) (nrm (/ 45 (log (+ x ε)))))) ∞)
    (#t (expt x y))
    )
  )


(define (_lg_ x y)
  (cond
    ((eps? x) 0)
    ((eps? y) -∞)
    ((<= (diff 1 x) (* ε 5)) ∞)
    (#t (log y x))
    )
  )


(define (choose-consvar)
  (cond
    ((and (nextBool) (nextBool) (nextBool)) (- (random 41) 20))
    (else (cond ((nextBool) 'x) (#t 'y)))
    )
  )


(define (choose-function)
  (define (choose-function-h r)
    (cond
      ((= 0 r) '_+_)
      ((= 1 r) '_-_)
      ((= 2 r) '_*_)
      ((= 3 r) '_/_)
      ((= 4 r) '_^_)
      (else '_lg_)
      )
    )(choose-function-h (random 6))
  )

