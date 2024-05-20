#lang swindle


(define imprimir
  (lambda (L outP)
    (begin (write L outP) (close-output-port outP))))




;(print (expt -3 (/ 43 115)))
