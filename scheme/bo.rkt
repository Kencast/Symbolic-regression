#lang swindle


(define imprimir
  (lambda (L outP)
    (begin (write L outP) (close-output-port outP))))

(imprimir '((2 1) 3 4) (open-output-file "scheme/salida.txt" #:exists 'truncate))




;(print (expt -3 (/ 43 115)))
