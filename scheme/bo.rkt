#lang racket

(require racket/future)

; Función computacional que será evaluada en paralelo
(define (computacion-pesada x)
  (sleep 2) ; Simula una operación que toma tiempo
  (list (* x x)))

; Crear múltiples futures para evaluar computacion-pesada en paralelo
(define futuros
  (map (lambda (x) (future (lambda () (computacion-pesada x))))
       '(1 2 3)))

(displayln "Haciendo otras cosas mientras los futures se evalúan...")

(define resultados (apply append (map touch futuros)))

(displayln (format "Los resultados de los futures son: ~a" resultados))
