#lang swindle

(define ln
  (lambda (a b)
    (cond
      ((or (= a 1) (<= a 0) (<= b 0)) 'oo)
      (else (/ (log b) (log a))))))

(define div
  (lambda (a b)
    (cond ((= b 0) 'oo)
          (else (/ a b)))))

(define expo
  (lambda (a b)
    (cond((and (= 0 a) (<= b 0)) 'oo)
         ((and (< a 0) (even? (denominator b))) 'oo)
         ((= a 0) 0)
         ((> (abs (* (log (abs a)) b)) 500) 'oo)
         (#t (expt a b)))))

(define rando
  (lambda (I J)
    (+ (random (+ (- J I) 1)) I)))

(define operations '(+ - * div expo ln a b constant))
(define constant (- (rando 0 40) 20))
(define noOperations '(constant a b))
(define archivo (open-input-file "scheme/prueba.txt"))
(define prbMut 5)
(define cantGen 50)
(define cantIndividuos 20)

(define separarNumeros
  (lambda (S G)
    (cond((equal? ")" S) (list (string->number G)))
         ((equal? (string-ref S 0) #\space) (cons (string->number G) (separarNumeros (substring S 1) "")))
         (#t(separarNumeros (substring S 1) (string-append G (string (string-ref S 0))))))))

(define leer
  (lambda (linea)
    (cond ((eof-object? linea) '())
          (else (cons (separarNumeros (substring linea 1) "") (leer (read-line archivo)))))))

(define Puntos (leer (read-line archivo)))

(close-input-port archivo)
(define getOperation
  (lambda (L N)
    (cond ((= N 1) (car L))
          (else (getOperation (cdr L) (- N 1))))))

(define obtenerRama
  (lambda (I o)
    (cond((= 0 o) (cadr I))
         (#t(caddr I)))))

(define ponerRama
  (lambda (R I o)
    (cond((= 0 o) (list (car I) R (caddr I)))
         (#t(list (car I) (cadr I) R)))))

(define cruzar
  (lambda (Pad1 Pad2)
    (fitness (ponerRama (obtenerRama Pad2 (rando 0 1)) Pad1 (rando 0 1)))))

(define elegirPadres
  (lambda (I i j)
    (cond(< (cadr (getOperation I i)) (cadr (getOperation I j)) (car (getOperation I i)))
         (#t(car (getOperation I j))))))

(define nuevaGeneracion
  (lambda (I n)
    (cond((= n cantIndividuos) '())
         (#t(cons (cruzar (elegirPadres I (rando 1 (/ cantIndividuos 2)) (rando 1 (/ cantIndividuos 2)))
                          (elegirPadres I (rando (/ cantIndividuos 2) cantIndividuos) (rando (/ cantIndividuos 2) cantIndividuos))
                          (nuevaGeneracion I (+ n 1))))))))

(define elegir
  (lambda (n)
    (cond((< n 15) 1)
         ((< n 30) 2)
         ((< n 45) 3)
         ((< n 57) 4)
         ((< n 62) 5)
         ((< n 67) 6)
         ((< n 79) 7)
         ((< n 91) 8)
         (#t 9))))

(define generarIndividuo
  (lambda (P h)
    (cond
      ((equal? P 'constant) (- (rando 0 40) 20))
      ((list? (member P noOperations)) P)
      ((= h 5)(getOperation noOperations (rando 2 3)))
      (#t(list P (generarIndividuo (getOperation operations (elegir(rando 0 100))) (+ 1 h)) (generarIndividuo (getOperation operations (elegir(rando 0 100))) (+ 1 h)))))))

(define genCero
  (lambda (n)
    (cond ((= n 0) '())
          (#t(cons (generarIndividuo (getOperation operations (elegir (rando 0 66))) 0) (genCero (- n 1)))))))

(define hojas
  (lambda (A)
    (cond ((not (list? A)) (list A))
          (#t (append (hojas(cadr A)) (hojas(caddr A)))))))

(define revisar
  (lambda (L f t)
    (cond ((and (null? L) (or (= f 0) (= t 0)))#f)
          ((and (> f 0) (> t 0)) #t)
          ((equal? (car L) 'a) (revisar (cdr L) (+ f 1) t))
          ((equal? (car L) 'b) (revisar (cdr L) f (+ t 1)))
          (#t(revisar (cdr L) f t)))))

(define revisarCalculo
  (lambda (O I D)
    (cond((or (equal? 'oo I) (equal? 'oo D)) 'oo)
         ((or (not (real? I)) (not (real? D))) 'oo)
         ((or (= (abs I) +inf.0) (= (abs D) +inf.0)) 'oo)
         ((not (real? (eval (list O I D)))) (begin (print (list O I D (eval (list O I D)))) (newline)(eval (list O I D))))
         (#t (eval (list O I D))))))

(define evaluar
  (lambda (A x y)
    (cond
      ((and (not (list? A)) (equal? A 'a)) x)
      ((and (not (list? A)) (equal? A 'b)) y)
      ((not (list? A)) A)
      (else (revisarCalculo (car A) (evaluar (cadr A) x y) (evaluar (caddr A) x y))))))

(define preEvaluar
  (lambda (z N)
    (cond((equal? N 'oo) +inf.0)
         (#t (* (- z N) (- z N))))))

(define fitnessKener
  (lambda (I P)
    (cond ((null? P) 0)
          (#t(+ (preEvaluar (caddar P) (evaluar I (caar P) (cadar P))) (fitnessKener I (cdr P)))))))

(define fitness
  (lambda (I)
    (list I (fitnessKener I Puntos))))

(define primFitness
  (lambda (I)
    (cond((null? I)'())
         ((not(revisar (hojas (car I)) 0 0)) (cons (list (car I) +inf.0) (primFitness (cdr I))))
         (#t (cons (fitness (car I)) (primFitness (cdr I)))))))

(define evolucion
  (lambda (Ind Elit cont) (nuevaGeneracion Ind 0)))

(define iniciar (lambda (n) (evolucion (primFitness (genCero n)) '(+ a b))))


(define prueba
  (lambda(Ind M)
    (cond
      ((null? Ind) M)
      ((< (cadar Ind) (cadr M)) (prueba (cdr Ind) (car Ind)))
      (else (prueba (cdr Ind) M)))
    ))

(define analizar
  (lambda(Ind)
    (cond
      ((null? Ind) '())
      (else (cons (cadar Ind) (analizar (cdr Ind)))))
    ))


(define prueba1 (lambda (I) (cruzar (elegirPadres I (rando 1 (/ cantIndividuos 2)) (rando 1 (/ cantIndividuos 2)))
                                    (elegirPadres I (rando (/ cantIndividuos 2) cantIndividuos) (rando (/ cantIndividuos 2) cantIndividuos)))))

;(print (prueba (primFitness (genCero 10)) '(() +inf.0)))
(print (prueba (primFitness (genCero 20)) '(() +inf.0)))
;(print (genCero 20))
;(print (nuevaGeneracion (primFitness (genCero 50)) 0))

;/home/kencast/cpp/Symbolic-regression/scheme/prueba.txt
