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
         ((> (abs (* (log (abs a)) b)) 100) 'oo)
         (#t (real-part (expt a b))))))

(define rando
  (lambda (I J)
    (+ (random (+ (- J I) 1)) I)))


(define operations '(+ - * div expo ln a b constant))
(define constant (- (rando 0 40) 20))
(define noOperations '(constant a b))
(define archivo (open-input-file "scheme/prueba.txt"))
(define prbMut 12)
(define cantGen 50)
(define cantIndividuos 60)
(define cantTorneo 5)

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
    (cond ((not (list? I)) I)
          ((= 1 (rando 0 3)) I)
          ((= o 0) (obtenerRama (cadr I) o))
          (else (obtenerRama (caddr I) o)))))

(define ponerRama
  (lambda (R I o F)
    (cond ((and (> F 0) (or (not (list? I)) (= (rando 0 1) 1))) R)
          ((= 0 o) (list (car I) (ponerRama R (cadr I) o 1) (caddr I)))
          (else (list (car I) (cadr I) (ponerRama R (caddr I) o 1))))))

(define mutar
  (lambda (I)
    (list (getOperation operations (rando 1 6)) (cadr I) (caddr I))))

(define mutar2
  (lambda (I)
    (cond ((not (list? I)) I)
          ((= 1 (rando 1 8)) (list (getOperation operations (rando 1 6)) (mutar2 (cadr I)) (mutar2 (caddr I))))
          (else (list (car I) (mutar2 (cadr I)) (mutar2 (caddr I)))))))

(define obtenerHijo
  (lambda (I o F)
    (cond ((not(list? I)) (getOperation noOperations (rando 2 3)))
          ((and (= o 0) (> F 0) (= 1 (rando 0 3))) (cadr I))
          ((and (= o 1) (> F 0) (= 1 (rando 0 3))) (caddr I))
          ((= 0 o) (obtenerHijo (cadr I) o 1))
          (#t(obtenerHijo (caddr I) o 1)))))

(define mutElimi
  (lambda (I o F)
    (cond ((and (> F 0) (or (not(list? I)) (= 1 (rando 0 3)))) (obtenerHijo I (rando 0 1) 0))
          ((= o 0) (list (car I) (mutElimi (cadr I) o 1) (caddr I)))
          (#t(list (car I) (cadr I) (mutElimi (caddr I) o 1))))))

(define mutacion
  (lambda (I res)
    (cond ((<= res (/ prbMut 2)) (mutar2 I))
          ((< res prbMut) (mutElimi I (rando 0 1) 0))
          (else I))))

(define cruzar
  (lambda (Pad1 Pad2)
    (fitness (mutacion (ponerRama (obtenerRama Pad2 (rando 0 1)) Pad1 (rando 0 1) 0) (rando 1 100)))))

(define elitismo
  (lambda(Ind M)
    (cond
      ((null? Ind) M)
      ((< (cadar Ind) (cadr M)) (elitismo (cdr Ind) (car Ind)))
      (else (elitismo (cdr Ind) M)))))

(define posiblesPadres
  (lambda (I n)
    (cond((= n 0) '())
         (#t(cons (getOperation I (rando 1 cantIndividuos)) (posiblesPadres I (- n 1)))))))

(define laEleccion
  (lambda (I n)
    (elitismo (posiblesPadres I n) (getOperation I n))))

(define nuevaGeneracion
  (lambda (I n)
    (cond((= n (- cantIndividuos 1)) '())
         (#t (cons (cruzar (car(laEleccion I cantTorneo)) (car(laEleccion I cantTorneo))) (nuevaGeneracion I (+ n 1)))))))

(define elegir
  (lambda (n)
    (cond((< n 15) 1)
         ((< n 30) 2)
         ((< n 45) 3)
         ((< n 57) 4)
         ((< n 62) 5)
         ((< n 67) 6)
         ((< n 80) 7)
         ((< n 93) 8)
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
         (#t (* (- z N) (- z N)))
         )))

(define fitnessKener
  (lambda (I P)
    (cond ((null? P) 0)
          (#t(+ (preEvaluar (caddar P) (evaluar I (caar P) (cadar P))) (fitnessKener I (cdr P)))))))

(define fitness
  (lambda (I)
    (cond
      ((not (revisar (hojas I) 0 0)) (list I +inf.0))
      (#t (list I (fitnessKener I Puntos))))))

(define primFitness
  (lambda (I)
    (cond((null? I)'())
         (#t (cons (fitness (car I)) (primFitness (cdr I)))))))

(define imprimir
  (lambda (L outP)
    (begin (write L outP) (close-output-port outP))))

(define evolucion
  (lambda (Ind Elit cont)
    (cond ((= cont cantGen) (imprimir (elitismo Ind Elit) (open-output-file "scheme/salida.txt" #:exists 'truncate)))
          ((= 0 (cadr Elit)) (imprimir Elit (open-output-file "scheme/salida.txt" #:exists 'truncate)))
          (#t (begin (print Elit) (newline) (evolucion (cons (fitness (mutacion (car Elit) (rando 1 100))) (nuevaGeneracion Ind 0)) (elitismo Ind Elit) (+ 1 cont))))
          )))

(define gen (primFitness(genCero cantIndividuos)))
(define iniciar (lambda () (evolucion gen (elitismo gen (car gen)) 0)))



;(print (ponerRama ramo ind (rando 0 1) 0))
(iniciar)



;/home/kencast/cpp/Symbolic-regression/scheme/prueba.txt