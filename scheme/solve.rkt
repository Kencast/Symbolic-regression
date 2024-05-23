#lang racket
(require plot)
(require racket/gui)
(require racket/future)

(define frame (new frame% [label "Symbolic Regresion"] [width 650] [height 650]))
(define canva (new canvas% [parent frame]))
(define epsilon 0.0000001)

(define ln
  (lambda (a b)
    (cond
      ((or (= a 1) (<= a 0) (<= b 0)) 'oo)
      (else (exact->inexact (/ (log b) (log a)))))))

(define div
  (lambda (a b)
    (cond ((= b 0) 'oo)
          ((<= (abs (- 1 b)) epsilon) (cond ((and (positive? a) (positive? b)) a) (else (- a))))
          ((<= (abs a) epsilon) 0)
          (else (exact->inexact (/ a b)))
          )))

(define expo
  (lambda (a b)
    (cond((and (= 0 a) (<= b 0)) 'oo)
         ((and (< a 0) (even? (denominator b))) 'oo)
         ((= a 0) 0)
         ((> (abs (* (log (abs a)) b)) 70) 'oo)
         (#t (exact->inexact (real-part (expt a b))))
         )))

(define mult
  (lambda (I D)
    (cond
      ((or (= I 0) (= D 0)) 0)
      ((> (+ (abs (log (abs I))) (abs (log (abs D)))) 80) 'oo)
      (else (* I D))
      )))

(define rando
  (lambda (I J)
    (+ (random (+ (- J I) 1)) I)))

(define operations '(+ - * div expo ln a b constant))
(define constant (- (rando 0 40) 20))
(define noOperations '(constant a b))
(define archivo (open-input-file "scheme/f1.txt"))
(define prbMut 30)
(define cantGen 2000)
(define cantIndividuos 101)
(define mitadIndividos (quotient cantIndividuos 2))
(define cantTorneo 5)
(define cantMigrar 10)
(define mitadMigrar (/ cantMigrar 2))
(define migrationTime 10)
(define cantPob 4)
;(send frame show #t)

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
(define Vuntos (map list->vector Puntos))
(define getOperation
  (lambda (L N)
    (cond ((= N 1) (car L))
          (else (getOperation (cdr L) (- N 1))))))

(define generarIndividuo
  (lambda (P h n)
    (cond
      ((equal? P 'constant) (- (rando 0 40) 20))
      ((list? (member P noOperations)) P)
      ((= h n)(getOperation noOperations (rando 2 3)))
      (#t(list P (generarIndividuo (getOperation operations (elegir(rando 0 100))) (+ 1 h) n) (generarIndividuo (getOperation operations (elegir(rando 0 100))) (+ 1 h) n))))))


(define obtenerRama
  (lambda (I o)
    (cond ((not (list? I)) I)
          ((= 1 (rando 0 2)) I)
          ((= o 0) (obtenerRama (cadr I) o))
          (else (obtenerRama (caddr I) o)))))


(define ponerRama
  (lambda (R I o F)
    (cond ((and (> F 0) (or (not (list? I)) (= (rando 0 2) 1))) R)
          ((= 0 o) (list (car I) (ponerRama R (cadr I) o 1) (caddr I)))
          (else (list (car I) (cadr I) (ponerRama R (caddr I) o 1))))))

(define nuevaRama
  (lambda (n)
    (generarIndividuo (getOperation operations (elegir (rando 0 66))) 0 n)))

(define mutAgrande
  (lambda (I F)
    (cond ((or (not (list? I)) (= 0 (rando 0 1))) I)
          ((and (> F 0) (= (rando 0 2) 1)) (nuevaRama 6))
          (else (list (car I) (mutAgrande (cadr I) 1) (mutAgrande (caddr I) 1))))))

(define mutar2
  (lambda (I)
    (cond ((not (list? I)) I)
          ((= 1 (rando 0 7)) (list (getOperation operations (rando 1 6)) (mutar2 (cadr I)) (mutar2 (caddr I))))
          (else (list (car I) (mutar2 (cadr I)) (mutar2 (caddr I)))))))

(define obtenerHijo
  (lambda (I o F)
    (cond ((not(list? I)) (getOperation noOperations (rando 2 3)))
          ((and (= o 0) (> F 0) (= 1 (rando 0 3))) (cadr I))
          ((and (= o 1) (> F 0) (= 1 (rando 0 3))) (caddr I))
          ((= 0 o) (obtenerHijo (cadr I) o 1))
          (#t(obtenerHijo (caddr I) o 1)))))

(define mutElimi
  (lambda (I F)
    (cond ((and (> F 0) (or (not(list? I)) (= 1 (rando 0 1)))) (obtenerHijo I (rando 0 1) 0))
          (#t (list (car I) (mutElimi (cadr I) 1) (mutElimi (caddr I) 1))))))

(define cantNodos
  (lambda (I)
    (cond ((not (list? I)) 1)
          (else (+ 1 (cantNodos (cadr I)) (cantNodos (caddr I)))))
    ))

(define mutacion
  (lambda (I res)
    (cond ((<= res (/ prbMut 2)) (mutar2 I))
          ((< res prbMut) (mutAgrande I 0))
          ((> (cantNodos I) 150) (mutElimi I 0))
          (else I))))

(define cruzar
  (lambda (Pad1 Pad2)
    (list (fitness (mutacion (ponerRama (obtenerRama Pad2 (rando 0 1)) Pad1 (rando 0 1) 0) (rando 0 100))) (fitness (mutacion (ponerRama (obtenerRama Pad1 (rando 0 1)) Pad2 (rando 0 1) 0) (rando 0 100))))))

(define elitismo
  (lambda(Ind M)
    (cond
      ((null? Ind) M)
      ((< (cadar Ind) (cadr M)) (elitismo (cdr Ind) (car Ind)))
      (else (elitismo (cdr Ind) M)))))

(define posiblesPadres
  (lambda (I n)
    (cond((= n 0) '())
         (#t (cons (getOperation I (rando 1 cantIndividuos)) (posiblesPadres I (- n 1)))))))

(define laEleccion
  (lambda (I n)
    (elitismo (posiblesPadres I n) (getOperation I n))))

(define nuevaGeneracion
  (lambda (I n)
    (cond((= n mitadIndividos) '())
         (#t (append (cruzar (car(laEleccion I cantTorneo)) (car(laEleccion I cantTorneo))) (nuevaGeneracion I (+ n 1)))))))

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

; (define elegir
;   (lambda (n)
;     (cond ((< n 67) (rando 1 6))
;           (else (rando 1 9))
;           )))

(define genCero
  (lambda (n)
    (cond ((= n 0) '())
          (#t(cons (generarIndividuo (getOperation operations (elegir (rando 0 66))) 0 5) (genCero (- n 1)))))))

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

(define eval2
  (lambda (O I D)
    (cond ((equal? O '+) (+ I D))
          ((equal? O '-) (- I D))
          ((equal? O '*) (mult I D))
          ((equal? O 'div) (div I D))
          ((equal? O 'expo) (expo I D))
          ((equal? O 'ln) (ln I D))
          )))

(define revisarCalculo
  (lambda (O I D)
    (cond((or (equal? 'oo I) (equal? 'oo D)) 'oo)
         ((or (not (real? I)) (not (real? D))) 'oo)
         ((or (= (abs I) +inf.0) (= (abs D) +inf.0)) 'oo)
         (#t (eval2 O I D))
         )))

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

(define fitnessAntiCuad
  (lambda (I P)
    (cond ((null? P) 0)
          (else (+ (abs (- (caddar P) (evaluar I (caar P) (cadar P)))) (fitnessAntiCuad I (cdr P)))))))

(define fitnessPrime
  (lambda (I)
    (list I (fitnessAntiCuad I Puntos))))

(define fitness
  (lambda (I)
    (cond((not (revisar (hojas I) 0 0)) (list I +inf.0))
         (#t (list I (fitnessKener I Puntos))))))

(define primFitness
  (lambda (I)
    (cond((null? I)'())
         (#t (cons (fitness (car I)) (primFitness (cdr I)))))))

(define imprimir
  (lambda (L outP)
    (begin (write L outP) (close-output-port outP))))

(define altura
  (lambda (I)
    (cond ((not (list? I)) 0)
          (else (+ 1 (max (altura (cadr I)) (altura (caddr I)))))
          )))

(define conv
  (lambda (n)
    (cond ((= +inf.0 n) n)
          (else (real->decimal-string n))
          )))

(define analisis
  (lambda (LA out)
    (cond ((null? LA) (close-output-port out))
          (else (begin (displayln (list (conv (cadar LA)) (cantNodos (caar LA))) out) (analisis (cdr LA) out))))))



(define graficar
  (λ (I s a)
    (plot3d/dc (list (points3d Vuntos #:color 'red #:sym 'fullcircle1)
                     (surface3d (λ (a b) (evaluar I a b)) #:color 2))
               (send canva get-dc)
               0 0
               (- (send frame get-width) 50)
               (- (send frame get-height) 50)
               #:title s
               #:x-label "EJE X"
               #:y-label "EJE Y"
               #:z-label "EJE Z"
               #:altitude 10
               #:angle a
               )))


(define futuros
  (lambda (P)
    (map (lambda (x) (future (lambda () (nuevaGeneracion x 0)))) P)))

(define elitismoMundial
  (lambda (P e)
    (cond ((null? P) e)
          (#t (elitismo (car P) (elitismoMundial (cdr P) e))))))

(define nuevasGen
  (lambda (F Pe)
    (list (elitismoMundial (cadr Pe) (car Pe)) (map touch F))))

(define meterElite
  (lambda (e P)
    (cond ((null? P) P)
          (#t(cons (cons (fitness (mutacion (car e) (rando 0 100))) (car P)) (meterElite e (cdr P)))))))

(define sacarInd
  (lambda (P n F S)
    (cond ((= n cantMigrar) (list F S P))
          ((< n mitadMigrar) (sacarInd (cdr P) (+ n 1) (cons (car P) F) S))
          (#t(sacarInd (cdr P) (+ n 1) F (cons (car P) S))))))


(define procesoMigrar
  (lambda (P1 P2 P3)
    (list (append (cadr P2) (car P3) (caddr P1))
          (append (cadr P3) (car P1) (caddr P2))
          (append (cadr P1) (car P2) (caddr P3)))))

(define coyotes
  (lambda (GP)
    (append (procesoMigrar (sacarInd (car GP) 0 '() '()) (sacarInd (cadr GP) 0 '() '()) (sacarInd (caddr GP) 0 '() '()))  (cdddr GP))))

(define shift
  (lambda (L)
    (cond
      ((null? (cdr L)) '())
      (else (cons (car L) (shift (cdr L)))))))

(define migracion
  (lambda (GP g)
    (cond ((not (= 0 (remainder g migrationTime))) GP)
          (#t (coyotes (cons (car (drop GP (- cantPob 1))) (shift GP)))))))

(define end
  (lambda (E out) (begin
                    (displayln E)
                    (analisis (list E) out))))

(define exito
  (lambda (E out)
    (begin
      (displayln E)
      (write E out))
    ))

(define evolucionFuture
  (lambda (Pe g a)
    (displayln (cadar Pe))
    (cond ((= g cantGen) (end (fitnessPrime (car (elitismoMundial (cadr Pe) (car Pe)))) (open-output-file "scheme/an.txt" #:exists 'truncate)))
          ((= 0 (cadar Pe)) (exito (fitnessPrime (caar Pe)) (open-output-file "scheme/salida.txt" #:exists 'truncate)))
          (#t (evolucionFuture (nuevasGen (futuros (meterElite (car Pe) (migracion (cadr Pe) g))) Pe) (+ 1 g) a)))))

; (define evolucionFuture
;   (lambda (Pe g a)
;     (displayln (cadar Pe))
;     (cond ((= g cantGen) (begin (graficar (caar Pe) (string-append "Distancia: " (real->decimal-string (cadr (fitnessPrime (caar Pe))))) a) (end (fitnessPrime (car (elitismoMundial (cadr Pe) (car Pe)))) (open-output-file "scheme/an.txt" #:exists 'truncate))))
;           ((= 0 (cadar Pe)) (begin (graficar (caar Pe) (string-append "Distancia: " (number->string (cadar Pe))) a)
;                                    (exito (fitnessPrime (caar Pe)) (open-output-file "scheme/salida.txt" #:exists 'truncate))))
;           (#t (begin (graficar (caar Pe) (string-append "Gen: " (number->string g)) a)
;                      (sleep/yield 1)
;                      (evolucionFuture (nuevasGen (futuros (meterElite (car Pe) (migracion (cadr Pe) g))) Pe) (+ 1 g) (remainder (+ 45 a) 360)))))))

(define pob
  (lambda (n)
    (cond ((= n 0) '())
          (#t(cons (primFitness(genCero cantIndividuos)) (pob (- n 1)))))))

(define pobIni (pob cantPob))

(define iniciar (lambda () (evolucionFuture (list (caar pobIni) pobIni) 0 0)))

(iniciar)

;/home/kencast/cpp/Symbolic-regression/scheme/prueba.txt