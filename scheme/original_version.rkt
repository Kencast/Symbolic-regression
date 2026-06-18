#lang racket

(require plot
         racket/gui
         racket/future)

;; -----------------------------------------------------------------------------
;; Symbolic regression with genetic programming
;; -----------------------------------------------------------------------------
;; How to use:
;;   1. Put the training points in INPUT-FILE using the same format as before.
;;      Example line: (1 2 3), where a = 1, b = 2, target = 3. a and b must be integers.
;;   2. Adjust the configuration values below if needed.
;;   3. Run: racket solve_refactored.rkt
;;
;; Output files:
;;   - answer.txt is written if a perfect solution is found.
;;   - found.txt is written when the maximum generation count is reached.
;; -----------------------------------------------------------------------------

;; -----------------------------------------------------------------------------
;; Configuration
;; -----------------------------------------------------------------------------

(define INPUT-FILE "f4.txt")
(define PERFECT-ANSWER-FILE "answer.txt")
(define BEST-FOUND-FILE "found.txt")

(define EPSILON 0.0000001)

(define MUTATION-PROBABILITY 15)
(define MAX-GENERATIONS 1000)
(define INDIVIDUALS-PER-POPULATION 101)
(define CHILD-PAIRS-PER-GENERATION (quotient INDIVIDUALS-PER-POPULATION 2))
(define TOURNAMENT-SIZE 5)

(define MIGRANTS-PER-MIGRATION 10)
(define MIGRANTS-PER-HALF (/ MIGRANTS-PER-MIGRATION 2))
(define MIGRATION-PERIOD 10)
(define POPULATION-COUNT 4)

(define INITIAL-TREE-DEPTH 5)
(define NEW-SUBTREE-DEPTH 6)
(define MAX-NODES-BEFORE-PRUNING 50)

;; GUI objects are only used by graph-expression. The evolution loop below keeps
;; plotting disabled, like the original active version of the program.
(define frame (new frame% [label "Symbolic Regression"] [width 650] [height 650]))
(define canvas (new canvas% [parent frame]))

;; -----------------------------------------------------------------------------
;; Expression representation
;; -----------------------------------------------------------------------------
;; A generated expression is either:
;;   - the variable symbol 'a
;;   - the variable symbol 'b
;;   - a numeric constant
;;   - a tree with the form: (operator left-expression right-expression)
;;
;; A scored individual has the form:
;;   (list expression fitness-value)

(define OPERATORS '(+ - * div expo ln))
(define TERMINALS '(constant a b))
(define ALL-GENES '(+ - * div expo ln a b constant))
(define INVALID-VALUE 'oo)

;; -----------------------------------------------------------------------------
;; Basic helpers
;; -----------------------------------------------------------------------------

(define (random-between min-value max-value)
  (+ (random (+ (- max-value min-value) 1)) min-value))

(define (list-ref-1-based values position)
  (cond [(= position 1) (car values)]
        [else (list-ref-1-based (cdr values) (- position 1))]))

(define (random-constant)
  (- (random-between 0 40) 20))

(define (invalid-result? value)
  (or (equal? value INVALID-VALUE)
      (not (real? value))
      (= (abs value) +inf.0)))

;; -----------------------------------------------------------------------------
;; Safe mathematical operations
;; -----------------------------------------------------------------------------
;; These functions preserve the original behavior: invalid calculations return
;; the symbol 'oo instead of raising an error.

(define (safe-log-base base value)
  (cond [(or (= base 1) (<= base 0) (<= value 0)) INVALID-VALUE]
        [else (exact->inexact (/ (log value) (log base)))]))

(define (safe-divide numerator denominator)
  (cond [(= denominator 0) INVALID-VALUE]
        [(<= (abs (- 1 denominator)) EPSILON)
         (cond [(and (positive? numerator) (positive? denominator)) numerator]
               [else (- numerator)])]
        [(<= (abs numerator) EPSILON) 0]
        [else (exact->inexact (/ numerator denominator))]))

(define (safe-expt base exponent)
  (cond [(and (= 0 base) (<= exponent 0)) INVALID-VALUE]
        [(and (< base 0) (even? (denominator exponent))) INVALID-VALUE]
        [(= base 0) 0]
        [(> (abs (* (log (abs base)) exponent)) 70) INVALID-VALUE]
        [else (exact->inexact (real-part (expt base exponent)))]))

(define (safe-multiply left right)
  (cond [(or (= left 0) (= right 0)) 0]
        [(> (+ (abs (log (abs left)))
               (abs (log (abs right))))
            80)
         INVALID-VALUE]
        [else (* left right)]))

(define (apply-operator operator left-value right-value)
  (cond [(equal? operator '+) (+ left-value right-value)]
        [(equal? operator '-) (- left-value right-value)]
        [(equal? operator '*) (safe-multiply left-value right-value)]
        [(equal? operator 'div) (safe-divide left-value right-value)]
        [(equal? operator 'expo) (safe-expt left-value right-value)]
        [(equal? operator 'ln) (safe-log-base left-value right-value)]))

(define (safe-apply-operator operator left-value right-value)
  (cond [(or (invalid-result? left-value) (invalid-result? right-value)) INVALID-VALUE]
        [else (apply-operator operator left-value right-value)]))

;; -----------------------------------------------------------------------------
;; Data loading
;; -----------------------------------------------------------------------------

(define (read-data-points file-path)
  (call-with-input-file file-path
    (lambda (input)
      (let loop ([point (read input)])
        (cond [(eof-object? point) '()]
              [else (cons point (loop (read input)))])))))

(define data-points (read-data-points INPUT-FILE))
(define data-points-as-vectors (map list->vector data-points))

;; -----------------------------------------------------------------------------
;; Random expression generation
;; -----------------------------------------------------------------------------

(define (choose-gene-index number)
  (cond [(< number 15) 1]   ; +
        [(< number 30) 2]   ; -
        [(< number 45) 3]   ; *
        [(< number 57) 4]   ; div
        [(< number 62) 5]   ; expo
        [(< number 67) 6]   ; ln
        [(< number 80) 7]   ; a
        [(< number 93) 8]   ; b
        [else 9]))          ; constant

(define (random-gene allow-terminals?)
  (define upper-bound (if allow-terminals? 100 66))
  (list-ref-1-based ALL-GENES
                    (choose-gene-index (random-between 0 upper-bound))))

(define (generate-expression current-gene current-depth max-depth)
  (cond [(equal? current-gene 'constant) (random-constant)]
        [(member current-gene TERMINALS) current-gene]
        [(= current-depth max-depth)
         ;; At maximum depth, force a variable leaf. The original code selected
         ;; only 'a or 'b here, not a numeric constant.
         (list-ref-1-based TERMINALS (random-between 2 3))]
        [else
         (list current-gene
               (generate-expression (random-gene #t)
                                    (+ current-depth 1)
                                    max-depth)
               (generate-expression (random-gene #t)
                                    (+ current-depth 1)
                                    max-depth))]))

(define (generate-new-subtree max-depth)
  (generate-expression (random-gene #f) 0 max-depth))

(define (generate-initial-expression)
  (generate-new-subtree INITIAL-TREE-DEPTH))

(define (generate-initial-expressions amount)
  (cond [(= amount 0) '()]
        [else (cons (generate-initial-expression)
                    (generate-initial-expressions (- amount 1)))]))

;; -----------------------------------------------------------------------------
;; Tree navigation and mutation helpers
;; -----------------------------------------------------------------------------

(define (expression-node-count expression)
  (cond [(not (list? expression)) 1]
        [else (+ 1
                 (expression-node-count (cadr expression))
                 (expression-node-count (caddr expression)))]))

(define (expression-height expression)
  (cond [(not (list? expression)) 0]
        [else (+ 1
                 (max (expression-height (cadr expression))
                      (expression-height (caddr expression))))]))

(define (expression-leaves expression)
  (cond [(not (list? expression)) (list expression)]
        [else (append (expression-leaves (cadr expression))
                      (expression-leaves (caddr expression)))]))

(define (uses-both-variables? expression)
  (define leaves (expression-leaves expression))
  (and (member 'a leaves) (member 'b leaves) #t))

(define (select-random-subtree expression branch-direction)
  (cond [(not (list? expression)) expression]
        [(= 1 (random-between 0 2)) expression]
        [(= branch-direction 0)
         (select-random-subtree (cadr expression) branch-direction)]
        [else
         (select-random-subtree (caddr expression) branch-direction)]))

(define (replace-random-subtree replacement expression branch-direction already-moved?)
  (cond [(and (> already-moved? 0)
              (or (not (list? expression))
                  (= (random-between 0 2) 1)))
         replacement]
        [(= 0 branch-direction)
         (list (car expression)
               (replace-random-subtree replacement
                                       (cadr expression)
                                       branch-direction
                                       1)
               (caddr expression))]
        [else
         (list (car expression)
               (cadr expression)
               (replace-random-subtree replacement
                                       (caddr expression)
                                       branch-direction
                                       1))]))

(define (mutate-operators expression)
  (cond [(not (list? expression)) expression]
        [(= 1 (random-between 0 5))
         (list (list-ref-1-based ALL-GENES (random-between 1 6))
               (mutate-operators (cadr expression))
               (mutate-operators (caddr expression)))]
        [else
         (list (car expression)
               (mutate-operators (cadr expression))
               (mutate-operators (caddr expression)))]))

(define (grow-mutation expression already-moved?)
  (cond [(or (not (list? expression))
             (= 0 (random-between 0 1)))
         expression]
        [(and (> already-moved? 0)
              (= (random-between 0 2) 1))
         (generate-new-subtree NEW-SUBTREE-DEPTH)]
        [else
         (list (car expression)
               (grow-mutation (cadr expression) 1)
               (grow-mutation (caddr expression) 1))]))

(define (select-child-for-pruning expression branch-direction already-moved?)
  (cond [(not (list? expression))
         (list-ref-1-based TERMINALS (random-between 2 3))]
        [(and (= branch-direction 0)
              (> already-moved? 0)
              (= 1 (random-between 0 3)))
         (cadr expression)]
        [(and (= branch-direction 1)
              (> already-moved? 0)
              (= 1 (random-between 0 3)))
         (caddr expression)]
        [(= 0 branch-direction)
         (select-child-for-pruning (cadr expression) branch-direction 1)]
        [else
         (select-child-for-pruning (caddr expression) branch-direction 1)]))

(define (prune-mutation expression already-moved?)
  (cond [(and (> already-moved? 0)
              (or (not (list? expression))
                  (= 1 (random-between 0 1))))
         (select-child-for-pruning expression (random-between 0 1) 0)]
        [else
         (list (car expression)
               (prune-mutation (cadr expression) 1)
               (prune-mutation (caddr expression) 1))]))

(define (mutate expression random-value)
  (cond [(<= random-value (/ MUTATION-PROBABILITY 2))
         (mutate-operators expression)]
        [(< random-value MUTATION-PROBABILITY)
         (grow-mutation expression 0)]
        [(> (expression-node-count expression) MAX-NODES-BEFORE-PRUNING)
         (prune-mutation expression 0)]
        [else expression]))

;; -----------------------------------------------------------------------------
;; Evaluation and fitness
;; -----------------------------------------------------------------------------

(define (evaluate-expression expression a-value b-value)
  (cond [(and (not (list? expression)) (equal? expression 'a)) a-value]
        [(and (not (list? expression)) (equal? expression 'b)) b-value]
        [(not (list? expression)) expression]
        [else
         (safe-apply-operator (car expression)
                              (evaluate-expression (cadr expression) a-value b-value)
                              (evaluate-expression (caddr expression) a-value b-value))]))

(define (squared-error target predicted)
  (cond [(equal? predicted INVALID-VALUE) +inf.0]
        [else (* (- target predicted) (- target predicted))]))

(define (absolute-error-fitness expression points)
  (cond [(null? points) 0]
        [else
         (+ (abs (- (caddar points)
                    (evaluate-expression expression (caar points) (cadar points))))
            (absolute-error-fitness expression (cdr points)))]))

(define (squared-error-fitness expression points)
  (cond [(null? points) 0]
        [else
         (+ (squared-error (caddar points)
                           (evaluate-expression expression (caar points) (cadar points)))
            (squared-error-fitness expression (cdr points)))]))

(define (score-expression-with-absolute-error expression)
  (list expression (absolute-error-fitness expression data-points)))

(define (score-expression expression)
  (cond [(not (uses-both-variables? expression)) (list expression +inf.0)]
        [else (list expression (squared-error-fitness expression data-points))]))

(define (score-expressions expressions)
  (cond [(null? expressions) '()]
        [else (cons (score-expression (car expressions))
                    (score-expressions (cdr expressions)))]))

;; -----------------------------------------------------------------------------
;; Selection, crossover, and population generation
;; -----------------------------------------------------------------------------

(define (best-individual individuals current-best)
  (cond [(null? individuals) current-best]
        [(< (cadar individuals) (cadr current-best))
         (best-individual (cdr individuals) (car individuals))]
        [else (best-individual (cdr individuals) current-best)]))

(define (random-parent-candidates population amount)
  (cond [(= amount 0) '()]
        [else
         (cons (list-ref-1-based population
                                 (random-between 1 INDIVIDUALS-PER-POPULATION))
               (random-parent-candidates population (- amount 1)))]))

(define (select-parent population tournament-size)
  (best-individual (random-parent-candidates population tournament-size)
                   (list-ref-1-based population tournament-size)))

(define (crossover parent-1 parent-2)
  (list
   (score-expression
    (mutate
     (replace-random-subtree (select-random-subtree parent-2 (random-between 0 1))
                             parent-1
                             (random-between 0 1)
                             0)
     (random-between 0 100)))
   (score-expression
    (mutate
     (replace-random-subtree (select-random-subtree parent-1 (random-between 0 1))
                             parent-2
                             (random-between 0 1)
                             0)
     (random-between 0 100)))))

(define (next-generation population child-pair-count)
  (cond [(= child-pair-count CHILD-PAIRS-PER-GENERATION) '()]
        [else
         (append (crossover (car (select-parent population TOURNAMENT-SIZE))
                            (car (select-parent population TOURNAMENT-SIZE)))
                 (next-generation population (+ child-pair-count 1)))]))

(define (make-initial-population)
  (score-expressions (generate-initial-expressions INDIVIDUALS-PER-POPULATION)))

(define (make-initial-populations amount)
  (cond [(= amount 0) '()]
        [else (cons (make-initial-population)
                    (make-initial-populations (- amount 1)))]))

;; -----------------------------------------------------------------------------
;; Parallel evolution helpers
;; -----------------------------------------------------------------------------

(define (generation-futures populations)
  (map (lambda (population)
         (future (lambda () (next-generation population 0))))
       populations))

(define (global-best populations current-best)
  (cond [(null? populations) current-best]
        [else (best-individual (car populations)
                               (global-best (cdr populations) current-best))]))

(define (finish-generation generation-results current-state)
  (list (global-best (cadr current-state) (car current-state))
        (map touch generation-results)))

(define (insert-elite elite populations)
  (cond [(null? populations) populations]
        [else
         (cons (cons (score-expression (mutate (car elite) (random-between 0 100)))
                     (car populations))
               (insert-elite elite (cdr populations)))]))

;; -----------------------------------------------------------------------------
;; Migration between populations
;; -----------------------------------------------------------------------------

(define (split-migrants population count first-half second-half)
  (cond [(= count MIGRANTS-PER-MIGRATION)
         (list first-half second-half population)]
        [(< count MIGRANTS-PER-HALF)
         (split-migrants (cdr population)
                         (+ count 1)
                         (cons (car population) first-half)
                         second-half)]
        [else
         (split-migrants (cdr population)
                         (+ count 1)
                         first-half
                         (cons (car population) second-half))]))

(define (migrate-three-populations population-1 population-2 population-3)
  (list (append (cadr population-2) (car population-3) (caddr population-1))
        (append (cadr population-3) (car population-1) (caddr population-2))
        (append (cadr population-1) (car population-2) (caddr population-3))))

(define (migrate-selected-populations populations)
  (append (migrate-three-populations
           (split-migrants (car populations) 0 '() '())
           (split-migrants (cadr populations) 0 '() '())
           (split-migrants (caddr populations) 0 '() '()))
          (cdddr populations)))

(define (all-but-last values)
  (cond [(null? (cdr values)) '()]
        [else (cons (car values) (all-but-last (cdr values)))]))

(define (rotate-last-population-to-front populations)
  (cons (car (drop populations (- POPULATION-COUNT 1)))
        (all-but-last populations)))

(define (migrate-if-needed populations generation-number)
  (cond [(not (= 0 (remainder generation-number MIGRATION-PERIOD))) populations]
        [else (migrate-selected-populations
               (rotate-last-population-to-front populations))]))

;; -----------------------------------------------------------------------------
;; Output and optional plotting
;; -----------------------------------------------------------------------------

(define (decimal-or-infinity value)
  (cond [(= +inf.0 value) value]
        [else (real->decimal-string value)]))

(define (write-analysis scored-individuals output)
  (cond [(null? scored-individuals) (close-output-port output)]
        [else
         (displayln (list (decimal-or-infinity (cadar scored-individuals))
                          (expression-node-count (caar scored-individuals)))
                    output)
         (write-analysis (cdr scored-individuals) output)]))

(define (save-final-result result file-path)
  (displayln result)
  (write-analysis (list result)
                  (open-output-file file-path #:exists 'truncate)))

(define (save-perfect-result result file-path generation-number)
  (displayln result)
  (displayln generation-number)
  (define output (open-output-file file-path #:exists 'truncate))
  (write result output)
  (write-analysis (list result) output))

(define (graph-expression expression title angle)
  (plot3d/dc (list (points3d data-points-as-vectors
                             #:color 'red
                             #:sym 'fullcircle1)
                   (surface3d (lambda (a b) (evaluate-expression expression a b))
                              #:color 2))
             (send canvas get-dc)
             0 0
             (- (send frame get-width) 50)
             (- (send frame get-height) 50)
             #:title title
             #:x-label "EJE X"
             #:y-label "EJE Y"
             #:z-label "EJE Z"
             #:altitude 10
             #:angle angle))

;; -----------------------------------------------------------------------------
;; Evolution loop
;; -----------------------------------------------------------------------------

(define (evolve current-state generation-number graph-angle)
  (displayln (cadar current-state))
  (cond [(= generation-number MAX-GENERATIONS)
         (save-final-result
          (score-expression-with-absolute-error
           (car (global-best (cadr current-state) (car current-state))))
          BEST-FOUND-FILE)]
        [(= 0 (cadar current-state))
         (save-perfect-result
          (score-expression-with-absolute-error (caar current-state))
          PERFECT-ANSWER-FILE
          generation-number)]
        [else
         (evolve
          (finish-generation
           (generation-futures
            (insert-elite (car current-state)
                          (migrate-if-needed (cadr current-state) generation-number)))
           current-state)
          (+ generation-number 1)
          graph-angle)]))

(define (start)
  (define initial-populations (make-initial-populations POPULATION-COUNT))
  (evolve (list (caar initial-populations) initial-populations) 0 0))

(module+ main
  (start))
