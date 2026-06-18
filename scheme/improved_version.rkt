#lang racket

(require plot
         racket/class
         racket/cmdline
         racket/future
         racket/list
         racket/match
         racket/string)

;; -----------------------------------------------------------------------------
;; Symbolic regression with a genetic algorithm.
;;
;; Input format:
;;   - One point per line: (a b z)
;;   - Or raw numbers grouped by triples: a b z a b z ...
;;
;; Example:
;;   racket improved_version.rkt --input prueba.txt --generations 1000 --show-plots
;; -----------------------------------------------------------------------------

(struct cfg (input-file
             generations
             population-size
             island-count
             tournament-size
             mutation-percent
             migrant-count
             migration-period
             initial-depth
             grow-depth
             max-nodes
             parsimony-coefficient
             validation-percent
             patience
             min-improvement
             random-seed
             output-dir
             save-plots?
             show-plots?
             plot-every
             progress-every)
  #:transparent)

(struct point (a b z) #:transparent)
(struct individual (tree sse mae nodes height fitness) #:transparent)
(struct history-row (generation fitness sse mae nodes height expression) #:transparent)

(define EPSILON 1e-7)
(define BAD +inf.0)
(define SEED-MODULUS (expt 2 31))

(define binary-operators '(+ - * div expo ln))
(define terminal-symbols '(a b constant))

;; Default weights for sumbols.
(define all-symbol-weights
  '((+ . 15) (- . 15) (* . 15) (div . 12) (expo . 5) (ln . 5)
    (a . 13) (b . 13) (constant . 8)))

(define operator-weights
  '((+ . 15) (- . 15) (* . 15) (div . 12) (expo . 5) (ln . 5)))

;; -----------------------------------------------------------------------------
;; Utilities
;; -----------------------------------------------------------------------------

(define (parse-natural text name)
  (define value (string->number text))
  (unless (and (exact-integer? value) (>= value 0))
    (error name "expected a non-negative integer, got: ~a" text))
  value)

(define (parse-nonnegative-real text name)
  (define value (string->number text))
  (unless (and (real? value) (finite-real? value) (>= value 0))
    (error name "expected a non-negative real number, got: ~a" text))
  (exact->inexact value))

(define (random-between low high)
  (+ low (random (add1 (- high low)))))

(define (random-ref* xs)
  (list-ref xs (random (length xs))))

(define (weighted-choice weighted-pairs)
  (define total (for/sum ([pair weighted-pairs]) (cdr pair)))
  (define target (random total))
  (let loop ([pairs weighted-pairs] [seen 0])
    (match pairs
      ['() (caar weighted-pairs)]
      [(cons (cons symbol weight) rest)
       (define next (+ seen weight))
       (if (< target next) symbol (loop rest next))])))

(define (finite-real? x)
  (and (real? x) (= x x) (< (abs x) +inf.0)))

(define (number->output-string x)
  (cond [(not (finite-real? x)) "+inf"]
        [else (real->decimal-string (exact->inexact x) 6)]))

(define (safe-result x)
  (if (finite-real? x) (exact->inexact x) BAD))

;; -----------------------------------------------------------------------------
;; Data loading
;; -----------------------------------------------------------------------------

(define (read-all-datums path)
  (with-input-from-file path
    (lambda ()
      (let loop ([items '()])
        (define item (read))
        (if (eof-object? item)
            (reverse items)
            (loop (cons item items)))))))

(define (group-triples xs)
  (cond [(null? xs) '()]
        [(< (length xs) 3)
         (error 'load-points "raw numeric input must contain a multiple of 3 values")]
        [else (cons (take xs 3) (group-triples (drop xs 3)))]))

(define (datum->point datum)
  (match datum
    [(list a b z)
     #:when (and (number? a) (number? b) (number? z))
     (point (exact->inexact a) (exact->inexact b) (exact->inexact z))]
    [(vector a b z)
     #:when (and (number? a) (number? b) (number? z))
     (point (exact->inexact a) (exact->inexact b) (exact->inexact z))]
    [_ (error 'load-points "invalid point: ~a" datum)]))

(define (load-points path)
  (unless (file-exists? path)
    (error 'load-points "input file not found: ~a" path))
  (define datums (read-all-datums path))
  (define point-datums
    (if (andmap number? datums)
        (group-triples datums)
        datums))
  (define points (map datum->point point-datums))
  (when (null? points)
    (error 'load-points "the input file contains no points"))
  points)

;; -----------------------------------------------------------------------------
;; Protected arithmetic
;; -----------------------------------------------------------------------------

(define (safe-div a b)
  (cond [(not (and (finite-real? a) (finite-real? b))) BAD]
        [(<= (abs b) EPSILON) BAD]
        [(<= (abs a) EPSILON) 0.0]
        [else (safe-result (/ a b))]))

(define (safe-log-base base value)
  (cond [(not (and (finite-real? base) (finite-real? value))) BAD]
        [(or (<= base 0) (<= value 0) (<= (abs (- base 1.0)) EPSILON)) BAD]
        [else (safe-result (/ (log value) (log base)))]))

(define (safe-pow base exponent)
  (cond [(not (and (finite-real? base) (finite-real? exponent))) BAD]
        [(and (<= (abs base) EPSILON) (<= exponent 0)) BAD]
        [(and (< base 0) (not (integer? exponent))) BAD]
        [(<= (abs base) EPSILON) 0.0]
        [(> (abs (* (log (abs base)) exponent)) 70) BAD]
        [else (safe-result (real-part (expt base exponent)))]))

(define (safe-mul left right)
  (cond [(not (and (finite-real? left) (finite-real? right))) BAD]
        [(or (<= (abs left) EPSILON) (<= (abs right) EPSILON)) 0.0]
        [else
         (define log-product (+ (log (abs left)) (log (abs right))))
         (if (> log-product 80) BAD (safe-result (* left right)))]))

(define (eval-operator operator left right)
  (cond [(not (and (finite-real? left) (finite-real? right))) BAD]
        [else
         (case operator
           [(+) (safe-result (+ left right))]
           [(-) (safe-result (- left right))]
           [(*) (safe-mul left right)]
           [(div) (safe-div left right)]
           [(expo) (safe-pow left right)]
           [(ln) (safe-log-base left right)]
           [else BAD])]))

;; -----------------------------------------------------------------------------
;; Expression trees
;; -----------------------------------------------------------------------------

(define (random-constant)
  (- (* 40.0 (random)) 20.0))

(define (make-random-tree root-symbol depth max-depth)
  (cond [(eq? root-symbol 'constant) (random-constant)]
        [(member root-symbol '(a b)) root-symbol]
        [(>= depth max-depth) (random-ref* '(a b))]
        [else
         (list root-symbol
               (make-random-tree (weighted-choice all-symbol-weights) (add1 depth) max-depth)
               (make-random-tree (weighted-choice all-symbol-weights) (add1 depth) max-depth))]))

(define (make-random-individual max-depth)
  (make-random-tree (weighted-choice operator-weights) 0 max-depth))

(define (tree-leaves tree)
  (if (list? tree)
      (append (tree-leaves (second tree)) (tree-leaves (third tree)))
      (list tree)))

(define (uses-both-variables? tree)
  (define leaves (tree-leaves tree))
  (and (member 'a leaves) (member 'b leaves) #t))

(define (node-count tree)
  (if (list? tree)
      (+ 1 (node-count (second tree)) (node-count (third tree)))
      1))

(define (tree-height tree)
  (if (list? tree)
      (+ 1 (max (tree-height (second tree)) (tree-height (third tree))))
      0))

(define (eval-tree tree a b)
  (match tree
    ['a a]
    ['b b]
    [(? number? n) n]
    [(list operator left right)
     (eval-operator operator (eval-tree left a b) (eval-tree right a b))]
    [_ BAD]))

(define (tree->infix tree)
  (match tree
    ['a "a"]
    ['b "b"]
    [(? number? n) (number->output-string n)]
    [(list '+ left right) (format "(~a + ~a)" (tree->infix left) (tree->infix right))]
    [(list '- left right) (format "(~a - ~a)" (tree->infix left) (tree->infix right))]
    [(list '* left right) (format "(~a * ~a)" (tree->infix left) (tree->infix right))]
    [(list 'div left right) (format "(~a / ~a)" (tree->infix left) (tree->infix right))]
    [(list 'expo left right) (format "(~a ^ ~a)" (tree->infix left) (tree->infix right))]
    [(list 'ln left right) (format "log_base(~a, ~a)" (tree->infix left) (tree->infix right))]
    [_ (format "~a" tree)]))

;; -----------------------------------------------------------------------------
;; Mutation and crossover
;; -----------------------------------------------------------------------------

(define (random-subtree tree)
  (cond [(not (list? tree)) tree]
        [(= 1 (random-between 0 2)) tree]
        [(zero? (random 2)) (random-subtree (second tree))]
        [else (random-subtree (third tree))]))

(define (replace-random-subtree replacement tree [below-root? #f])
  (cond [(and below-root? (or (not (list? tree)) (= 1 (random-between 0 2)))) replacement]
        [(not (list? tree)) replacement]
        [(zero? (random 2))
         (list (first tree)
               (replace-random-subtree replacement (second tree) #t)
               (third tree))]
        [else
         (list (first tree)
               (second tree)
               (replace-random-subtree replacement (third tree) #t))]))

(define (operator-mutation tree)
  (cond [(not (list? tree)) tree]
        [else
         (define operator
           (if (= 1 (random-between 0 5))
               (weighted-choice operator-weights)
               (first tree)))
         (list operator
               (operator-mutation (second tree))
               (operator-mutation (third tree)))]))

(define (grow-mutation tree cfg [below-root? #f])
  (cond [(not (list? tree)) tree]
        [(and below-root? (= 1 (random-between 0 2)))
         (make-random-individual (cfg-grow-depth cfg))]
        [else
         (list (first tree)
               (grow-mutation (second tree) cfg #t)
               (grow-mutation (third tree) cfg #t))]))

(define (prune-child tree side below-root?)
  (cond [(not (list? tree)) (random-ref* '(a b))]
        [(and below-root? (= side 0)) (second tree)]
        [(and below-root? (= side 1)) (third tree)]
        [(zero? side) (prune-child (second tree) side #t)]
        [else (prune-child (third tree) side #t)]))

(define (prune-mutation tree [below-root? #f])
  (cond [(and below-root? (or (not (list? tree)) (= 1 (random-between 0 1))))
         (prune-child tree (random 2) #f)]
        [(not (list? tree)) tree]
        [else
         (list (first tree)
               (prune-mutation (second tree) #t)
               (prune-mutation (third tree) #t))]))

(define (perturb-constant n)
  (define delta (- (* 2.0 (random)) 1.0))
  (safe-result (+ n delta)))

(define (constant-mutation tree)
  (define (walk current)
    (match current
      [(? number? n)
       (if (zero? (random 2)) (perturb-constant n) n)]
      [(list operator left right)
       (list operator (walk left) (walk right))]
      [_ current]))
  (define mutated (walk tree))
  (if (equal? mutated tree)
      (replace-random-subtree (random-constant) tree)
      mutated))

(define (enforce-max-nodes tree cfg)
  (let loop ([candidate tree] [attempts 0])
    (if (or (<= (node-count candidate) (cfg-max-nodes cfg))
            (>= attempts 5))
        candidate
        (loop (prune-mutation candidate) (add1 attempts)))))

(define (mutate-tree tree cfg)
  (define mutated
    (if (< (random 100) (cfg-mutation-percent cfg))
        (case (random 3)
          [(0) (operator-mutation tree)]
          [(1) (grow-mutation tree cfg)]
          [(2) (constant-mutation tree)])
        tree))
  (enforce-max-nodes mutated cfg))

(define (crossover parent-a parent-b points cfg)
  (define tree-a (individual-tree parent-a))
  (define tree-b (individual-tree parent-b))
  (list
   (score-tree (mutate-tree (replace-random-subtree (random-subtree tree-b) tree-a) cfg) points cfg)
   (score-tree (mutate-tree (replace-random-subtree (random-subtree tree-a) tree-b) cfg) points cfg)))

;; -----------------------------------------------------------------------------
;; Fitness and selection
;; -----------------------------------------------------------------------------

(define (individual-penalized-fitness sse nodes cfg)
  (if (finite-real? sse)
      (+ sse (* (cfg-parsimony-coefficient cfg) nodes))
      BAD))

(define (score-tree tree points cfg)
  (define nodes (node-count tree))
  (define height (tree-height tree))
  (cond [(or (null? points) (not (uses-both-variables? tree)))
         (individual tree BAD BAD nodes height BAD)]
        [else
         (define-values (sse absolute-error-sum)
           (for/fold ([sse 0.0]
                      [absolute-error-sum 0.0])
                     ([p points])
             (define predicted (eval-tree tree (point-a p) (point-b p)))
             (if (finite-real? predicted)
                 (let ([err (- (point-z p) predicted)])
                   (values (+ sse (sqr err))
                           (+ absolute-error-sum (abs err))))
                 (values BAD BAD))))
         (define mae
           (if (finite-real? absolute-error-sum)
               (/ absolute-error-sum (length points))
               BAD))
         (individual tree sse mae nodes height (individual-penalized-fitness sse nodes cfg))]))

(define (better-individual? a b)
  (< (individual-fitness a) (individual-fitness b)))

(define (best-individual population)
  (foldl (lambda (candidate current-best)
           (if (better-individual? candidate current-best) candidate current-best))
         (first population)
         (rest population)))

(define (sort-population population)
  (sort population < #:key individual-fitness))

(define (tournament-select population cfg)
  (best-individual
   (for/list ([_ (in-range (cfg-tournament-size cfg))])
     (random-ref* population))))

(define (initial-population points cfg)
  (for/list ([_ (in-range (cfg-population-size cfg))])
    (score-tree (make-random-individual (cfg-initial-depth cfg)) points cfg)))

(define (best-across-islands islands)
  (best-individual (map best-individual islands)))

(define (next-population population points cfg)
  (define local-best (best-individual population))
  (define target-children (sub1 (cfg-population-size cfg)))
  (define pair-count (quotient (+ target-children 1) 2))
  (define children
    (take
     (apply append
            (for/list ([_ (in-range pair-count)])
              (crossover (tournament-select population cfg)
                         (tournament-select population cfg)
                         points
                         cfg)))
     target-children))
  (cons local-best children))

(define (migrate-islands islands cfg)
  (define m (min (cfg-migrant-count cfg) (cfg-population-size cfg)))
  (if (or (<= m 0) (< (length islands) 2))
      islands
      (let* ([sorted-islands (map sort-population islands)]
             [outgoing (map (lambda (p) (take p m)) sorted-islands)]
             [remaining (map (lambda (p) (drop p m)) sorted-islands)]
             [incoming (cons (last outgoing) (drop-right outgoing 1))])
        (map append incoming remaining))))

(define (maybe-migrate islands generation cfg)
  (if (and (> generation 0)
           (> (cfg-migration-period cfg) 0)
           (= 0 (remainder generation (cfg-migration-period cfg))))
      (migrate-islands islands cfg)
      islands))

;; -----------------------------------------------------------------------------
;; Reporting and plots
;; -----------------------------------------------------------------------------

(define (make-history-row generation best)
  (history-row generation
               (individual-fitness best)
               (individual-sse best)
               (individual-mae best)
               (individual-nodes best)
               (individual-height best)
               (tree->infix (individual-tree best))))

(define (print-progress generation best)
  (printf "gen ~a | fitness=~a | sse=~a | mae=~a | nodes=~a | height=~a | expr=~a~n"
          generation
          (number->output-string (individual-fitness best))
          (number->output-string (individual-sse best))
          (number->output-string (individual-mae best))
          (individual-nodes best)
          (individual-height best)
          (tree->infix (individual-tree best))))

(define (ensure-output-dir! path)
  (make-directory* path))

(define (output-path cfg filename)
  (build-path (cfg-output-dir cfg) filename))

(define (csv-escape text)
  (string-append "\"" (string-replace text "\"" "\"\"") "\""))

(define (write-history! history cfg)
  (call-with-output-file (output-path cfg "history.csv")
    #:exists 'replace
    (lambda (out)
      (displayln "generation,fitness,sse,mae,nodes,height,expression" out)
      (for ([row history])
        (fprintf out "~a,~a,~a,~a,~a,~a,~a~n"
                (history-row-generation row)
                (number->output-string (history-row-fitness row))
                (number->output-string (history-row-sse row))
                (number->output-string (history-row-mae row))
                (history-row-nodes row)
                (history-row-height row)
                (csv-escape (history-row-expression row)))))))

(define (write-summary! best validation-score reason generation cfg)
  (call-with-output-file (output-path cfg "best-expression.txt")
    #:exists 'replace
    (lambda (out)
      (fprintf out "Status: ~a~n" reason)
      (fprintf out "Generation: ~a~n" generation)
      (fprintf out "Training fitness: ~a~n" (number->output-string (individual-fitness best)))
      (fprintf out "Training SSE: ~a~n" (number->output-string (individual-sse best)))
      (fprintf out "Training MAE: ~a~n" (number->output-string (individual-mae best)))
      (when validation-score
        (fprintf out "Validation SSE: ~a~n" (number->output-string (individual-sse validation-score)))
        (fprintf out "Validation MAE: ~a~n" (number->output-string (individual-mae validation-score))))
      (fprintf out "Nodes: ~a~n" (individual-nodes best))
      (fprintf out "Height: ~a~n" (individual-height best))
      (fprintf out "Expression: ~a~n" (tree->infix (individual-tree best)))
      (fprintf out "Raw tree: ~s~n" (individual-tree best)))))

(define (point->vector p)
  (vector (point-a p) (point-b p) (point-z p)))

(define (range-with-padding nums)
  (define low (apply min nums))
  (define high (apply max nums))
  (if (= low high)
      (values (- low 1.0) (+ high 1.0))
      (let ([pad (* 0.05 (- high low))])
        (values (- low pad) (+ high pad)))))

(define (plot-safe-eval tree a b)
  (define predicted (eval-tree tree a b))
  (if (finite-real? predicted) predicted 0.0))

(define (surface-renderers tree data-points x-min x-max y-min y-max)
  (list (surface3d (lambda (a b) (plot-safe-eval tree a b))
                   x-min x-max y-min y-max
                   #:alpha 0.70
                   #:label "Best expression")
        (points3d (map point->vector data-points)
                  #:sym 'fullcircle1
                  #:label "Data points")))

(define (save-surface-plot! best points path)
  (define tree (individual-tree best))
  (define-values (x-min x-max) (range-with-padding (map point-a points)))
  (define-values (y-min y-max) (range-with-padding (map point-b points)))
  (plot3d-file (surface-renderers tree points x-min x-max y-min y-max)
               (path->string path)
               #:title (format "Best model | SSE ~a" (number->output-string (individual-sse best)))
               #:x-label "a"
               #:y-label "b"
               #:z-label "z"
               #:x-min x-min
               #:x-max x-max
               #:y-min y-min
               #:y-max y-max
               #:width 900
               #:height 700))

(define (save-convergence-plot! history path)
  (define finite-history
    (filter (lambda (row) (finite-real? (history-row-fitness row))) history))
  (when (not (null? finite-history))
    (plot-file (lines (for/list ([row finite-history])
                        (vector (history-row-generation row) (history-row-fitness row))))
               (path->string path)
               #:title "Convergence"
               #:x-label "Generation"
               #:y-label "Penalized fitness"
               #:width 900
               #:height 500)))

(define (show-surface-plot! best points)
  (define-values (x-min x-max) (range-with-padding (map point-a points)))
  (define-values (y-min y-max) (range-with-padding (map point-b points)))
  ;; Dynamic GUI loading keeps command-line/headless runs usable when --show-plots is off.
  (define frame% (dynamic-require 'racket/gui/base 'frame%))
  (define canvas% (dynamic-require 'racket/gui/base 'canvas%))
  (define frame (new frame%
                     [label "Symbolic Regression - Best Surface"]
                     [width 900]
                     [height 700]))
  (define canvas
    (new canvas%
         [parent frame]
         [paint-callback
          (lambda (canvas dc)
            (plot3d/dc (surface-renderers (individual-tree best) points x-min x-max y-min y-max)
                       dc
                       0
                       0
                       (send canvas get-width)
                       (send canvas get-height)
                       #:title (format "Best model | SSE ~a" (number->output-string (individual-sse best)))
                       #:x-label "a"
                       #:y-label "b"
                       #:z-label "z"
                       #:x-min x-min
                       #:x-max x-max
                       #:y-min y-min
                       #:y-max y-max
                       #:altitude 10
                       #:angle 45))]))
  (send frame show #t)
  canvas)

;; -----------------------------------------------------------------------------
;; Evolution loop
;; -----------------------------------------------------------------------------

(define (make-seeded-rng seed)
  (define rng (make-pseudo-random-generator))
  (parameterize ([current-pseudo-random-generator rng])
    (random-seed (modulo seed SEED-MODULUS)))
  rng)

(define (make-unseeded-rng)
  (make-seeded-rng (random SEED-MODULUS)))

(define (make-island-rngs cfg)
  (for/list ([island-index (in-range (cfg-island-count cfg))])
    (if (cfg-random-seed cfg)
        (make-seeded-rng (+ (cfg-random-seed cfg) island-index 1))
        (make-unseeded-rng))))

(define (make-split-rng cfg)
  (if (cfg-random-seed cfg)
      (make-seeded-rng (+ (cfg-random-seed cfg) 104729))
      (make-unseeded-rng)))

(define (shuffle-list xs)
  (define v (list->vector xs))
  (define n (vector-length v))
  (when (> n 1)
    (for ([i (in-range (sub1 n) 0 -1)])
      (define j (random (add1 i)))
      (define tmp (vector-ref v i))
      (vector-set! v i (vector-ref v j))
      (vector-set! v j tmp)))
  (vector->list v))

(define (split-points points cfg)
  (define percent (cfg-validation-percent cfg))
  (cond [(zero? percent) (values points '())]
        [(< (length points) 2)
         (error 'config "validation split requires at least two points")]
        [else
         (define shuffled
           (parameterize ([current-pseudo-random-generator (make-split-rng cfg)])
             (shuffle-list points)))
         (define validation-count
           (min (sub1 (length points))
                (max 1 (quotient (* (length points) percent) 100))))
         (values (drop shuffled validation-count)
                 (take shuffled validation-count))]))

(define (evolve train-points cfg)
  (define island-rngs (make-island-rngs cfg))
  (define islands
    (for/list ([rng island-rngs])
      (parameterize ([current-pseudo-random-generator rng])
        (initial-population train-points cfg))))
  (let loop ([generation 0]
             [current-islands islands]
             [history '()]
             [best-so-far #f]
             [last-improvement-generation 0])
    (define candidate (best-across-islands current-islands))
    (define improvement
      (if best-so-far
          (- (individual-fitness best-so-far) (individual-fitness candidate))
          +inf.0))
    (define new-best
      (if (or (not best-so-far) (better-individual? candidate best-so-far))
          candidate
          best-so-far))
    (define new-last-improvement-generation
      (if (> improvement (cfg-min-improvement cfg))
          generation
          last-improvement-generation))
    (define current-history (cons (make-history-row generation new-best) history))
    (when (or (= generation 0)
              (= 0 (remainder generation (max 1 (cfg-progress-every cfg))))
              (= generation (cfg-generations cfg))
              (= 0 (individual-sse new-best)))
      (print-progress generation new-best))
    (when (and (cfg-save-plots? cfg)
               (> (cfg-plot-every cfg) 0)
               (> generation 0)
               (= 0 (remainder generation (cfg-plot-every cfg))))
      (save-surface-plot! new-best train-points (output-path cfg (format "surface-gen-~a.png" generation))))
    (cond [(= 0 (individual-sse new-best))
           (values new-best (reverse current-history) "exact fit" generation)]
          [(>= generation (cfg-generations cfg))
           (values new-best (reverse current-history) "maximum generations reached" generation)]
          [(and (> (cfg-patience cfg) 0)
                (>= (- generation new-last-improvement-generation) (cfg-patience cfg)))
           (values new-best
                   (reverse current-history)
                   (format "stopped after ~a generations without material fitness improvement" (cfg-patience cfg))
                   generation)]
          [else
           (define migrated-islands (maybe-migrate current-islands generation cfg))
           (define futures
             (for/list ([population migrated-islands]
                        [rng island-rngs])
               (future
                (lambda ()
                  (parameterize ([current-pseudo-random-generator rng])
                    (next-population population train-points cfg))))))
           (loop (add1 generation)
                 (map touch futures)
                 current-history
                 new-best
                 new-last-improvement-generation)])))

(define (validate-config! cfg)
  (when (< (cfg-population-size cfg) 3)
    (error 'config "population size must be at least 3"))
  (when (even? (cfg-population-size cfg))
    (error 'config "population size should be odd so elitism keeps one parent plus paired children"))
  (when (< (cfg-island-count cfg) 1)
    (error 'config "island count must be at least 1"))
  (when (< (cfg-tournament-size cfg) 1)
    (error 'config "tournament size must be at least 1"))
  (when (> (cfg-tournament-size cfg) (cfg-population-size cfg))
    (error 'config "tournament size cannot exceed population size"))
  (when (or (not (exact-integer? (cfg-mutation-percent cfg)))
            (< (cfg-mutation-percent cfg) 0)
            (> (cfg-mutation-percent cfg) 100))
    (error 'config "mutation percent must be an integer between 0 and 100"))
  (when (or (not (finite-real? (cfg-parsimony-coefficient cfg)))
            (< (cfg-parsimony-coefficient cfg) 0))
    (error 'config "parsimony coefficient must be a non-negative finite number"))
  (when (or (not (finite-real? (cfg-min-improvement cfg)))
            (< (cfg-min-improvement cfg) 0))
    (error 'config "minimum improvement must be a non-negative finite number"))
  (when (< (cfg-initial-depth cfg) 1)
    (error 'config "initial depth must be at least 1"))
  (when (< (cfg-grow-depth cfg) 1)
    (error 'config "grow depth must be at least 1"))
  (when (< (cfg-max-nodes cfg) 3)
    (error 'config "max nodes must be at least 3"))
  (when (or (not (exact-integer? (cfg-validation-percent cfg)))
            (< (cfg-validation-percent cfg) 0)
            (> (cfg-validation-percent cfg) 99))
    (error 'config "validation percent must be an integer between 0 and 99"))
  (when (and (cfg-random-seed cfg)
             (or (not (exact-integer? (cfg-random-seed cfg)))
                 (< (cfg-random-seed cfg) 0)
                 (> (cfg-random-seed cfg) (sub1 SEED-MODULUS))))
    (error 'config "random seed must be an integer between 0 and ~a" (sub1 SEED-MODULUS))))

(define (run-symbolic-regression cfg)
  (validate-config! cfg)
  (ensure-output-dir! (cfg-output-dir cfg))
  (define points (load-points (cfg-input-file cfg)))
  (define-values (train-points validation-points) (split-points points cfg))
  (printf "Loaded ~a points from ~a~n" (length points) (cfg-input-file cfg))
  (printf "Training points: ~a | validation points: ~a~n"
          (length train-points)
          (length validation-points))
  (printf "Using ~a islands x ~a individuals for up to ~a generations~n"
          (cfg-island-count cfg)
          (cfg-population-size cfg)
          (cfg-generations cfg))
  (printf "Parsimony coefficient: ~a | patience: ~a | min improvement: ~a~n"
          (cfg-parsimony-coefficient cfg)
          (cfg-patience cfg)
          (cfg-min-improvement cfg))
  (define-values (best history reason final-generation) (evolve train-points cfg))
  (define validation-score
    (and (not (null? validation-points))
         (score-tree (individual-tree best) validation-points cfg)))
  (write-history! history cfg)
  (write-summary! best validation-score reason final-generation cfg)
  (when (cfg-save-plots? cfg)
    (save-surface-plot! best train-points (output-path cfg "best-surface.png"))
    (save-convergence-plot! history (output-path cfg "convergence.png")))
  (when (cfg-show-plots? cfg)
    (show-surface-plot! best train-points))
  (printf "~nDone: ~a~n" reason)
  (printf "Training fitness: ~a~n" (number->output-string (individual-fitness best)))
  (printf "Training SSE: ~a~n" (number->output-string (individual-sse best)))
  (printf "Training MAE: ~a~n" (number->output-string (individual-mae best)))
  (when validation-score
    (printf "Validation SSE: ~a~n" (number->output-string (individual-sse validation-score)))
    (printf "Validation MAE: ~a~n" (number->output-string (individual-mae validation-score))))
  (printf "Best expression: ~a~n" (tree->infix (individual-tree best)))
  (printf "Artifacts written to: ~a~n" (cfg-output-dir cfg))
  best)

;; -----------------------------------------------------------------------------
;; CLI
;; -----------------------------------------------------------------------------

(module+ main
  (define input-file "prueba.txt")
  (define generations 1000)
  (define population-size 101)
  (define island-count 4)
  (define tournament-size 5)
  (define mutation-percent 10)
  (define migrant-count 10)
  (define migration-period 10)
  (define initial-depth 5)
  (define grow-depth 6)
  (define max-nodes 50)
  (define parsimony-coefficient 0.001)
  (define validation-percent 20)
  (define patience 0)
  (define min-improvement 1e-6)
  (define random-seed #f)
  (define output-dir "symbolic-regression-output")
  (define save-plots? #t)
  (define show-plots? #f)
  (define plot-every 0)
  (define progress-every 100)

  (command-line
   #:program "improved_version.rkt"
   #:once-each
   [("-i" "--input") path "Input file with triples (a b z). Default: prueba.txt"
    (set! input-file path)]
   [("-g" "--generations") value "Maximum number of generations. Default: 1000"
    (set! generations (parse-natural value 'generations))]
   [("-p" "--population-size") value "Individuals per island; use an odd number. Default: 101"
    (set! population-size (parse-natural value 'population-size))]
   [("--islands") value "Number of populations/islands. Default: 4"
    (set! island-count (parse-natural value 'islands))]
   [("--tournament-size") value "Tournament selection size. Default: 5"
    (set! tournament-size (parse-natural value 'tournament-size))]
   [("--mutation-percent") value "Mutation percentage. Default: 10"
    (set! mutation-percent (parse-natural value 'mutation-percent))]
   [("--migrants") value "Migrants per island. Default: 10"
    (set! migrant-count (parse-natural value 'migrants))]
   [("--migration-period") value "Migrate every N generations; 0 disables migration. Default: 10"
    (set! migration-period (parse-natural value 'migration-period))]
   [("--initial-depth") value "Initial tree depth. Default: 5"
    (set! initial-depth (parse-natural value 'initial-depth))]
   [("--grow-depth") value "Maximum depth for growth mutation. Default: 6"
    (set! grow-depth (parse-natural value 'grow-depth))]
   [("--max-nodes") value "Prune trees larger than this node count. Default: 50"
    (set! max-nodes (parse-natural value 'max-nodes))]
   [("--parsimony") value "Node-count penalty added to SSE. Default: 0.001"
    (set! parsimony-coefficient (parse-nonnegative-real value 'parsimony))]
   [("--validation-percent") value "Holdout percentage for validation/test reporting. Default: 20"
    (set! validation-percent (parse-natural value 'validation-percent))]
   [("--patience") value "Stop after N generations without material fitness improvement; 0 disables. Default: 5"
    (set! patience (parse-natural value 'patience))]
   [("--min-improvement") value "Minimum fitness improvement that resets patience. Default: 1e-6"
    (set! min-improvement (parse-nonnegative-real value 'min-improvement))]
   [("--seed") value "Random seed for reproducible runs."
    (set! random-seed (parse-natural value 'seed))]
   [("-o" "--output-dir") path "Output directory. Default: symbolic-regression-output"
    (set! output-dir path)]
   [("--plot-every") value "Also save a surface plot every N generations; 0 saves only final plots."
    (set! plot-every (parse-natural value 'plot-every))]
   [("--progress-every") value "Print progress every N generations. Default: 100"
    (set! progress-every (parse-natural value 'progress-every))]
   [("--no-save-plots") "Do not write PNG plots."
    (set! save-plots? #f)]
   [("--show-plots") "Open the final 3D surface plot in a GUI window."
    (set! show-plots? #t)])

  (run-symbolic-regression
   (cfg input-file
        generations
        population-size
        island-count
        tournament-size
        mutation-percent
        migrant-count
        migration-period
        initial-depth
        grow-depth
        max-nodes
        parsimony-coefficient
        validation-percent
        patience
        min-improvement
        random-seed
        output-dir
        save-plots?
        show-plots?
        plot-every
        progress-every)))
