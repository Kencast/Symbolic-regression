

#lang racket

(require racket/gui plot racket/draw)

(define num 0)

(define f (new frame% [label "Test graph"]
               [width 500]
               [height 500]))
(define c (new canvas% [parent f]))

(print (real->decimal-string -5))