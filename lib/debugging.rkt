#lang racket

(provide
  print-and-return)

(define (print-and-return x)
  (display x)
  (newline)
  x)
