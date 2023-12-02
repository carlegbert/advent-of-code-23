#lang racket

(provide
  every
  first-regexp-match-or)

(define (every fn items)
  (andmap fn items))

(define (first-of-list-or items-or-false alternate)
  (or
    (and items-or-false (string->number (car items-or-false)))
    alternate))

(define (first-regexp-match-or rx s alternate)
  (let ([matches (regexp-match rx s)])
    (if matches
      (car matches)
      alternate)))
