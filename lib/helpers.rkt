#lang racket

(provide
  every
  some
  first-regexp-match-or
  enumerate)

(define (every fn items)
  (andmap fn items))

(define (some fn items)
  (ormap fn items))

(define (first-of-list-or items-or-false alternate)
  (or
    (and items-or-false (string->number (car items-or-false)))
    alternate))

(define (first-regexp-match-or rx s alternate)
  (let ([matches (regexp-match rx s)])
    (if matches
      (car matches)
      alternate)))

(define (enumerate fn items)
  (define (iter _items idx)
    (if (null? _items)
      '()
      (cons (fn idx (car _items))
            (iter (cdr _items) (add1 idx)))))
  (iter items 0))
