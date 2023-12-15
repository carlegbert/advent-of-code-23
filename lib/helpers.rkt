#lang racket

(require
  threading)

(provide
  every
  some
  first-regexp-match-or
  enumerate
  not-zero
  list-of
  print-and-return
  default
  add-uneven-lists
  string-counter
  translate
  hash-pop
  index-of
  replace-first
  car-or)

(define (print-and-return x)
  (display x)
  (newline)
  x)

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

(define (not-zero n)
  (not (= 0 n)))

(define (list-of size val)
  (~>> size
      range
      (map (lambda (x) val))))

(define (add-uneven-lists a b)
  (cond [(null? a) b]
        [(null? b) a]
        [else
          (cons (+ (car a) (car b))
                (add-uneven-lists (cdr a) (cdr b)))]))

(define (default fn fallback item)
  (if (null? item)
    fallback
    (fn item)))

(define (string-counter s)
  (let ([hmap (make-hash)]
        [chars (string->list s)])
    (for/list ([char chars])
      (hash-update! hmap char add1 0))
    hmap))

(define (translate s original translation)
  (let* ([original (string->list original)]
         [translation (string->list translation)]
         [mappings (map cons original translation)]
         [h (make-hash mappings)])
    (~>> s
         string->list
         (map (lambda (c) (hash-ref h c c)))
         (apply string))))

(define (hash-pop . args)
  (let ([val (apply hash-ref args)])
    (hash-remove! (car args) (cadr args))
    val))

(define (index-of items x [i 0])
  (cond [(null? items) #f]
        [(equal? (car items) x) i]
        [else (index-of (cdr items) (add1 i))]))

(define (replace-first items x replacement)
  (if (equal? (car items) x)
    (cons replacement (cdr items))
    (cons (car x) (replace-first (cdr items) x replacement))))

(define (car-or item alt)
  (if (null? item)
    alt
    (car item)))
