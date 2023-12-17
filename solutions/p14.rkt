#lang racket

(require
  "../lib/helpers.rkt"
  threading
  racket/file
  racket/trace)

(define (roll-boulders col)
  (~>> col
       list->string
       (string-split _ "#" #:trim? #f)
       (map string->list)
       (map (lambda~> (sort (lambda (a b) (equal? a #\O)))))
       (map list->string)
       (string-join _ "#")
       string->list))

(define (load col)
  (define len (length col))
  (foldl
    (lambda (item acc)
      (if (equal? #\O (cdr item))
        (+ acc (- len (car item)))
        acc))
    0
    (enumerated col)))

(define (flip-grid grid)
  (define (iter x)
    (if (null? (car x))
      '()
      (cons (map (lambda~> car) x)
            (iter (map (lambda~> cdr) x)))))
  (iter grid))

(define (file->cols fname)
  (~>> fname file->lines (map string->list) flip-grid))


(define (solve-p1 fname)
  (~>> fname
       file->cols
       (map roll-boulders)
       (map load)
       (apply +)))

(define (solve-p2 fname)
  0)

(module+ test
  (require
    rackunit
    rackunit/text-ui)

  (define input-file "../test_inputs/day_14.txt")
  (define suite
    (test-suite "day 14 tests"
                (test-equal?
                  "part 1 with sample input"
                  (solve-p1 input-file)
                  136)

                (test-equal?
                  "part 2 with sample input"
                  (solve-p2 input-file)
                  64)))

  (run-tests suite))

(module* main #f
  (define part (vector-ref (current-command-line-arguments) 0))
  (define input-file "inputs/day_14.txt")
  (match part
    ["1" (solve-p1 input-file)]
    ["2" (solve-p2 input-file)]
    [else (error "specify part 1 or 2")]))


