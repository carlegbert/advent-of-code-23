#lang racket

(require
  "../lib/helpers.rkt"
  threading
  racket/file
  racket/trace)

(define (column-load col)
  (define (iter item running-load current-load idx)
    (cond [(null? item)
           running-load]
          [(equal? #\O (car item))
           (iter (cdr item)
                 (+ current-load running-load)
                 (sub1 current-load)
                 (sub1 idx))]
          [(equal? #\# (car item))
           (iter (cdr item)
                 running-load
                 (sub1 idx)
                 (sub1 idx))]
          [else (iter (cdr item)
                 running-load
                 current-load
                 (sub1 idx))]))
  (define len (length col))
  (iter col 0 len len))

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
      (map column-load)
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
                 0)))

  (run-tests suite))

(module* main #f
  (define part (vector-ref (current-command-line-arguments) 0))
  (define input-file "inputs/day_14.txt")
  (match part
    ["1" (solve-p1 input-file)]
    ["2" (solve-p2 input-file)]
    [else (error "specify part 1 or 2")]))


