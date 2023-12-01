#lang racket

(require
  racket/file)

(define (solve-p1 fname) 0)

(define (solve-p2 fname) 0)

(module+ test
  (require
    rackunit
    rackunit/text-ui)

  (define suite
    (define input-file "../test_inputs/day_{{DAY}}.txt")
    (test-suite "day {{DAY}} tests"
                (test-equal?
                  "part 1 with sample input"
                  (solve-p1 input-file)
                  0)
                (test-equal?
                  "part 2 with sample input"
                  (solve-p2 input-file)
                  0)))

  (run-tests suite))

(module* main #f
  (define part (vector-ref (current-command-line-arguments) 0))
  (define input-file "inputs/day_{{DAY}}.txt")
  (match part
    ["1" (solve-p1 input-file)]
    ["2" (solve-p2 input-file)]
    [else (error "specify part 1 or 2")]))


