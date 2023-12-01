#lang racket

(require
  threading
  racket/file)

(define (first-last digits)
  (+ (* 10 (car digits)) (last digits)))

(define (line->number line)
  (~>> line
      string->list
      (map string)
      (map string->number)
      (filter number?)
      first-last))

(define (solve-p1 fname)
  (~>> fname
      file->lines
      (map line->number)
      (apply +)))

(define (solve-p2 fname) 0)

(module+ test
  (require
    rackunit
    rackunit/text-ui)

  (define input-file "../test_inputs/day_1.txt")
  (define suite
    (test-suite "day 1 tests"
                (test-equal?
                  "part 1 with sample input"
                  (solve-p1 input-file)
                  142)
                (test-equal?
                  "part 2 with sample input"
                  (solve-p2 input-file)
                  0)))

  (run-tests suite))

(module* main #f
  (define part (vector-ref (current-command-line-arguments) 0))
  (define input-file "inputs/day_1.txt")
  (case part
    [("1") (solve-p1 input-file)]
    [("2") (solve-p2 input-file)]
    [else (error "specify part 1 or 2")]))


