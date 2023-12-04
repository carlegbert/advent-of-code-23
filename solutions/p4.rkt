#lang racket

(require
  "../lib/helpers.rkt"
  "../lib/debugging.rkt"
  threading
  racket/file
  racket/set)

(define (line->winning-numbers line)
  (~>> line
      (string-split _ "|")
      car
      (string-split _ ":")
      cadr
      (regexp-match* #rx"[0-9]+")
      (map string->number)
      list->set))

(define (line->scratched-numbers line)
  (~>> line
      (string-split _ "|")
      cadr
      (regexp-match* #rx"[0-9]+")
      (map string->number)
      list->set))

(define (number-of-winning-numbers line)
  (~> line
      line->winning-numbers
      (set-intersect (line->scratched-numbers line))
      set->list
      length))

(define (score-line-length len)
  (~> len
      (- 1)
      (expt 2 _)
      round))

(define (solve-p1 fname)
  (~>> fname
       file->lines
       (map number-of-winning-numbers)
       (map score-line-length)
       (apply +)))

(define (solve-p2 fname) 0)

(module+ test
  (require
    rackunit
    rackunit/text-ui)

  (define input-file "../test_inputs/day_4.txt")
  (define suite
    (test-suite "day 4 tests"
                (test-equal?
                  "part 1 with sample input"
                  (solve-p1 input-file)
                  13)
                (test-equal?
                  "part 2 with sample input"
                  (solve-p2 input-file)
                  0)))

  (run-tests suite))

(module* main #f
  (define part (vector-ref (current-command-line-arguments) 0))
  (define input-file "inputs/day_4.txt")
  (match part
    ["1" (solve-p1 input-file)]
    ["2" (solve-p2 input-file)]
    [else (error "specify part 1 or 2")]))


