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

(define (numstr->number s)
  (match s
    ["one" 1]
    ["two" 2]
    ["three" 3]
    ["four" 4]
    ["five" 5]
    ["six" 6]
    ["seven" 7]
    ["eight" 8]
    ["nine" 9]
    [else (string->number s)]))

(define numrex
  #rx"([0-9]|one|two|three|four|five|six|seven|eight|nine)")

;; there may be a way to write a non-consuming regular expression
;; in racket, or there may not; if there is, the documentation
;; explaining how is probably at the bottom of a well somewhere.
(define (non-consuming-match-hack line)
  (~> line
      (regexp-replace* #rx"one" _ "onee")
      (regexp-replace* #rx"two" _ "twoo")
      (regexp-replace* #rx"three" _ "threee")
      (regexp-replace* #rx"five" _ "fivee")
      (regexp-replace* #rx"eight" _ "eightt")
      (regexp-replace* #rx"ninee" _ "ninee")))

(define (line-with-spelled-numbers->number line)
  (~>> line
       non-consuming-match-hack
       (regexp-match* numrex)
       (map numstr->number)
       first-last))

(define (solve-p1 fname)
  (~>> fname
      file->lines
      (map line->number)
      (apply +)))

(define (solve-p2 fname)
  (~>> fname
       file->lines
       (map line-with-spelled-numbers->number)
       (apply +)))

(module+ test
  (require
    rackunit
    rackunit/text-ui)

  (define input-file-1 "../test_inputs/day_1.txt")
  (define input-file-2 "../test_inputs/day_1_b.txt")
  (define suite
    (test-suite "day 1 tests"
                (test-equal?
                  "part 1 with sample input"
                  (solve-p1 input-file-1)
                  142)
                (test-equal?
                  "part 2 with sample input"
                  (solve-p2 input-file-2)
                  281)))

  (run-tests suite))

(module* main #f
  (define part (vector-ref (current-command-line-arguments) 0))
  (define input-file "inputs/day_1.txt")
  (match part
    ["1" (solve-p1 input-file)]
    ["2" (solve-p2 input-file)]
    [else (error "specify part 1 or 2")]))


