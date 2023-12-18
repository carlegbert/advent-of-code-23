#lang racket

(require
  "../lib/helpers.rkt"
  threading
  racket/file
  racket/trace)

(define (hash-char c current-value)
  (~> c
      char->integer
      (+ current-value)
      (* 17)
      (remainder 256)))

(define (hash-str s)
  (foldl hash-char 0 (string->list s)))

(define (solve-p1 s)
  (~>> s
       (string-split _ ",")
       (map string-trim)
       (map hash-str)
       (apply +)))

(define (solve-p2 fname)
  0)

(module+ test
  (require
    rackunit
    rackunit/text-ui)

  (define input-file "../test_inputs/day_15.txt")
  (define suite
    (test-suite "day 15 tests"
                (test-equal?
                  "part 1 with HASH"
                  (solve-p1 "HASH")
                  52)

                (test-equal?
                  "part 1 with sample input"
                  (solve-p1 (file->string input-file))
                  1320)

                (test-equal?
                  "part 2 with sample input"
                  (solve-p2 input-file)
                  0)))

  (run-tests suite))

(module* main #f
  (define part (vector-ref (current-command-line-arguments) 0))
  (define input-file "inputs/day_15.txt")
  (match part
    ["1" (solve-p1 (file->string input-file))]
    ["2" (solve-p2 input-file)]
    [else (error "specify part 1 or 2")]))


