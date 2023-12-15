#lang racket

(require
  "../lib/helpers.rkt"
  threading
  racket/file
  racket/trace)

(define (replace-first replacement items)
  (cons replacement (cdr items)))

(define (combos parts nums hmap)
  (define first-part (car-or parts ""))
  (define first-num (car-or nums #f))

  (hash-ref! hmap
             (list parts nums)
             (thunk (cond [(and (null? nums) (null? parts)) 1]
                          [(null? parts) 0]
                          [(string-contains? first-part "?")
                           (+ (combos (append (string-split
                                                (string-replace first-part "?" "." #:all? #f)
                                                ".")
                                              (cdr parts))
                                      nums
                                      hmap)
                              (combos (replace-first (string-replace first-part "?" "#" #:all? #f) parts)
                                      nums
                                      hmap))]
                          [(equal? (string-length first-part) first-num)
                           (combos (cdr parts) (cdr nums) hmap)]
                          [else 0]))))

(define (combos-per-line line)
  (let* ([parts (string-split line)]
         [nums (~> parts cadr (string-split ",") (map string->number _))]
         [string-parts (~> parts car (string-split #px"\\.+"))])
    (combos string-parts nums (make-hash))))

(define (solve-p1 fname)
  (~>> fname
      file->lines
      (map combos-per-line)
      (apply +)))

(define (solve-p2 fname) 0)

(module+ test
  (require
    rackunit
    rackunit/text-ui)

  (define input-file "../test_inputs/day_12.txt")
  (define suite
    (test-suite "day 12 tests"
                (test-equal?
                  "part 1 with sample input"
                  (solve-p1 input-file)
                  21)
                (test-equal?
                  "part 2 with sample input"
                  (solve-p2 input-file)
                  0)))

  (run-tests suite))

(module* main #f
  (define part (vector-ref (current-command-line-arguments) 0))
  (define input-file "inputs/day_12.txt")
  (match part
    ["1" (solve-p1 input-file)]
    ["2" (solve-p2 input-file)]
    [else (error "specify part 1 or 2")]))


