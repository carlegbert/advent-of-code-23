#lang racket

(require
  "../lib/helpers.rkt"
  threading
  racket/file
  racket/trace)

(define (file->sections fname)
  (~>> fname
       file->string
       (string-split _ "\n\n")
       (map (lambda~> (string-split "\n")))))

(define (horizontal-reflection-score section)
  (~>> (for/list ([n (range 1 (length section))])
     (let* ([upper (take section n)]
            [lower (list-tail (take section (- (max (* 2 n) (length section)) 1)) (length upper))])
       (uneven-equal upper (reverse lower))))
      (filter identity)
      length
      (* 100)))

(define (reflection-score section)
  (+ (horizontal-reflection-score section)
     0))

(define (solve-p1 fname)
  (~>> fname
      file->sections
      (map reflection-score)
      (apply +)))

(define (solve-p2 fname)
  0)

(module+ test
  (require
    rackunit
    rackunit/text-ui)

  (define input-file "../test_inputs/day_13.txt")
  (define suite
    (test-suite "day 13 tests"
                (test-equal?
                  "part 1 with sample input"
                  (solve-p1 input-file)
                  405)

                (test-equal?
                  "part 2 with sample input"
                  (solve-p2 input-file)
                  0)))

  (run-tests suite))

(module* main #f
  (define part (vector-ref (current-command-line-arguments) 0))
  (define input-file "inputs/day_13.txt")
  (match part
    ["1" (solve-p1 input-file)]
    ["2" (solve-p2 input-file)]
    [else (error "specify part 1 or 2")]))


