#lang racket

(require
  "../lib/helpers.rkt"
  threading
  racket/file
  racket/trace)

(define (build-map lines)
  (let* ([hmap (make-hash)])
    (for/list ([line lines])
      (let ([parts (regexp-match* #rx"[A-Z]+" line)])
        (hash-set! hmap (car parts) (cdr parts))))
    hmap))

(define (map-get hmap key instruction)
  (let ([val (hash-ref hmap key)])
    (if (equal? instruction #\L)
      (car val)
      (cadr val))))

(define (path-length hmap instructions start end-matcher)
  (define (iter pos moves instruction-cursor)
    (if (end-matcher pos)
      moves
      (iter
        (map-get hmap pos (car instruction-cursor))
        (add1 moves)
        (if (null? (cdr instruction-cursor))
          instructions
          (cdr instruction-cursor)))))
  (iter start 0 instructions))

(define (solve-p1 fname)
  (let* ([lines (file->lines fname)]
        [instructions (~> lines car string->list)]
        [hmap (build-map (cddr lines))])
    (path-length
      hmap
      instructions
      "AAA"
      (lambda (pos) (equal? pos "ZZZ")))))

(define (solve-p2 fname) 0)

(module+ test
  (require
    rackunit
    rackunit/text-ui)

  (define input-file "../test_inputs/day_8.txt")
  (define input-file-b "../test_inputs/day_8_b.txt")
  (define suite
    (test-suite "day 8 tests"
                (test-equal?
                  "part 1 with sample input: no repeat"
                  (solve-p1 input-file)
                  2)

                (test-equal?
                  "part 1 with sample input: with repeat"
                  (solve-p1 input-file-b)
                  6)

                (test-equal?
                  "part 2 with sample input"
                  (solve-p2 input-file)
                  0)))

  (run-tests suite))

(module* main #f
  (define part (vector-ref (current-command-line-arguments) 0))
  (define input-file "inputs/day_8.txt")
  (match part
    ["1" (solve-p1 input-file)]
    ["2" (solve-p2 input-file)]
    [else (error "specify part 1 or 2")]))


