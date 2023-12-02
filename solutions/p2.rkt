#lang racket

(require
  "../lib/helpers.rkt"
  threading
  racket/file)

(define (string->rgb s)
  (let* ([r (first-regexp-match-or #rx"[0-9]+(?= red)" s "0")]
         [g (first-regexp-match-or #rx"[0-9]+(?= green)" s "0")]
         [b (first-regexp-match-or #rx"[0-9]+(?= blue)" s "0")])
    (map string->number (list r g b))))

(define (parse-game-info line)
  (~>> line
       (regexp-match #rx"(?=:).*")
       car
       (string-split _ ";")
       (map string->rgb)))

(define (solve-p1 fname)
  (define bag (list 12 13 14))

  (define (value-from-line line)
    (define (bag-can-contain colors)
      (andmap >= bag colors))
    (let ([game-num (string->number (car (regexp-match #rx"[0-9]+" line)))])
      (~>> line
           parse-game-info
           (map bag-can-contain)
           (every identity)
           (if _ game-num 0))))

  (~>> fname
      file->lines
      (map value-from-line)
      (apply +)))

(define (solve-p2 fname)
  (define (max-colors list-of-rgbs)
    (foldl (lambda (acc item)
             (map max acc item))
           (list 0 0 0)
           list-of-rgbs))

  (define (min-cubes line)
    (~>> line
         parse-game-info
         max-colors
         (apply *)))

  (~>> fname
       file->lines
       (map min-cubes)
       (apply +)))

(module+ test
  (require
    rackunit
    rackunit/text-ui)

  (define input-file "../test_inputs/day_2.txt")
  (define suite
    (test-suite "day 2 tests"
                (test-equal?
                  "part 1 with sample input"
                  (solve-p1 input-file)
                  8)
                (test-equal?
                  "part 2 with sample input"
                  (solve-p2 input-file)
                  2286)))

  (run-tests suite))

(module* main #f
  (define part (vector-ref (current-command-line-arguments) 0))
  (define input-file "inputs/day_2.txt")
  (match part
    ["1" (solve-p1 input-file)]
    ["2" (solve-p2 input-file)]
    [else (error "specify part 1 or 2")]))


