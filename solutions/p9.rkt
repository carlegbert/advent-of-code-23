#lang racket

(require
  "../lib/helpers.rkt"
  threading
  racket/file
  racket/trace)

(define (build-new-level level)
  (define (iter nums)
    (if (null? (cdr nums))
      '()
      (cons (- (cadr nums) (car nums))
            (iter (cdr nums)))))
  (iter level))

(define (build-pyramid starting-level)
  (define (iter level)
    (if (andmap zero? level)
      '()
      (let ([new-level (build-new-level level)])
        (cons new-level (iter new-level)))))
  (cons starting-level (iter starting-level)))

(define (parse-line line)
  (~>> line
       (regexp-match* #rx"[-0-9]+")
       (map string->number)))

(define (solve-p1 fname)
  (~>> fname
       file->lines
       (map (lambda~>> parse-line
                       build-pyramid
                       (map reverse)
                       (map car)
                       (foldr (lambda (item acc) (+ item acc)) 0)))
       (apply +)))


(define (solve-p2 fname)
  (~>> fname
       file->lines
       (map (lambda~>> parse-line
                       build-pyramid
                       (map car)
                       (foldr (lambda (item acc) (- item acc)) 0)))
       (apply +)))

(module+ test
  (require
    rackunit
    rackunit/text-ui)

  (define input-file "../test_inputs/day_9.txt")
  (define suite
    (test-suite "day 9 tests"
                (test-equal?
                  "part 1 with sample input"
                  (solve-p1 input-file)
                  114)

                (test-equal?
                  "part 2 with sample input"
                  (solve-p2 input-file)
                  2)))

  (run-tests suite))

(module* main #f
  (define part (vector-ref (current-command-line-arguments) 0))
  (define input-file "inputs/day_9.txt")
  (match part
    ["1" (solve-p1 input-file)]
    ["2" (solve-p2 input-file)]
    [else (error "specify part 1 or 2")]))


