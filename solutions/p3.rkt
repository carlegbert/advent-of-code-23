#lang racket

(require
  "../lib/helpers.rkt"
  "../lib/debugging.rkt"
  threading
  racket/file
  racket/set)

(define (symbol-locations-from-line line-number line)
  (~>> line
       (regexp-match-positions* #rx"[^0-9.]")
       (or _ '())
       (map car)
       (map (lambda (x) (cons x line-number)))))

(define (get-symbol-locations fname)
  (~>> fname
       file->lines
       (enumerate symbol-locations-from-line)
       (apply append)
       (map (lambda (x) (cons x '())))
       make-hash))

(define (get-number-neighbors line-number start-and-end)
  (apply
    append
    (for/list ([x (range (- (car start-and-end) 1) (add1 (cdr start-and-end)))])
      (for/list ([y (range (- line-number 1) (+ line-number 2))])
        (cons x y)))))

(define (neighbors-in-symbol-map neighbors symbol-map)
 ;; this will use hash-has-key? instead of set-member?
  (some (lambda (neighbor) (hash-has-key? symbol-map neighbor)) neighbors))

(define (part-numbers-from-line symbol-map line-number line)
  ;; todo rewrite without state
  ;; maybe thread number+location as a pair/list
  (let* ([number-positions (regexp-match-positions* #rx"[0-9]+" line)]
  [number-positions (or number-positions '())]
  [number-positionsx (filter (lambda (number-position)
    (neighbors-in-symbol-map (get-number-neighbors line-number number-position) symbol-map)) number-positions)]
    ;; logic somewhere to add the number to the hash set
              [numstrings (map (lambda (pos) (substring line (car pos) (cdr pos))) number-positionsx)]
              [nums (map string->number numstrings)]
              )
              nums))
  ; (~>> line
  ;      (regexp-match-positions* #rx"[0-9]+")
  ;      (or _ '())
  ;      (map (lambda (number-start-and-end)
  ;             (get-number-neighbors line-number number-start-and-end)))
  ;      (filter (lambda (neighbors) (neighbors-in-symbol-map neighbors symbol-map)))
  ;      (apply append)
  ;      (print-and-return)
  ;      (map (lambda (start-end) (substring line (car start-end) (cdr start-end))))
  ;      ))

(define (solve-p1 fname) 
  (let ([symbol-map (get-symbol-locations fname)])
    (~>> fname
         file->lines
         (enumerate (lambda (i line)
           (part-numbers-from-line symbol-map i line)))
         (apply append)
         (apply +)
         )))

(define (solve-p2 fname) 0)

(module+ test
  (require
    rackunit
    rackunit/text-ui)

  (define input-file "../test_inputs/day_3.txt")
  (define suite
    (test-suite "day 3 tests"
                (test-equal?
                  "part 1 with sample input"
                  (solve-p1 input-file)
                  4361)
                (test-equal?
                  "part 2 with sample input"
                  (solve-p2 input-file)
                  0)))

  (run-tests suite))

(module* main #f
  (define part (vector-ref (current-command-line-arguments) 0))
  (define input-file "inputs/day_3.txt")
  (match part
    ["1" (solve-p1 input-file)]
    ["2" (solve-p2 input-file)]
    [else (error "specify part 1 or 2")]))


