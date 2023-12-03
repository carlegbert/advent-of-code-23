#lang racket

(require
  "../lib/helpers.rkt"
  "../lib/debugging.rkt"
  threading
  racket/file
  racket/set)

(define (symbol-locations-from-line line-number symbol-regexp line)
  (~>> line
       (regexp-match-positions* symbol-regexp)
       (or _ '())
       (map car)
       (map (lambda (x) (cons x line-number)))))

(define (get-symbol-locations fname symbol-regexp)
  (~>> fname
       file->lines
       (enumerate (lambda (line-number line)
                    (symbol-locations-from-line line-number symbol-regexp line)))
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
  (some (lambda (neighbor) (hash-has-key? symbol-map neighbor)) neighbors))

(define (visit-number symbol-map line-number location-and-value)
  (for ([n (get-number-neighbors line-number (car location-and-value))])
    (when (hash-has-key? symbol-map n)
      (hash-update! symbol-map n (lambda (items) (cons (cdr location-and-value) items))))))

(define (visit-line symbol-map line-number line)
  (~>> line
       (regexp-match-positions* #rx"[0-9]+")
       (or _ '())

       (map (lambda (number-start-and-end)
              (cons number-start-and-end
                    (string->number
                      (substring line
                                 (car number-start-and-end)
                                 (cdr number-start-and-end))))))

       (map (lambda (x) (visit-number symbol-map line-number x)))))

(define (solve-p1 fname) 
  (let ([symbol-map (get-symbol-locations fname #rx"[^0-9.]")])
    (~>> fname
         file->lines
         (enumerate (lambda (i line)
                      (visit-line symbol-map i line))))
    (~>> symbol-map
         hash-values
         (apply append)
         remove-duplicates
         (apply +))))

(define (solve-p2 fname)
  (let ([symbol-map (get-symbol-locations fname #rx"[^0-9.]")])
    (~>> fname
         file->lines
         (enumerate (lambda (i line)
                      (visit-line symbol-map i line))))
    (~>> symbol-map
         hash-values
         (filter (lambda (nums)
                   (= (length nums) 2)))
         (map (lambda (n) (apply * n)))
         (apply +))))

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
                  467835)))

  (run-tests suite))

(module* main #f
  (define part (vector-ref (current-command-line-arguments) 0))
  (define input-file "inputs/day_3.txt")
  (match part
    ["1" (solve-p1 input-file)]
    ["2" (solve-p2 input-file)]
    [else (error "specify part 1 or 2")]))


