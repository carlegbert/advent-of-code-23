#lang racket

(require
  "../lib/helpers.rkt"
  threading
  racket/file)

(define (get-seeds fname)
  (~>> fname file->lines car string-split cdr (map string->number)))

(define (get-map-blocks fname)
  (~> fname
      file->string
      (string-split "\n\n")))

(define (check-block-line source line)
  (let* ([nums (~>> line string-split (map string->number))]
         [dest-start (car nums)]
         [source-start (cadr nums)]
         [range-length (caddr nums)]
         [source-end (+ source-start range-length)])
    (if (and (>= source source-start) (< source source-end))
      (+ dest-start (- source source-start))
      #f)))

(define (check-block source map-block)
  (~>> map-block
       (string-split _ "\n")
       cdr
       (ormap (lambda (block-line) (check-block-line source block-line)))
       (or _ source)))

(define (check-seed map-blocks seed)
  (foldl (lambda (map-block source)
           (~>> map-block (check-block source)))
         seed
         map-blocks))

(define (solve-p1 fname)
  (define map-blocks (cdr (get-map-blocks fname)))

  (~>> fname
       get-seeds
       (map (lambda (seed) (check-seed map-blocks seed)))
       (apply min)))

(define (check-block-line-backwards dest line)
  (let* ([nums (~>> line string-split (map string->number))]
         [dest-start (car nums)]
         [source-start (cadr nums)]
         [range-length (caddr nums)]
         [dest-end (+ dest-start range-length)])
    (if (and (>= dest dest-start) (< dest dest-end))
      (+ source-start (- dest dest-start))
      #f)))

(define (check-seed-block dest seed-block)
  (~>> seed-block
       (regexp-match* #rx"[0-9]+ [0-9]+")
       (map (lambda~>> string-split
                       (map string->number)))
       (ormap (lambda (r) (and (<= (car r) dest)
                               (> (+ (car r) (cadr r)) dest)
                               dest)))))

(define (check-block-backwards dest map-block)
  (if (regexp-match #rx"seeds: " map-block)
    (check-seed-block dest map-block)
    (~>> map-block
         (string-split _ "\n")
         cdr
         (ormap (lambda (block-line) (check-block-line-backwards dest block-line)))
         (or _ dest))))

(define (check-backwards map-blocks location)
  (and
    (foldr (lambda (map-block dest)
             (check-block-backwards dest map-block))
           location
           map-blocks)
    location))

(define (stream-from n)
  (stream-cons n (stream-from (add1 n))))

(define (solve-p2 fname) 
  (define map-blocks (get-map-blocks fname))
  (stream-ormap (lambda (x) (check-backwards map-blocks x)) (stream-from 1)))

(module+ test
  (require
    rackunit
    rackunit/text-ui)

  (define input-file "../test_inputs/day_5.txt")
  (define suite
    (test-suite "day 5 tests"
                (test-equal?
                  "part 1 with sample input"
                  (solve-p1 input-file)
                  35)

                (test-equal?
                  "part 2 with sample input"
                  (solve-p2 input-file)
                  46)))

  (run-tests suite))

(module* main #f
  (define part (vector-ref (current-command-line-arguments) 0))
  (define input-file "inputs/day_5.txt")
  (match part
    ["1" (solve-p1 input-file)]
    ["2" (solve-p2 input-file)]
    [else (error "specify part 1 or 2")]))


