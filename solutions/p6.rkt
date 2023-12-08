#lang racket

(require
  "../lib/helpers.rkt"
  threading
  racket/file)

(struct race (time distance)
  #:transparent
  #:methods gen:custom-write
  [(define (write-proc race-val output-port output-mode)
     (fprintf output-port "#<race:~a ~a>"
              (race-time race-val)
              (race-distance race-val)))])

(define (get-multiple-races fname)
  (~>> fname
       file->lines
       (map (lambda~>> (regexp-match* #rx"[0-9]+")
                       (map string->number)))
       (apply (lambda (a b) (map race a b)))))

(define (get-race fname)
  (~>> fname
       file->lines
       (map (lambda~>> (regexp-match* #rx"[0-9]+")
                       (string-join _ "")
                       string->number))
       (apply race)))

(define (get-all-race-solutions race)
  (let ([time (race-time race)]
        [distance (race-distance race)])
    (foldl
      (lambda (ms-held winning-distances)
        (let* ([time-left (- time ms-held)]
               [result (* time-left ms-held)])
          (if (> result distance)
            (cons result winning-distances)
            winning-distances)))
      '()
      (range 0 time))))

(define (solve-p1 fname) 
  (~>> fname
       get-multiple-races
       (map get-all-race-solutions)
       (map length)
       (apply *)))

(define (solve-p2 fname)
  (~>> fname
       get-race
       get-all-race-solutions
       length))

(module+ test
  (require
    rackunit
    rackunit/text-ui)

  (define input-file "../test_inputs/day_6.txt")
  (define suite
    (test-suite "day 6 tests"
                (test-equal?
                  "part 1 with sample input"
                  (solve-p1 input-file)
                  288)
                (test-equal?
                  "part 2 with sample input"
                  (solve-p2 input-file)
                  71503)))

  (run-tests suite))

(module* main #f
  (define part (vector-ref (current-command-line-arguments) 0))
  (define input-file "inputs/day_6.txt")
  (match part
    ["1" (solve-p1 input-file)]
    ["2" (solve-p2 input-file)]
    [else (error "specify part 1 or 2")]))


