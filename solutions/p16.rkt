#lang racket

(require
  "../lib/helpers.rkt"
  threading
  racket/file
  racket/trace)

(define (file->grid fname)
  (~>> fname file->lines (map string->list) (map list->vector) list->vector))

(struct beam (x y direction)
  #:transparent)

(define (move-beam b direction)
  (match direction
    ["L" (beam (sub1 (beam-x b)) (beam-y b) "L")]
    ["R" (beam (add1 (beam-x b)) (beam-y b) "R")]
    ["U" (beam (beam-x b) (sub1 (beam-y b)) "U")]
    ["D" (beam (beam-x b) (add1 (beam-y b)) "D")]))

(define (grid-ref g b)
  (~> g (vector-ref (beam-y b)) (vector-ref (beam-x b))))

(define (inside-grid? b grid)
  (and (< (beam-x b) (vector-length (vector-ref grid 0)))
       (>= (beam-x b) 0)
       (< (beam-y b) (vector-length grid))
       (>= (beam-y b) 0)))

(define (outside-grid? b grid)
  (not (inside-grid? b grid)))

(define (lateral? d)
  (or (equal? d "L") (equal? d "R")))

(define (vertical? d)
  (not (lateral? d)))

(define (advance-beam beam grid)
  (let ([c (grid-ref grid beam)]
        [d (beam-direction beam)])
    (cond [(and (equal? c #\|) (lateral? d))
           (list (move-beam beam "U") (move-beam beam "D"))]
          [(and (equal? c #\-) (vertical? d))
           (list (move-beam beam "R") (move-beam beam "L"))]

          [(and (equal? c #\\) (equal? d "L"))
           (list (move-beam beam "U"))]
          [(and (equal? c #\\) (equal? d "R"))
           (list (move-beam beam "D"))]
          [(and (equal? c #\\) (equal? d "U"))
           (list (move-beam beam "L"))]
          [(and (equal? c #\\) (equal? d "D"))
           (list (move-beam beam "R"))]

          [(and (equal? c #\/) (equal? d "L"))
           (list (move-beam beam "D"))]
          [(and (equal? c #\/) (equal? d "R"))
           (list (move-beam beam "U"))]
          [(and (equal? c #\/) (equal? d "U"))
           (list (move-beam beam "R"))]
          [(and (equal? c #\/) (equal? d "D"))
           (list (move-beam beam "L"))]
          [else (list (move-beam beam d))])))

(define (process-beams grid beams visited)
              (let ([beam (car-or beams #f)])
    (cond [(not beam) visited]
          [(outside-grid? beam grid)
           (process-beams grid (cdr beams) visited)]
          [(set-member? visited beam)
           (process-beams grid (cdr beams) visited)]
          [else (begin
                  (set-add! visited beam)
                  (process-beams
                    grid
                    (append (advance-beam beam grid) (cdr beams))
                    visited))])))

(define (solve-p1 fname)
  (~>> fname
       file->grid
       (process-beams _ (list (beam 0 0 "R")) (mutable-set))
       set->list
       (map (lambda (b) (cons (beam-x b) (beam-y b))))
       list->set
       set-count))

(define (solve-p2 fname)
  0)

(module+ test
  (require
    rackunit
    rackunit/text-ui)

  (define input-file "../test_inputs/day_16.txt")
  (define suite
    (test-suite "day 16 tests"
                (test-equal?
                  "part 1 with sample input"
                  (solve-p1 input-file)
                  46)
                (test-equal?
                  "part 2 with sample input"
                  (solve-p2 input-file)
                  0)))

  (run-tests suite))

(module* main #f
  (define part (vector-ref (current-command-line-arguments) 0))
  (define input-file "inputs/day_16.txt")
  (match part
    ["1" (solve-p1 input-file)]
    ["2" (solve-p2 input-file)]
    [else (error "specify part 1 or 2")]))


