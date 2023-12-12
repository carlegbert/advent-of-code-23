#lang racket

(require
  "../lib/helpers.rkt"
  threading
  racket/file
  racket/trace)

(define (col-empty grid i)
  (not (for/first
         ([line grid]
          #:when (~> line (string-ref i) (equal? #\#)))
         #t)))

(define (get-empty-cols grid)
  (~> (for/list
        ([i (~> grid car string-length)]
         #:when (col-empty grid i))
        i)
      list->set))

(define (get-empty-rows grid)
  (~> (for/list
        ([i (length grid)]
         #:when (not (string-contains? (list-ref grid i)"#")))
        i)
      list->set))

(struct galaxy (v h)
  #:transparent)

(define (get-galaxies grid)
  (~>
    (for/list ([v (length grid)])
      (for/list ([h (string-length (car grid))]
        #:when (equal? (string-ref (list-ref grid v) h) #\#))
        (galaxy v h)))
    flatten
    list->set))

(define (get-galaxy-pairs g)
  (~>> (for/list ([a g])
      (for/list ([b g]
                 #:when (not (equal? a b)))
        (set a b)))
      flatten
      list->set
      set->list
      (map set->list)))

(define (1d-distance pair expanded-space accessor age)
  (let* ([space-to-cross (~>> pair
                           (map accessor)
                           (sort _ <)
                           (apply range))]
         [expanded (~> space-to-cross
                       list->set
                       (set-intersect (list->set expanded-space))
                       set->list
                       )])
    (+ (length space-to-cross) (* age (length expanded)))))

(define (distance pair empty-rows empty-cols age)
  (+ (1d-distance pair empty-rows galaxy-v age)
     (1d-distance pair empty-cols galaxy-h age)))

(define (solve-p1 fname)
  (let* ([grid (file->lines fname)]
         [empty-rows (get-empty-rows grid)]
         [empty-cols (get-empty-cols grid)]
         [galaxies (get-galaxies grid)]
         [pairs (get-galaxy-pairs galaxies)])
    (~>> pairs
        (map (lambda (p)
              (distance p empty-rows empty-cols 1)))
        (apply +))))

(define (solve-p2 fname)
  (let* ([grid (file->lines fname)]
         [empty-rows (get-empty-rows grid)]
         [empty-cols (get-empty-cols grid)]
         [galaxies (get-galaxies grid)]
         [pairs (get-galaxy-pairs galaxies)])
    (~>> pairs
        (map (lambda (p)
              (distance p empty-rows empty-cols 999999)))
        (apply +))))

(module+ test
  (require
    rackunit
    rackunit/text-ui)

  (define input-file "../test_inputs/day_11.txt")
  (define suite
    (test-suite "day 11 tests"
                (test-equal?
                  "part 1 with sample input"
                  (solve-p1 input-file)
                  374)))

  (run-tests suite))

(module* main #f
  (define part (vector-ref (current-command-line-arguments) 0))
  (define input-file "inputs/day_11.txt")
  (match part
    ["1" (solve-p1 input-file)]
    ["2" (solve-p2 input-file)]
    [else (error "specify part 1 or 2")]))

