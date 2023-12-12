#lang racket

(require
  "../lib/helpers.rkt"
  threading
  racket/file
  racket/trace)

(define (file->grid fname)
  (~>> fname
       file->lines
       list->vector))

(struct position (vert horiz)
  #:transparent)

(define ALL-DIRECTIONS (string->list "NSEW"))

(define (move-position p d)
  (match d
    [#\N (position (sub1 (position-vert p))
                   (position-horiz p))]
    [#\S (position (add1 (position-vert p))
                   (position-horiz p))]
    [#\W (position (position-vert p)
                   (sub1 (position-horiz p)))]
    [#\E (position (position-vert p)
                   (add1 (position-horiz p)))]))

(define (get-next-direction pipe-type current-direction)
  (match (list pipe-type current-direction)
    [(list #\| #\N) #\N]
    [(list #\| #\S) #\S]

    [(list #\- #\W) #\W]
    [(list #\- #\E) #\E]

    [(list #\L #\W) #\N]
    [(list #\L #\S) #\E]

    [(list #\J #\E) #\N]
    [(list #\J #\S) #\W]

    [(list #\7 #\N) #\W]
    [(list #\7 #\E) #\S]

    [(list #\F #\W) #\S]
    [(list #\F #\N) #\E]

    [_ #f]))

(define (grid-ref grid position)
  (if (in-grid grid position)
    (~> grid
        (vector-ref (position-vert position))
        (string-ref (position-horiz position)))
    #\X))

(define (in-grid grid p)
  (and (<= 0 (position-vert p))
       (<= 0 (position-horiz p))
       (< (position-vert p) (vector-length grid))
       (< (position-horiz p) (string-length (vector-ref grid 0)))))

(define (find-start grid)
  (for/first ([v (range (vector-length grid))]
              #:when (string-contains? (vector-ref grid v) "S"))
    (for/first ([h (range (string-length (vector-ref grid 1)))]
                #:when (equal? #\S (grid-ref grid (position v h))))
      (position v h))))

(define (all-possible-moves start grid)
  (filter
    identity
    (for/list ([d ALL-DIRECTIONS])
      (let* ([potential-next-position (move-position start d)]
             [potential-next-pipe (grid-ref grid potential-next-position)]
             [potential-next-direction (get-next-direction potential-next-pipe d)])
        (if potential-next-direction
          d
          #f)))))

(define (stream-path grid start first-move)
  (define (iter pos direction i)
    (if (equal? pos start)
      (stream-cons (cons i pos) '())
      (let* ([next-direction (get-next-direction (grid-ref grid pos) direction)]
             [pos (move-position pos next-direction)])
        (stream-cons (cons i pos) (iter pos next-direction (add1 i))))))
  (iter (move-position start first-move)
        first-move
        2))

(define (solve-p1 fname)
  (let* ([grid (file->grid fname)]
         [start (find-start grid)]
         [possible-moves (all-possible-moves start grid)]
         [streams (map (lambda~>> (stream-path grid start)) possible-moves)])
    (for/first ([a (car streams)]
                [b (cadr streams)]
                #:when (equal? (cdr a) (cdr b)))
      (car a))))

(define (position-is-contained pos grid)
  (define (iter p crossing-count direction pipe-entrance)
    (let* ([ch (grid-ref grid p)]
           [next-position (move-position p direction)]
           [on-a-corner-pipe (string-contains? "FJ7L" (string ch))])
      (cond [(equal? #\S ch)
             (iter pos 0 #\S #f)]
            [(equal? ch #\X)
             crossing-count]
            [(equal? ch #\-)
             (iter next-position (add1 crossing-count) direction #f)]
            [(equal? ch #\|)
             (iter next-position crossing-count direction pipe-entrance)]
            [(and (equal? #\F ch) (equal? #\J pipe-entrance))
             (iter next-position (add1 crossing-count) direction #f)]
            [(and (equal? #\J ch) (equal? #\F pipe-entrance))
             (iter next-position (add1 crossing-count) direction #f)]
            [(and (equal? #\L ch) (equal? #\7 pipe-entrance))
             (iter next-position (add1 crossing-count) direction #f)]
            [(and (equal? #\7 ch) (equal? #\L pipe-entrance))
             (iter next-position (add1 crossing-count) direction #f)]
            [(and on-a-corner-pipe pipe-entrance)
             (iter next-position crossing-count direction #f)]
            [on-a-corner-pipe
              (iter next-position crossing-count direction ch)]
            [else (iter next-position crossing-count direction #f)])))

  (if (equal? (grid-ref grid pos) #\.)
    (odd? (iter pos 0 #\N #f))
    #f))

(define (clean-grid grid path)
  (list->vector
    (for/list ([v (vector-length grid)])
      (list->string
        (for/list ([h (string-length (vector-ref grid 0))])
          (let* ([pos (position v h)]
                 [c (grid-ref grid pos)])
            (if (and (set-member? path pos)
                     (string-contains? "JL7FS-|" (string c)))
              c
              #\.)))))))

(define (solve-p2 fname)
  (let* ([grid (file->grid fname)]
         [grid-height (~> grid vector-length)]
         [grid-width (~> grid (vector-ref 0) string-length)]
         [start (~>> grid find-start)]
         [first-move (~> start (all-possible-moves grid) car)]
         [path (~> grid (stream-path start first-move) stream->list (map cdr _) list->set)]
         [grid (clean-grid grid path)])
    (for/fold ([outer-sum 0]) ([v (range grid-height)])
      (+
        (for/fold ([inner-sum 0]) ([h (range grid-width)])
          (if (~> (position v h) (position-is-contained grid))
            (begin
              (add1 inner-sum))
            inner-sum))
        outer-sum))))

(module+ test
  (require
    rackunit
    rackunit/text-ui)

  (define input-file "../test_inputs/day_10.txt")
  (define input-file-b "../test_inputs/day_10_b.txt")
  (define input-file-c "../test_inputs/day_10_c.txt")
  (define input-file-d "../test_inputs/day_10_d.txt")
  (define input-file-e "../test_inputs/day_10_e.txt")
  (define suite
    (test-suite "day 10 tests"
                (test-equal?
                  "part 1 with sample input, simple"
                  (solve-p1 input-file)
                  4)

                (test-equal?
                  "part 1 with sample input, complex"
                  (solve-p1 input-file-b)
                  8)

                (test-equal?
                  "part 2 with sample input, simple"
                  (solve-p2 input-file)
                  1)

                (test-equal?
                  "part 2 with sample input, complex"
                  (solve-p2 input-file-b)
                  1)

                (test-equal?
                  "part 2 with sample input, complexer"
                  (solve-p2 input-file-c)
                  4)

                (test-equal?
                  "part 2 with sample input, even complexer"
                  (solve-p2 input-file-d)
                  4)

                (test-equal?
                  "part 2 with sample input, complexest"
                  (solve-p2 input-file-e)
                  8)))

  (run-tests suite))

(module* main #f
  (define part (vector-ref (current-command-line-arguments) 0))
  (define input-file "inputs/day_10.txt")
  (match part
    ["1" (solve-p1 input-file)]
    ["2" (solve-p2 input-file)]
    [else (error "specify part 1 or 2")]))

