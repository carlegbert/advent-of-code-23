#lang racket

(require
  "../lib/helpers.rkt"
  threading
  racket/file)

(define (hash-char c current-value)
  (~> c
      char->integer
      (+ current-value)
      (* 17)
      (remainder 256)))

(define (hash-str s)
  (foldl hash-char 0 (string->list s)))

(struct lens-config
  ;; The - action will be
  ;; encoded as val=#f.
  (s hashkey val)
  #:transparent)

(define (list-update-item items pred new-val)
  (cond [(null? items) (cons new-val '())]
        [(pred (car items))
         (cons new-val (cdr items))]
        [else (cons (car items)
                    (list-update-item (cdr items) pred new-val))]))

(define (make-lens-config s)
  (let* ([parts (regexp-match #px"([a-z]+)[-=]([1-9]*)" s)]
         [s (cadr parts)]
         [hashkey (hash-str s)]
         [val (string->number (caddr parts))])
    (lens-config s hashkey val)))

(define (process-lens-config ht lc)
  (hash-update!
    ht (lens-config-hashkey lc)
    (lambda (items)
      (if (lens-config-val lc)
        (list-update-item
          items
          (lambda (x) (equal? (lens-config-s x) (lens-config-s lc)))
          lc)
        (filter (lambda (x) (not (equal? (lens-config-s x)
                                         (lens-config-s lc))))
                items)))
    '()))

(define (focusing-power lens-box)
  (~>> lens-box
       (enumerate
         (lambda (i lc)
           (* (add1 (lens-config-hashkey lc))
              (add1 i)
              (lens-config-val lc))))
       (apply +)))

(define (solve-p1 s)
  (~>> s
       (string-split _ ",")
       (map string-trim)
       (map hash-str)
       (apply +)))

(define (solve-p2 fname)
  (let* ([ht (make-hash)])
    (~>> fname
         file->string
         (string-split _ ",")
         (map string-trim)
         (map make-lens-config)
         (map (lambda~>> (process-lens-config ht))))
    (~>> ht
         hash-values
         (map focusing-power)
         (apply +))))

(module+ test
  (require
    rackunit
    rackunit/text-ui)

  (define input-file "../test_inputs/day_15.txt")
  (define suite
    (test-suite "day 15 tests"
                (test-equal?
                  "part 1 with HASH"
                  (solve-p1 "HASH")
                  52)

                (test-equal?
                  "part 1 with sample input"
                  (solve-p1 (file->string input-file))
                  1320)

                (test-equal?
                  "part 2 with sample input"
                  (solve-p2 input-file)
                  145)))

  (run-tests suite))

(module* main #f
  (define part (vector-ref (current-command-line-arguments) 0))
  (define input-file "inputs/day_15.txt")
  (match part
    ["1" (solve-p1 (file->string input-file))]
    ["2" (solve-p2 input-file)]
    [else (error "specify part 1 or 2")]))


