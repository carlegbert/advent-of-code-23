#lang racket

(require
  "../lib/helpers.rkt"
  threading
  racket/file
  racket/trace)

(struct rule
  (part-key comparator value dest)
  #:transparent)

(struct workflow
  (key rules default)
  #:transparent)

(define (part-comparator s)
  (match s [">" >] ["<" <]))

(define (rule-result r p)
  (let ([c (rule-comparator r)]
        [k (rule-part-key r)]
        [v (rule-value r)]
        [d (rule-dest r)])
    (and (c (hash-ref p k) v) d)))

(define (workflow-result w p)
  (or (ormap (lambda~> (rule-result p)) (workflow-rules w))
      (workflow-default w)))

(define (check-part part workflows key)
  (let ([result (workflow-result (hash-ref workflows key) part)])
    (match result
      ["R" #f]
      ["A" #t]
      [_ (check-part part workflows result)])))

(define (parse-rule s)
  (let* ([parts (regexp-match #px"([xmas])([<>])([0-9]+):([a-zRA]+)" s)]
         [accessor (cadr parts)]
         [comparator (part-comparator (caddr parts))]
         [val (~> parts cadddr string->number)]
         [dest (car (cddddr parts))])
    (rule accessor comparator val dest)))

(define (parse-workflow s)
  (let* ([parts (regexp-match* #px"[^{},]+" s)]
         [key (car parts)]
         [parts (reverse (cdr parts))]
         [default (car parts)]
         [parts (cdr parts)]
         [rules (map parse-rule parts)]
         [rules (reverse rules)])
    (workflow key rules default)))

(define (build-workflow-hmap s)
  (~>> s
      string-split
      (map parse-workflow)
      (map (lambda (w) (cons (workflow-key w) w)))
      make-hash))

(define (parse-part s)
  (let* ([nums (regexp-match* #px"[0-9]+" s)]
         [nums (map string->number nums)])
    (make-hash (map cons (list "x" "m" "a" "s") nums))))

(define (parse-parts s)
  (~>> s
       string-split
       (map parse-part)))

(define (solve-p1 fname)
  (let ([workflows (~> fname
                       file->string
                       (string-split "\n\n")
                       car
                       build-workflow-hmap)])
    (~>> fname
         file->string
         (string-split _ "\n\n")
         cadr
         parse-parts
         (filter (lambda~> (check-part workflows "in")))
         (map (lambda~>> hash-values (apply +)))
         (apply +))))

(define (solve-p2 fname)
  0)

(module+ test
  (require
    rackunit
    rackunit/text-ui)

  (define input-file "../test_inputs/day_19.txt")
  (define suite
    (test-suite "day 19 tests"
                (test-equal?
                  "part 1 with sample input"
                  (solve-p1 input-file)
                  19114)
                (test-equal?
                  "part 2 with sample input"
                  (solve-p2 input-file)
                  167409079868000)))

  (run-tests suite))

(module* main #f
  (define part (vector-ref (current-command-line-arguments) 0))
  (define input-file "inputs/day_19.txt")
  (match part
    ["1" (solve-p1 input-file)]
    ["2" (solve-p2 input-file)]
    [else (error "specify part 1 or 2")]))


