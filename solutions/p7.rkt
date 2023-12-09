#lang racket

(require
  "../lib/helpers.rkt"
  threading
  racket/file
  racket/trace)

(struct hand (cards bid))

(define (parse-hand translation line)
  (let* ([parts (string-split line)]
         [bid (string->number (cadr parts))])
    (hand (~> parts
               car
               (translate "TJQKA" translation))
          bid)))

(define (get-matches hand)
  (let* ([counts (~>> hand hand-cards string-counter)]
         [wildcards (hash-pop counts #\0 0)]
         [matches (~>> counts
                      hash-values
                      (sort _ >))]
         [matches (if (null? matches) '(0) matches)])
    (cons (+ (car matches) wildcards)
          (cdr matches))))

(define (score hand)
  (let* ([matches (get-matches hand)])
    (match matches
      ['(5) 10]
      ['(4 1) 9]
      ['(3 2) 8]
      ['(3 1 1) 7]
      ['(2 2 1) 6]
      ['(2 1 1 1) 5]
      ['(1 1 1 1 1) 0])))

(define (tiebreak-lt hand-a hand-b)
  (if (equal? (car hand-a) (car hand-b))
    (tiebreak-lt (cdr hand-a) (cdr hand-b))
    (char<? (car hand-a) (car hand-b))))

(define (hand-lt a b)
  (let ([a-score (score a)]
        [b-score (score b)])
    (cond [(< a-score b-score) #t]
          [(> a-score b-score) #f]
          [else (tiebreak-lt (string->list (hand-cards a))
                             (string->list (hand-cards b)))])))

(define (calculate-winnings hands)
  (define (iter hands rank)
    (if (null? hands)
      '()
      (cons (* rank (hand-bid (car hands)))
            (iter (cdr hands) (add1 rank)))))
  (iter hands 1))

(define (solve-p1 fname)
  (~>> fname
       file->lines
       (map (lambda~>> (parse-hand "BCDEF")))
       (sort _ hand-lt)
       calculate-winnings
       (apply +)))

(define (solve-p2 fname)
  (~>> fname
       file->lines
       (map (lambda~>> (parse-hand "B0DEF")))
       (sort _ hand-lt)
       calculate-winnings
       (apply +)))

(module+ test
  (require
    rackunit
    rackunit/text-ui)

  (define input-file "../test_inputs/day_7.txt")
  (define suite
    (test-suite "day 7 tests"
                (test-equal?
                  "part 1 with sample input"
                  (solve-p1 input-file)
                  6440)
                (test-equal?
                  "part 2 with sample input"
                  (solve-p2 input-file)
                  5905)))

  (run-tests suite))

(module* main #f
  (define part (vector-ref (current-command-line-arguments) 0))
  (define input-file "inputs/day_7.txt")
  (match part
    ["1" (solve-p1 input-file)]
    ["2" (solve-p2 input-file)]
    [else (error "specify part 1 or 2")]))


