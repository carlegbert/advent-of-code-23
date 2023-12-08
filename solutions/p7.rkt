#lang racket

(require
  "../lib/helpers.rkt"
  threading
  racket/file
  racket/trace)

(struct hand (cards bid))

(define (parse-hand line)
  (let* ([parts (string-split line)]
         [bid (string->number (cadr parts))])
    (hand (car parts) bid)))

(define (get-matches hand)
  (~>> hand
       hand-cards
       string->list
       (sort _ char<?)
       (map string)
       (string-join _ "")
       (regexp-match* #px"(.)\\1+")
       (map (lambda~> string->list length))
       (sort _ <)))

(define (score hand)
  (match (get-matches hand)
    ['(5) 10]
    ['(4) 9]
    ['(3 2) 8]
    ['(2 3) 8]
    ['(3) 7]
    ['(2 2) 6]
    ['(2) 5]
    ['() 0]))

(define (card->score card)
  (match card
    [#\1 1]
    [#\2 2]
    [#\3 3]
    [#\4 4]
    [#\5 5]
    [#\6 6]
    [#\7 7]
    [#\8 8]
    [#\9 9]
    [#\T 10]
    [#\J 11]
    [#\Q 12]
    [#\K 13]
    [#\A 14]))

(define (tiebreak-lt hand-a hand-b)
  (let ([a-score (card->score (car hand-a))]
        [b-score (card->score (car hand-b))])
    (if (= a-score b-score)
      (tiebreak-lt (cdr hand-a) (cdr hand-b))
      (< a-score b-score))))

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
       (map parse-hand)
       (sort _ hand-lt)
       (calculate-winnings)
       (apply +)))

(define (solve-p2 fname) 0)

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
                  0)))

  (run-tests suite))

(module* main #f
  (define part (vector-ref (current-command-line-arguments) 0))
  (define input-file "inputs/day_7.txt")
  (match part
    ["1" (solve-p1 input-file)]
    ["2" (solve-p2 input-file)]
    [else (error "specify part 1 or 2")]))


