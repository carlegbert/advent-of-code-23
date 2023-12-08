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

(define (filter-jacks-if-wild wild cards)
  (if wild
    (filter (lambda (card) (not (equal? card #\J))) cards)
    cards))

(define (get-matches hand wild)
  (~>> hand
       hand-cards
       string->list
       (filter-jacks-if-wild wild)
       (sort _ char<?)
       (map string)
       (string-join _ "")
       (regexp-match* #px"(.)\\1+")
       (map (lambda~> string->list length))
       (sort _ <)))

(define (wildcards hand)
  (~>> hand
       hand-cards
       (regexp-match* #rx"J")
       length))

(define (upgrade-hand-strength hand-strength wildcards)
  (cond [(= wildcards 5) '(5)]
        [(= wildcards 0) hand-strength]
        [else (upgrade-hand-strength
                (match hand-strength
                  ['(4) '(5)]
                  ['(3) '(4)]
                  ['(2 2) '(3 2)]
                  ['(2) '(3)]
                  ['() '(2)])
                (sub1 wildcards))]))

(define (score hand wild)
  (let* ([matches (get-matches hand wild)]
        [wildcard-count (if wild (wildcards hand) 0)]
        [matches (upgrade-hand-strength matches wildcard-count)])
    (match matches
      ['(5) 10]
      ['(4) 9]
      ['(3 2) 8]
      ['(2 3) 8]
      ['(3) 7]
      ['(2 2) 6]
      ['(2) 5]
      ['() 0])))

(define (card->score card wild)
  (match card
    [#\J (if wild 0 11)]

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
    [#\Q 12]
    [#\K 13]
    [#\A 14]))

(define (tiebreak-lt hand-a hand-b wild)
  (let ([a-score (card->score (car hand-a) wild)]
        [b-score (card->score (car hand-b) wild)])
    (if (= a-score b-score)
      (tiebreak-lt (cdr hand-a) (cdr hand-b) wild)
      (< a-score b-score))))

(define (hand-lt a b [wild #f])
  (let ([a-score (score a wild)]
        [b-score (score b wild)])
    (cond [(< a-score b-score) #t]
          [(> a-score b-score) #f]
          [else (tiebreak-lt (string->list (hand-cards a))
                             (string->list (hand-cards b))
                             wild)])))

(define (hand-lt-wild a b)
  (hand-lt a b #t))

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

(define (solve-p2 fname)
  (~>> fname
       file->lines
       (map parse-hand)
       (sort _ hand-lt-wild)
       (calculate-winnings)
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


