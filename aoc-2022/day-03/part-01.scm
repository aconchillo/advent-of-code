;;
;; Advent Of Code 2022
;; Day 3 (Part 1)
;;
;; Aleix Conchillo Flaqué <aconchillo@gmail.com>
;;

(define-module (aoc-2022 day-03 part-01)
  #:use-module (ice-9 match)
  #:use-module (ice-9 textual-ports)
  #:use-module (srfi srfi-1)
  #:export (solve))

(define (item-priority item)
  (- (char->integer item) (if (char-lower-case? item) 96 38)))

(define (find-wrong-item-priority rucksack)
  (let* ((mid (/ (length rucksack) 2)))
    (item-priority (car (lset-intersection eq? (take rucksack mid) (take-right rucksack mid))))))

(define (solve port)
  (let loop ((sum 0))
    (cond
     ((eof-object? (peek-char port)) sum)
     (else (loop (+ sum (find-wrong-item-priority (string->list (get-line port)))))))))
