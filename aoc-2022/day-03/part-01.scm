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

(define (find-wrong-item-priority line)
  (let* ((rucksack (string->list line))
         (mid (/ (length rucksack) 2)))
    (let loop ((i 0)
               (items rucksack)
               (h (make-hash-table)))
      (cond
       ((< i mid)
        (hash-set! h (car items) #t)
        (loop (1+ i) (cdr items) h))
       (else
        (if (hashq-ref h (car items))
            (item-priority (car items))
            (loop (1+ i) (cdr items) h)))))))

(define (solve port)
  (let loop ((sum 0) (line (get-line port)))
    (cond
     ((eof-object? line) sum)
     (else (loop (+ sum (find-wrong-item-priority line)) (get-line port))))))
