;;
;; Advent Of Code 2022
;; Day 3 (Part 2)
;;
;; Aleix Conchillo Flaqué <aconchillo@gmail.com>
;;

(define-module (aoc-2022 day-03 part-02)
  #:use-module (ice-9 match)
  #:use-module (ice-9 textual-ports)
  #:use-module (srfi srfi-1)
  #:export (solve))

(define (item-priority item)
  (- (char->integer item) (if (char-lower-case? item) 96 38)))


(define (find-group-badge-priority group)
  (item-priority (car (apply lset-intersection eq? group))))

(define (solve port)
  (let loop ((sum 0))
    (cond
     ((eof-object? (peek-char port)) sum)
     (else
      (let ((group (list (string->list (get-line port))
                         (string->list (get-line port))
                         (string->list (get-line port)))))
        (loop (+ sum (find-group-badge-priority group))))))))
