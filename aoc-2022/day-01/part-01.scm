;;
;; Advent Of Code 2022
;; Day 1 (Part 1)
;;
;; Aleix Conchillo Flaqué <aconchillo@gmail.com>
;;

(define-module (aoc-2022 day-01 part-01)
  #:use-module (ice-9 textual-ports))

(define (solve port)
  (let loop ((max-calories 0)
             (cur-calories 0)
             (line (get-line port)))
    (cond
      ((eof-object? line) (max max-calories cur-calories))
      ((string-null? line) (loop (max max-calories cur-calories) 0 (get-line port)))
      (else (loop max-calories (+ cur-calories (string->number line)) (get-line port))))))
