;;
;; Advent Of Code 2022
;; Day 1 (Part 2)
;;
;; Aleix Conchillo Flaqué <aconchillo@gmail.com>
;;

(define-module (aoc-2022 day-01 part-02)
  #:use-module (ice-9 textual-ports))

(define (solve port)
  (let loop ((calories '())
             (cur-calories 0)
             (line (get-line port)))
    (cond
     ((eof-object? line) (apply + (list-head (sort calories >) 3)))
     ((string-null? line) (loop (cons cur-calories calories) 0 (get-line port)))
     (else (loop calories (+ cur-calories (string->number line)) (get-line port))))))
