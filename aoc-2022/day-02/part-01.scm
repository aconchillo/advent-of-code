;;
;; Advent Of Code 2022
;; Day 2 (Part 1)
;;
;; Aleix Conchillo Flaqué <aconchillo@gmail.com>
;;

(define-module (aoc-2022 day-02 part-01)
  #:use-module (ice-9 match)
  #:use-module (ice-9 textual-ports)
  #:export (solve))

(define (shape-score shape)
  (assoc-ref
   '(("A" . 1)
     ("X" . 1)
     ("B" . 2)
     ("Y" . 2)
     ("C" . 3)
     ("Z" . 3))
   shape))

(define (round-score shapes)
  (match shapes
    ;; Won (rock-paper, paper-scissors, scissors-rock)
    ((or ("A" "Y") ("B" "Z") ("C" "X")) 6)
    ;; Draw
    ((or ("A" "X") ("B" "Y") ("C" "Z"))  3)
    ;; Lost (rock-scissors, paper-rock, scissors-paper)
    ((or ("A" "Z") ("B" "X") ("C" "Y")) 0)))

(define (game-score shapes)
  (+ (shape-score (cadr shapes)) (round-score shapes)))

(define (solve port)
  (let loop ((score 0) (line (get-line port)))
    (cond
     ((eof-object? line) score)
     (else (loop (+ score (game-score (string-split line #\sp))) (get-line port))))))
