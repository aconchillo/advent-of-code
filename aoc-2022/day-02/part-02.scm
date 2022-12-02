;;
;; Advent Of Code 2022
;; Day 2 (Part 2)
;;
;; Aleix Conchillo Flaqué <aconchillo@gmail.com>
;;

(define-module (aoc-2022 day-02 part-02)
  #:use-module (ice-9 match)
  #:use-module (ice-9 textual-ports)
  #:export (solve))

(define (shape-score shape)
  (assoc-ref
   '(("A" . 1)
     ("B" . 2)
     ("C" . 3))
   shape))

(define (round-score end-game)
  (assoc-ref
   '(("X" . 0)
     ("Y" . 3)
     ("Z" . 6))
   end-game))

(define (find-my-shape opponent-shape end-game)
  (assoc-ref
   (assoc-ref
    '(("A" . (("X" . "C")
              ("Y" . "A")
              ("Z" . "B")))
      ("B" . (("X" . "A")
              ("Y" . "B")
              ("Z" . "C")))
      ("C" . (("X" . "B")
              ("Y" . "C")
              ("Z" . "A"))))
    opponent-shape)
   end-game))

(define (game-score shapes)
  (let* ((opponent-shape (car shapes))
         (end-game (cadr shapes))
         (my-shape (find-my-shape opponent-shape end-game)))
    (+ (shape-score my-shape) (round-score end-game))))

(define (solve port)
  (let loop ((score 0) (line (get-line port)))
    (cond
     ((eof-object? line) score)
     (else (loop (+ score (game-score (string-split line #\sp))) (get-line port))))))
