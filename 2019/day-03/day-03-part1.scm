#!/bin/sh
# -*- scheme -*-
exec guile -l $0 -c "(apply main (cdr (command-line)))" "$@"
!#

;;
;; Advent Of Code 2019
;; Day 3 (Part 1)
;;
;; Aleix Conchillo Flaque <aconchillo@gmail.com>
;;

(use-modules (ice-9 receive)
             (ice-9 textual-ports)
             (srfi srfi-1)
             (system base pmatch))

(define (load-paths port)
  (list (string-split (get-line port) #\,)
        (string-split (get-line port) #\,)))

;; Return new step considering direction and position.
(define (next-step direction x y)
  (cond
   ((eqv? direction #\R) (values (+ x 1) y))
   ((eqv? direction #\L) (values (- x 1) y))
   ((eqv? direction #\U) (values x (- y 1)))
   ((eqv? direction #\D) (values x (+ y 1)))))

;; Compute all steps generated starting at the current step and following
;; the indication from a given path.
(define (compute-steps indication steps init-x init-y)
  (let loop ((direction (string-ref indication 0))
             (count (string->number (substring indication 1)))
             (x init-x)
             (y init-y))
    ;; No need to add (0,0)
    (if (or (not (zero? x)) (not (zero? y)))
        (hash-set! steps (cons x y) #t))
    (cond
     ((zero? count) (values x y))
     (else
      ;; Get new step coordinate and loop starting there.
      (receive (new-x new-y)
          (next-step direction x y)
        (loop direction (- count 1) new-x new-y))))))

;; Converts a path (list of steps) to a hash table with all the traversed
;; steps in the map.
(define (path->steps-hash path)
  (let loop ((indications path)
             (steps (make-hash-table))
             (x 0) (y 0))
    (cond
     ((null? indications) steps)
     (else
      (receive (new-x new-y)
          (compute-steps (car indications) steps x y)
        (loop (cdr indications) steps new-x new-y))))))

;; Returns a list of all intersections.
(define (step-intersections steps-0 steps-1)
  (hash-fold (lambda (k v intersection)
               (if (hash-ref steps-1 k)
                   (append intersection (list k))
                   intersection))
             '() steps-0))

(define (manhattan-distance step)
  (+ (abs (car step)) (abs (cdr step))))

(define (main . args)
  (pmatch args
    ((,file-name)
     (let* ((paths (call-with-input-file file-name load-paths))
            (steps-0 (path->steps-hash (first paths)))
            (steps-1 (path->steps-hash (second paths)))
            (intersections (step-intersections steps-0 steps-1))
            (distances (map manhattan-distance intersections)))
       (display (apply min distances))))
    (else
     (format (current-error-port) "Usage: ./day-03-part1.scm <input-file>~%"))))
