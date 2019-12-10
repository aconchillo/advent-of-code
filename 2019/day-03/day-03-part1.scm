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

;; Return new point considering direction and position.
(define (next-point direction x y)
  (cond
   ((eqv? direction #\R) (values (+ x 1) y))
   ((eqv? direction #\L) (values (- x 1) y))
   ((eqv? direction #\U) (values x (- y 1)))
   ((eqv? direction #\D) (values x (+ y 1)))))

;; Compute all points generated starting at the current point and following
;; the step from a given path.
(define (compute-step-points step points init-x init-y)
  (let loop ((direction (string-ref step 0))
             (count (string->number (substring step 1)))
             (x init-x)
             (y init-y))
    ;; No need to add (0,0)
    (if (or (not (zero? x)) (not (zero? y)))
        (hash-set! points (cons x y) #t))
    (cond
     ((zero? count) (values x y))
     (else
      ;; Get new point coordinate and loop starting there.
      (receive (new-x new-y)
          (next-point direction x y)
        (loop direction (- count 1) new-x new-y))))))

;; Converts a path (list of steps) to a hash table with all the traversed
;; points in the map.
(define (path->points-hash path)
  (let loop ((steps path)
             (points (make-hash-table))
             (x 0) (y 0))
    (cond
     ((null? steps) points)
     (else
      (receive (new-x new-y)
          (compute-step-points (car steps) points x y)
        (loop (cdr steps) points new-x new-y))))))

;; Returns a list of all intersection points.
(define (points-intersection points-0 points-1)
  (hash-fold (lambda (k v intersection)
               (if (hash-ref points-1 k) (append intersection (list k)) intersection))
             '() points-0))

(define (manhattan-distance point)
  (+ (abs (car point)) (abs (cdr point))))

(define (main . args)
  (pmatch args
    ((,file-name)
     (let* ((paths (call-with-input-file file-name load-paths))
            (points-0 (path->points-hash (first paths)))
            (points-1 (path->points-hash (second paths)))
            (intersection (points-intersection points-0 points-1))
            (distances (map manhattan-distance intersection)))
       (display (apply min distances))))
    (else
     (format (current-error-port) "Usage: ./day-03-part1.scm <input-file>~%"))))
