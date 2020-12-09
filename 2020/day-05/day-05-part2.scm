#!/bin/sh
# -*- scheme -*-
exec guile -l $0 -c "(apply main (cdr (command-line)))" "$@"
!#

;;
;; Advent Of Code 2020
;; Day 5 (Part 2)
;;
;; Aleix Conchillo Flaque <aconchillo@gmail.com>
;;

(use-modules (ice-9 regex)
             (ice-9 textual-ports)
             (srfi srfi-1)
             (srfi srfi-43)
             (system base pmatch))

(define (load-passes port)
  (let loop ((lines '())
             (line (get-line port)))
    (cond
     ((eof-object? line) lines)
     (else (loop (cons line lines) (get-line port))))))

(define (compute-range code lower-char upper-char max)
  (fold (lambda (elem prev)
          (let* ((a (car prev)) (b (cdr prev)) (mid (/ (+ a b) 2)))
            (cond
             ((eqv? elem lower-char) (cons a (floor mid)))
             ((eqv? elem upper-char) (cons (ceiling mid) b)))))
        (cons 0 max) (string->list code)))

(define (compute-row code)
  (car (compute-range code #\F #\B 127)))

(define (compute-column code)
  (cdr (compute-range code #\L #\R 7)))

(define (compute-seat pass)
  (let ((m (string-match "([FB]{7})([LR]{3})" pass)))
    (cons (compute-row (match:substring m 1))
          (compute-column (match:substring m 2)))))

(define (compute-seat-id pass)
  (let ((seat (compute-seat pass)))
    (+ (* (car seat) 8) (cdr seat))))

;; We could (sort) and find a gap. Sorting it's usually O(nlogn), so since
;; seat ids are sequential it's probably better to just create a vector and
;; then look for the missing element.
;;
;; This has an O(n) complexity.
(define (find-my-seat-id passes)
  (let* ((seat-ids (map compute-seat-id passes))
         (min-seat-id (apply min seat-ids))
         (seats (make-vector (+ (length seat-ids) 1)  #f)))
    (for-each
     (lambda (elem)
       ;; Front seats don't exist that's why we substract the minimum seat
       ;; identifier as that will be our index 0 in the vector.
       (vector-set! seats (- elem min-seat-id) #t))
     seat-ids)
    ;; Find the index that is false (not) and add the minimum seat id.
    (+ (vector-index not seats) min-seat-id)))

(define (main . args)
  (pmatch args
    ((,file-name)
     (let ((passes (call-with-input-file file-name load-passes)))
       (display (find-my-seat-id passes))))
    (else
     (format (current-error-port) "Usage: ./day-05-part2.scm <input-file>~%"))))
