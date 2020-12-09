#!/bin/sh
# -*- scheme -*-
exec guile -l $0 -c "(apply main (cdr (command-line)))" "$@"
!#

;;
;; Advent Of Code 2020
;; Day 5 (Part 1)
;;
;; Aleix Conchillo Flaque <aconchillo@gmail.com>
;;

(use-modules (ice-9 regex)
             (ice-9 textual-ports)
             (srfi srfi-1)
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

(define (find-highest-seat-id passes)
  (apply max (map compute-seat-id passes)))

(define (main . args)
  (pmatch args
    ((,file-name)
     (let ((passes (call-with-input-file file-name load-passes)))
       (display (find-highest-seat-id passes))))
    (else
     (format (current-error-port) "Usage: ./day-05-part1.scm <input-file>~%"))))
