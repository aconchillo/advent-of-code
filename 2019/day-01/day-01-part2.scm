#!/bin/sh
# -*- scheme -*-
exec guile -l $0 -c "(apply main (cdr (command-line)))" "$@"
!#

;;
;; Advent Of Code 2019
;; Day 1 (Part 2)
;;
;; Aleix Conchillo Flaque <aconchillo@gmail.com>
;;

(use-modules (ice-9 textual-ports)
             (srfi srfi-1)
             (system base pmatch))

(define (load-masses port)
  (let loop ((masses '())
             (line (get-line port)))
    (cond
     ((eof-object? line) masses)
     (else (loop (append masses (list (string->number line)))
                 (get-line port))))))

(define (compute-fuel value)
  (- (floor (/ value 3)) 2))

(define (fuel->fuel fuel)
  (let loop ((total 0) (value fuel))
    (cond
     ((< value 0) total)
     (else (loop (+ total value) (compute-fuel value))))))

(define (mass->fuel mass)
  (fuel->fuel (compute-fuel mass)))

(define (fuel masses)
  (fold + 0 (map mass->fuel masses)))

(define (main . args)
  (pmatch args
    ((,file-name)
     (let ((masses (call-with-input-file file-name load-masses)))
       (display (fuel masses))))
    (else
     (format (current-error-port) "Usage: ./day-01-part2.scm <input-file>~%"))))
