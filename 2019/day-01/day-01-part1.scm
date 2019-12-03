#!/bin/sh
# -*- scheme -*-
exec guile -l $0 -c "(apply main (cdr (command-line)))" "$@"
!#

;;
;; Advent Of Code 2019
;; Day 1
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

(define (fuel masses)
  (fold + 0 (map (lambda (m) (- (floor (/ m 3)) 2)) masses)))

(define (main . args)
  (pmatch args
    ((,file-name)
     (let ((masses (call-with-input-file file-name load-masses)))
       (display (fuel masses))))
    (else
     (format (current-error-port) "Usage: ./day-01.scm <input-file>~%"))))
