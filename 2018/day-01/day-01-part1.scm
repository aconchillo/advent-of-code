#!/bin/sh
# -*- scheme -*-
exec guile -l $0 -c "(apply main (cdr (command-line)))" "$@"
!#

;;
;; Advent Of Code 2018
;; Day 1 (Part 1)
;;
;; Aleix Conchillo Flaque <aconchillo@gmail.com>
;;

(use-modules (ice-9 textual-ports)
             (srfi srfi-1)
             (system base pmatch))

(define (load-changes port)
  (let loop ((changes '())
             (line (get-line port)))
    (cond
     ((eof-object? line) changes)
     (else (loop (append changes (list (string->number line)))
                 (get-line port))))))

(define (part1 changes)
  (fold + 0 changes))

(define (main . args)
  (pmatch args
    ((,file-name)
     (let ((changes (call-with-input-file file-name load-changes)))
       (display (part1 changes))))
    (else
     (format (current-error-port) "Usage: ./day-01-part1 <input-file>~%"))))
