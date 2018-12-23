#!/bin/sh
# -*- scheme -*-
exec guile -l $0 -c "(apply main (cdr (command-line)))" "$@"
!#

;;
;; Advent Of Code 2018
;; Day 1 (Part 2)
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

(define (process-changes changes rem-changes frequency freq-seen)
  (hashv-set! freq-seen frequency #t)
  (cond
   ((null? rem-changes)
    (process-changes changes changes frequency freq-seen))
   (else
    (let ((new-freq (+ frequency (car rem-changes))))
      (cond
       ((hashv-ref freq-seen new-freq) new-freq)
       (else
        (process-changes changes (cdr rem-changes) new-freq freq-seen)))))))

(define (part2 changes)
  (let ((freq-seen (make-hash-table)))
    (process-changes changes changes 0 freq-seen)))

(define (main . args)
  (pmatch args
    ((,file-name)
     (let ((changes (call-with-input-file file-name load-changes)))
       (display (part2 changes))))
    (else
     (format (current-error-port) "Usage: ./day-01-part2 <input-file>~%"))))
