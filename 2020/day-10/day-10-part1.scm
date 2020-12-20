#!/bin/sh
# -*- scheme -*-
exec guile -l $0 -c "(apply main (cdr (command-line)))" "$@"
!#

;;
;; Advent Of Code 2020
;; Day 10 (Part 1)
;;
;; Aleix Conchillo Flaque <aconchillo@gmail.com>
;;

(use-modules (ice-9 match)
             (ice-9 regex)
             (ice-9 textual-ports)
             (srfi srfi-1)
             (system base pmatch))

(define (load-sorted-adapters port)
  (let loop ((adapters '())
             (line (get-line port)))
    (cond
     ((eof-object? line) (sort adapters <))
     (else
      (loop (cons (string->number line) adapters) (get-line port))))))

(define (find-joltage-diff adapters)
  (let ((v-adapters (list->vector adapters)))
    (let loop ((i 0) (prev-jolt 0) (diff-1 0) (diff-3 0))
      (cond
       ((>= i (vector-length v-adapters)) (* diff-1 (+ diff-3 1)))
       (else
        (let* ((joltage (vector-ref v-adapters i))
               (diff (- joltage prev-jolt)))
          (cond
           ((eq? diff 1) (loop (+ i 1) joltage (+ diff-1 1) diff-3))
           ((eq? diff 3) (loop (+ i 1) joltage diff-1 (+ diff-3 1))))))))))

(define (main . args)
  (pmatch args
    ((,file-name)
     (let ((adapters (call-with-input-file file-name load-sorted-adapters)))
       (display (find-joltage-diff adapters))))
    (else
     (format (current-error-port) "Usage: ./day-10-part1.scm <input-file>~%"))))
