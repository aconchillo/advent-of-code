#!/bin/sh
# -*- scheme -*-
exec guile -l $0 -c "(apply main (cdr (command-line)))" "$@"
!#

;;
;; Advent Of Code 2019
;; Day 2 (Part 1)
;;
;; Aleix Conchillo Flaque <aconchillo@gmail.com>
;;

(use-modules (ice-9 textual-ports)
             (srfi srfi-1)
             (system base pmatch))

(define (load-program port)
  (list->vector (map string->number (string-split (get-line port) #\,))))

(define (run-program program)
  (define (exec-op f pc)
    (let ((in1 (vector-ref program (+ pc 1)))
          (in2 (vector-ref program (+ pc 2)))
          (out (vector-ref program (+ pc 3))))
      (vector-set! program out (f (vector-ref program in1)
                                  (vector-ref program in2)))))
  (let loop ((pc 0))
    (let ((op (vector-ref program pc)))
      (cond
       ((eqv? op 1) (exec-op + pc) (loop (+ pc 4)))
       ((eqv? op 2) (exec-op * pc) (loop (+ pc 4)))
       ((eqv? op 99) (vector-ref program 0))))))

(define (main . args)
  (pmatch args
    ((,file-name)
     (let ((program (call-with-input-file file-name load-program)))
       (vector-set! program 1 12)
       (vector-set! program 2 2)
       (display (run-program program))))
    (else
     (format (current-error-port) "Usage: ./day-02-part1.scm <input-file>~%"))))
