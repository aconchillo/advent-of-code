#!/bin/sh
# -*- scheme -*-
exec guile -l $0 -c "(apply main (cdr (command-line)))" "$@"
!#

;;
;; Advent Of Code 2020
;; Day 8 (Part 1)
;;
;; Aleix Conchillo Flaque <aconchillo@gmail.com>
;;

(use-modules (ice-9 match)
             (ice-9 regex)
             (ice-9 textual-ports)
             (srfi srfi-1)
             (system base pmatch))

(define (load-program port)
  (define (parse-instruction line)
    (let* ((m (string-match "(acc|jmp|nop) ([\\+\\-][0-9]+)" line))
           (op-code (string->symbol (match:substring m 1)))
           (argument (string->number (match:substring m 2))))
      (cons op-code argument)))

  (let loop ((program '())
             (line (get-line port)))
    (cond
     ((eof-object? line) (list->vector (reverse program)))
     (else
      (loop (cons (parse-instruction line) program) (get-line port))))))

(define (run-program program)
  (let ((execution (make-vector (vector-length program) #f)))
    (let loop ((pc 0) (acc 0))
      (cond
       ((>= pc (vector-length program)) acc)
       ;; Return accumulator as soon as we try to run an instruction a second
       ;; time.
       ((vector-ref execution pc) acc)
       (else
        (vector-set! execution pc #t)
        (let* ((inst (vector-ref program pc)) (op-code (car inst)) (argument (cdr inst)))
          (match op-code
            ('acc (loop (+ pc 1) (+ acc argument)))
            ('jmp (loop (+ pc argument) acc))
            ('nop (loop (+ pc 1) acc)))))))))

(define (main . args)
  (pmatch args
    ((,file-name)
     (let ((program (call-with-input-file file-name load-program)))
       (display (run-program program))))
    (else
     (format (current-error-port) "Usage: ./day-08-part1.scm <input-file>~%"))))
