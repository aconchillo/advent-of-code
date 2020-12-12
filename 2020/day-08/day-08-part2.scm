#!/bin/sh
# -*- scheme -*-
exec guile -l $0 -c "(apply main (cdr (command-line)))" "$@"
!#

;;
;; Advent Of Code 2020
;; Day 8 (Part 2)
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

;; This just detects if the program has an infinite loop.
(define (is-infinite-loop? program)
  (let ((execution (make-vector (vector-length program) #f)))
    (let loop ((pc 0))
      (cond
       ((>= pc (vector-length program)) #f)
       ((vector-ref execution pc) #t)
       (else
        (vector-set! execution pc #t)
        (let* ((inst (vector-ref program pc)) (op-code (car inst)) (argument (cdr inst)))
          (match op-code
            ('acc (loop (+ pc 1)))
            ('jmp (loop (+ pc argument)))
            ('nop (loop (+ pc 1))))))))))

;; For each nop and jmp we replace it by jmp and nop respectively and every
;; time we do that we check if the resulting program has an infinite loop.
;;
;; There might be a way to fix this more efficiently probably while running
;; the program but I couldn't come up with any.
(define (fix-program program)
  (let loop ((i 0))
    (let* ((inst (vector-ref program i)) (op-code (car inst)) (argument (cdr inst)))
      (match op-code
        ('acc (loop (+ i 1)))
        ('jmp
         (vector-set! program i (cons 'nop argument))
         (cond
          ((is-infinite-loop? program)
           ;; If it's an infinite loop set back old instruction.
           (vector-set! program i (cons op-code argument))
           (loop (+ i 1)))
          (else program)))
        ('nop
         (vector-set! program i (cons 'jmp argument))
         (cond
          ((is-infinite-loop? program)
           ;; If it's an infinite loop set back old instruction.
           (vector-set! program i (cons op-code argument))
           (loop (+ i 1)))
          (else program)))))))

(define (run-program program)
  (let ((execution (make-vector (vector-length program) #f)))
    (let loop ((pc 0) (acc 0))
      (cond
       ((>= pc (vector-length program)) acc)
       ;; There shouldn't be any loops.
       ((vector-ref execution pc) (throw 'loop-error))
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
       (display (run-program (fix-program program)))))
    (else
     (format (current-error-port) "Usage: ./day-08-part2.scm <input-file>~%"))))
