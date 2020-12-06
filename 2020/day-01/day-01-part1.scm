#!/bin/sh
# -*- scheme -*-
exec guile -l $0 -c "(apply main (cdr (command-line)))" "$@"
!#

;;
;; Advent Of Code 2020
;; Day 1 (Part 1)
;;
;; Aleix Conchillo Flaque <aconchillo@gmail.com>
;;

(use-modules (ice-9 textual-ports)
             (system base pmatch))

(define (load-entries port)
  (let loop ((entries '())
             (line (get-line port)))
    (cond
     ((eof-object? line) (reverse entries))
     (else (loop (cons (string->number line) entries)
                 (get-line port))))))

;; This is a well-known solution. You basically create an array ARR that
;; contains ARR[b] = a where b = 2020 - a and a is the number we are currently
;; evaluating from the given list.
;;
;; This has an O(n) complexity.
(define (fix-report entries total)
  (let ((sums (make-vector (+ total 1) -1)))
    (let loop ((values entries))
      (let* ((a (car values)) (b (- total a)))
        (cond
         ((not (eqv? (vector-ref sums a) -1))
          (list a b))
         (else
          (vector-set! sums b a)
          (loop (cdr values))))))))

(define (main . args)
  (pmatch args
    ((,file-name)
     (let ((entries (call-with-input-file file-name load-entries)))
       (display (apply * (fix-report entries 2020)))))
    (else
     (format (current-error-port) "Usage: ./day-01-part1.scm <input-file>~%"))))
