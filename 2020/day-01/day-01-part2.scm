#!/bin/sh
# -*- scheme -*-
exec guile -l $0 -c "(apply main (cdr (command-line)))" "$@"
!#

;;
;; Advent Of Code 2020
;; Day 1 (Part 2)
;;
;; Aleix Conchillo Flaque <aconchillo@gmail.com>
;;

(use-modules (ice-9 textual-ports)
             (system base pmatch))

(define (vector-max v)
  (apply max (vector->list v)))

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
;; This has O(n) complexity.
(define (fix-report entries total skip)
  (let* ((max-value (vector-max entries))
         (sums (make-vector (+ max-value 1) -1)))
    (let loop ((i 0))
      (cond
       ;; We don't have moe entries
       ((eqv? i (vector-length entries)) #f)
       ;; If value is greater than the value we are looking for this is not a
       ;; candidate.
       ((> (vector-ref entries i) total)
        (loop (+ i 1)))
       (else
        (let* ((a (vector-ref entries i)) (b (- total a)))
          (cond
           ;; Skip entry a from a + b + c
           ((eqv? i skip) (loop (+ i 1)))
           ((not (eqv? (vector-ref sums a) -1))
            (list a b))
           (else
            (vector-set! sums b a)
            (loop (+ i 1))))))))))

;; What we do here is a + b + c = 2020 and b + c = 2020 - a. So we consider
;; each entry "a" and then we try to find "2020 - a" re-using
;; (fix-report). The only catch is that in (fix-report) we need to skip the
;; "a" value).
;;
;; Unfortunately this has O(n^2) complexity, there must be a better solution.
(define (fix-report-3 entries total)
  (let loop ((i 0))
    (let* ((value (vector-ref entries i))
           (result (fix-report entries (- total value) i)))
      (if result
          (cons value result)
          (loop (+ i 1))))))

(define (main . args)
  (pmatch args
    ((,file-name)
     (let ((entries (call-with-input-file file-name load-entries)))
       (display (apply * (fix-report-3 (list->vector entries) 2020)))))
    (else
     (format (current-error-port) "Usage: ./day-01-part2.scm <input-file>~%"))))
