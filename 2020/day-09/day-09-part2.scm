#!/bin/sh
# -*- scheme -*-
exec guile -l $0 -c "(apply main (cdr (command-line)))" "$@"
!#

;;
;; Advent Of Code 2020
;; Day 9 (Part 2)
;;
;; Aleix Conchillo Flaque <aconchillo@gmail.com>
;;

(use-modules (ice-9 match)
             (ice-9 regex)
             (ice-9 textual-ports)
             (srfi srfi-43)
             (system base pmatch))

(define (load-numbers port)
  (let loop ((numbers '())
             (line (get-line port)))
    (cond
     ((eof-object? line) (list->vector (reverse numbers)))
     (else
      (loop (cons (string->number line) numbers) (get-line port))))))

;; This is similar to day 1 part 1. In this case since number can be big we
;; use a hash table instead of a vector that holds TABLE[b] = a where TOTAL -
;; a = b.
(define (find-sum numbers start end sum)
  (let ((sums (make-hash-table)))
    (let loop ((i start))
      (cond
       ((> i end) #f)
       ((hash-ref sums (vector-ref numbers i) #f) #t)
       (else
        (let ((value (vector-ref numbers i)))
          (hash-set! sums (- sum value) value)
          (loop (+ i 1))))))))

(define (find-weakness numbers preamble-size)
  (let loop ((i preamble-size))
    (let ((start (- i preamble-size)) (end (- i 1)))
      (cond
       ((>= i (vector-length numbers)) #f)
       ((not (find-sum numbers start end (vector-ref numbers i))) (vector-ref numbers i))
       (else (loop (+ i 1)))))))

(define (find-2nd-weakness numbers start end)
  (let ((values (vector->list numbers start end)))
    (+ (apply min values) (apply max values))))

;; Since the block is contiguous just use two pointers and move them according
;; to the resulting sum.
;;
;; This has an O(n) complexity.
(define (find-contiguous-set numbers sum)
  (let loop ((a 0) (b 1)
             (current (+ (vector-ref numbers 0) (vector-ref numbers 1))))
    (cond
     ((>= b (vector-length numbers)) #f)
     (else
      (let* ((val-a (vector-ref numbers a))
             (val-b (vector-ref numbers b)))
        (cond
         ((eq? current sum) (find-2nd-weakness numbers a b))
         ((> current sum) (loop (+ a 1) (- b 1) (- current val-a val-b)))
         ((< current sum) (loop a (+ b 1) (+ current (vector-ref numbers (+ b 1)))))))))))

(define (main . args)
  (pmatch args
    ((,file-name)
     (let ((numbers (call-with-input-file file-name load-numbers)))
       (display (find-contiguous-set numbers (find-weakness numbers 25)))))
    (else
     (format (current-error-port) "Usage: ./day-09-part2.scm <input-file>~%"))))
