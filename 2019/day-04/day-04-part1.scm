#!/bin/sh
# -*- scheme -*-
exec guile -l $0 -c "(apply main (cdr (command-line)))" "$@"
!#

;;
;; Advent Of Code 2019
;; Day 4 (Part 1)
;;
;; Aleix Conchillo Flaque <aconchillo@gmail.com>
;;

(use-modules (srfi srfi-1))

(define (password->digits password)
  (map (compose string->number string) (string->list password)))

(define (increasing-sequence? password)
  (let loop ((digits (password->digits password)) (prev 0))
    (cond
     ((null? digits) #t)
     ((> prev (car digits)) #f)
     (else (loop (cdr digits) (car digits))))))

(define (adjacent-digits? password)
  (let loop ((digits (password->digits password))
             (prev #nil)
             (adj 1))
    (cond
     ((null? digits) (>= adj 2))
     (else
      (loop (cdr digits) (car digits) (if (eqv? (car digits) prev) (+ adj 1) adj))))))

(define (valid-password? password)
  (and (increasing-sequence? password) (adjacent-digits? password)))

(define (possible-passwords start end)
  (map number->string (iota (+ (- end start) 1) start)))

(define (find-passwords start end)
  (filter valid-password? (possible-passwords start end)))

(define (main . args)
  (display (length (find-passwords 123257 647015))))
