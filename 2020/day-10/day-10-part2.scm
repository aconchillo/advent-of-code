#!/bin/sh
# -*- scheme -*-
exec guile -l $0 -c "(apply main (cdr (command-line)))" "$@"
!#

;;
;; Advent Of Code 2020
;; Day 10 (Part 2)
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

(define (vector-last v)
  (vector-ref v (- (vector-length v) 1)))

(define (counters-ref counters k)
  (if (< k 0) 0 (vector-ref counters k)))

;; Dynamic Programming. The complexity is O(n). Unfortunately, I completely
;; missed it was DP and had to look for the approach to use.
;;
;; The idea is simple: we don't really care about the combinations per se, we
;; just want to find the total amount of combinations. The way to do this is by
;; asking "how many ways to get to adapter N?" considering that we can get to
;; each adapter only by previous adapters that have 1, 2 or 3 joltages
;; difference.
(define (count-arrangements adapters)
  (let* ((max-adapter (apply max adapters))
         (v-adapters (list->vector (append adapters (list (+ max-adapter 3)))))
         (counters (make-vector (+ (vector-last v-adapters) 1) 0)))
    (vector-set! counters 0 1)
    (let loop ((i 0))
      (cond
       ((>= i (vector-length v-adapters)) (vector-last counters))
       (else
        (let* ((a (vector-ref v-adapters i))
               (counter-1 (- a 1)) (counter-2 (- a 2)) (counter-3 (- a 3)))
          (vector-set! counters a (+ (counters-ref counters counter-1)
                                     (counters-ref counters counter-2)
                                     (counters-ref counters counter-3)))
          (loop (+ i 1))))))))

(define (main . args)
  (pmatch args
    ((,file-name)
     (let ((adapters (call-with-input-file file-name load-sorted-adapters)))
       (display (count-arrangements adapters))))
    (else
     (format (current-error-port) "Usage: ./day-10-part2.scm <input-file>~%"))))
