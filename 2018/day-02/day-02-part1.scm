#!/bin/sh
# -*- scheme -*-
exec guile -l $0 -c "(apply main (cdr (command-line)))" "$@"
!#

;;
;; Advent Of Code 2018
;; Day 2 (Part 1)
;;
;; Aleix Conchillo Flaque <aconchillo@gmail.com>
;;

(use-modules (ice-9 textual-ports)
             (srfi srfi-1)
             (srfi srfi-43)
             (system base pmatch))

(define (load-box-ids port)
  (let loop ((box-ids '())
             (line (get-line port)))
    (cond
     ((eof-object? line) box-ids)
     (else (loop (append box-ids (list line)) (get-line port))))))

(define (box-id-char-counter box-id)
  (let ((char-counts (make-vector 26 0)))
    (string-for-each
     (lambda (c)
       (let* ((idx (- (char->integer c) (char->integer #\a)))
              (count (vector-ref char-counts idx)))
         (vector-set! char-counts idx (+ count 1))))
     box-id)
    char-counts))

(define (box-id-with-2? char-counts)
  (vector-any (lambda (v) (eqv? v 2)) char-counts))

(define (box-id-with-3? char-counts)
  (vector-any (lambda (v) (eqv? v 3)) char-counts))

(define (day-02-part1 box-ids)
  (let* ((counters (map box-id-char-counter box-ids))
         (count2 (fold (lambda (v prev) (+ prev (if (box-id-with-2? v) 1 0))) 0 counters))
         (count3 (fold (lambda (v prev) (+ prev (if (box-id-with-3? v) 1 0))) 0 counters)))
    (* count2 count3)))

(define (main . args)
  (pmatch args
    ((,file-name)
     (let ((box-ids (call-with-input-file file-name load-box-ids)))
       (display (day-02-part1 box-ids))))
    (else
     (format (current-error-port) "Usage: ./day-02-part1.scm <input-file>~%"))))
