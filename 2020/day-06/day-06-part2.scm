#!/bin/sh
# -*- scheme -*-
exec guile -l $0 -c "(apply main (cdr (command-line)))" "$@"
!#

;;
;; Advent Of Code 2020
;; Day 6 (Part 2)
;;
;; Aleix Conchillo Flaque <aconchillo@gmail.com>
;;

(use-modules (ice-9 textual-ports)
             (srfi srfi-1)
             (system base pmatch))

;; We now return a pair (count . questions) where count indicates the number
;; of people in this group.
(define (load-questions port)
  (define (parse-questions lines)
    (let ((single (string-join lines ""))
          ;; We could have used an alist but (hash-ref) has this nice last
          ;; argument to specify a default value if a key is not found.
          (questions (make-hash-table)))
      (for-each (lambda (q)
                  (hash-set! questions q (+ (hash-ref questions q 0) 1)))
                (string->list single))
      (cons (length lines) questions)))
  (let loop ((lines '())
             (line (get-line port)))
    (cond
     ((eof-object? line) (parse-questions lines))
     ((zero? (string-length line)) (parse-questions lines))
     (else
      (loop (cons line lines) (get-line port))))))

(define (load-groups port)
  (let loop ((questions '()))
    (cond
     ((eof-object? (peek-char port)) questions)
     (else
      (loop (cons (load-questions port) questions))))))

;; Could have also used (fold) here but it's a bit more typing.
(define (count-questions groups)
  (apply + (map (lambda (group)
                  ;; For each group (count . questions) find all questions
                  ;; with as many answers as count. This means everyone has
                  ;; answered yes to that question.
                  (hash-count (lambda (k v) (eq? v (car group)))
                              (cdr group)))
                groups)))

(define (main . args)
  (pmatch args
    ((,file-name)
     (let ((groups (call-with-input-file file-name load-groups)))
       (display (count-questions groups))))
    (else
     (format (current-error-port) "Usage: ./day-06-part2.scm <input-file>~%"))))
