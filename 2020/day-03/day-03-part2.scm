#!/bin/sh
# -*- scheme -*-
exec guile -l $0 -c "(apply main (cdr (command-line)))" "$@"
!#

;;
;; Advent Of Code 2020
;; Day 3 (Part 2)
;;
;; Aleix Conchillo Flaque <aconchillo@gmail.com>
;;

(use-modules (ice-9 textual-ports)
             (srfi srfi-1)
             (system base pmatch))

(define (load-map port)
  (let loop ((rows '())
             (line (get-line port)))
    (cond
     ((eof-object? line)
      (list->array 2 (reverse rows)))
     (else
      (loop (cons (string->list line) rows) (get-line port))))))

(define (map-width map)
  (cadr (array-dimensions map)))

(define (map-height map)
  (car (array-dimensions map)))

(define (map-pos-is-tree? map x y)
  (and (< x (map-width map))
       (< y (map-height map))
       (char=? (array-ref map y x) #\#)))

(define (traverse-map map slope-x slope-y)
  (let loop ((x 0) (y 0) (count 0))
    (cond
     ((>= y (map-height map)) count)
     (else
      (let* ((new-x (modulo (+ x slope-x) (map-width map)))
             (new-y (+ y slope-y))
             (is-tree (map-pos-is-tree? map new-x new-y)))
        (loop new-x new-y (+ count (if is-tree 1 0))))))))

(define (traverse-map-with-slopes map slopes)
  (fold (lambda (elem prev)
          (* prev (traverse-map map (car elem) (cadr elem))))
        1 slopes))

(define (main . args)
  (pmatch args
    ((,file-name)
     (let ((map (call-with-input-file file-name load-map)))
       (display (traverse-map-with-slopes map '((1 1) (3 1) (5 1) (7 1) (1 2))))))
    (else
     (format (current-error-port) "Usage: ./day-03-part1.scm <input-file>~%"))))
