#!/bin/sh
# -*- scheme -*-
exec guile -l $0 -c "(apply main (cdr (command-line)))" "$@"
!#

;;
;; Advent Of Code 2020
;; Day 3 (Part 1)
;;
;; Aleix Conchillo Flaque <aconchillo@gmail.com>
;;

(use-modules (ice-9 textual-ports)
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

(define (traverse-map map start-x start-y)
  (let loop ((x start-x) (y start-y) (count 0))
    (cond
     ((>= y (map-height map)) count)
     (else
      (let* ((new-x (modulo (+ x 3) (map-width map)))
             (new-y (+ y 1))
             (is-tree (map-pos-is-tree? map new-x new-y)))
        (loop new-x new-y (+ count (if is-tree 1 0))))))))

(define (main . args)
  (pmatch args
    ((,file-name)
     (let ((map (call-with-input-file file-name load-map)))
       (display (traverse-map map 0 0))))
    (else
     (format (current-error-port) "Usage: ./day-03-part1.scm <input-file>~%"))))
