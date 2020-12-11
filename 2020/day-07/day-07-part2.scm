#!/bin/sh
# -*- scheme -*-
exec guile -l $0 -c "(apply main (cdr (command-line)))" "$@"
!#

;;
;; Advent Of Code 2020
;; Day 7 (Part 2)
;;
;; Aleix Conchillo Flaque <aconchillo@gmail.com>
;;

(use-modules (ice-9 match)
             (ice-9 regex)
             (ice-9 textual-ports)
             (srfi srfi-1)
             (system base pmatch))

(define (load-rules port)
  ;; Returns a pair (bag-name count) where count is the number of bags of that
  ;; type.
  (define (parse-contains rule)
    (match rule
      ("no other bags." '())
      (_
       (let ((all-bags (string-split rule #\,)))
         (map
          (lambda (elem)
            (let ((m (string-match "([0-9]+) (\\w+ \\w+) bags?\\.?" elem)))
              (cons (match:substring m 2)
                    (string->number (match:substring m 1)))))
          all-bags)))))

  ;; Returns a pair (bag-name contains) where contains is a list of (count
  ;; bag-name).
  (define (parse-rule rule)
    (let* ((m (string-match "(\\w+ \\w+) bags contain (.+)" rule))
           (bag (match:substring m 1))
           (contains (parse-contains (match:substring m 2))))
      (cons bag contains)))

  (define (parse-rules lines)
    (map parse-rule lines))

  (let loop ((lines '())
             (line (get-line port)))
    (cond
     ((eof-object? line) (parse-rules lines))
     (else
      (loop (cons line lines) (get-line port))))))

(define (find-container rules name)
  (assoc-ref rules name))

(define (count-container-bags rules init-name)
  (fold (lambda (c prev)
          (let ((name (car c)) (count (cdr c)))
            (+ prev count (* count (count-container-bags rules name)))))
        0 (find-container rules init-name)))

(define (main . args)
  (pmatch args
    ((,file-name)
     (let ((rules (call-with-input-file file-name load-rules)))
       (display (count-container-bags rules "shiny gold"))))
    (else
     (format (current-error-port) "Usage: ./day-07-part2.scm <input-file>~%"))))
