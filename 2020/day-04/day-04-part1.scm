#!/bin/sh
# -*- scheme -*-
exec guile -l $0 -c "(apply main (cdr (command-line)))" "$@"
!#

;;
;; Advent Of Code 2020
;; Day 4 (Part 1)
;;
;; Aleix Conchillo Flaque <aconchillo@gmail.com>
;;

(use-modules (ice-9 textual-ports)
             (srfi srfi-1)
             (system base pmatch))

(define (load-passport port)
  (define (parse-line lines)
    (let ((single (string-join lines)))
      (map (lambda (entry) (string-split entry #\:))
           (string-split single #\sp))))
  (let loop ((lines '())
             (line (get-line port)))
    (cond
     ((eof-object? line) (parse-line lines))
     ((zero? (string-length line)) (parse-line lines))
     (else (loop (cons line lines) (get-line port))))))

(define (load-passports port)
  (let loop ((passports '()))
    (cond
     ((eof-object? (peek-char port) ) (reverse passports))
     (else
      (loop (cons (load-passport port) passports))))))

(define (is-passport-valid? passport)
  (and (assoc-ref passport "byr")
       (assoc-ref passport "iyr")
       (assoc-ref passport "eyr")
       (assoc-ref passport "hgt")
       (assoc-ref passport "hcl")
       (assoc-ref passport "ecl")
       (assoc-ref passport "pid")))

(define (count-valid-passports passports)
  (count is-passport-valid? passports))

(define (main . args)
  (pmatch args
    ((,file-name)
     (let ((passports (call-with-input-file file-name load-passports)))
       (display (count-valid-passports passports))))
    (else
     (format (current-error-port) "Usage: ./day-04-part1.scm <input-file>~%"))))
