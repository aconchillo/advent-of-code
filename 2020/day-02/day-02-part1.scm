#!/bin/sh
# -*- scheme -*-
exec guile -l $0 -c "(apply main (cdr (command-line)))" "$@"
!#

;;
;; Advent Of Code 2020
;; Day 2 (Part 1)
;;
;; Aleix Conchillo Flaque <aconchillo@gmail.com>
;;

(use-modules (ice-9 textual-ports)
             (srfi srfi-1)
             (srfi srfi-9)
             (system base pmatch))

(define-record-type <password-rule>
  (make-password-rule char min max)
  password-rule?
  (char password-rule-char)
  (min password-rule-min)
  (max password-rule-max))

(define (parse-password-line line)
  (let* ((parts (string-split line #\:))
         (part-rule (string-split (car parts) #\sp))
         (part-password (string-trim (cadr parts)))
         (part-min-max (string-split (car part-rule) #\-))
         (part-char (string-ref (cadr part-rule) 0)))
    (cons (make-password-rule part-char
                              (string->number (car part-min-max))
                              (string->number (cadr part-min-max)))
          part-password)))

(define (string-count-char str c)
  (count (lambda (elem) (char=? elem c))
         (string->list str)))

(define (validate-password rule password)
  (let ((count (string-count-char password (password-rule-char rule))))
    (and (>= count (password-rule-min rule))
         (<= count (password-rule-max rule)))))

(define (count-valid-passwords passwords)
  (count (lambda (elem) (validate-password (car elem) (cdr elem)))
         passwords))

(define (load-passwords port)
  (let loop ((entries '())
             (line (get-line port)))
    (cond
     ((eof-object? line) entries)
     (else (loop (cons (parse-password-line line) entries)
                 (get-line port))))))

(define (main . args)
  (pmatch args
    ((,file-name)
     (let ((passwords (call-with-input-file file-name load-passwords)))
       (display (count-valid-passwords passwords))))
    (else
     (format (current-error-port) "Usage: ./day-02-part1.scm <input-file>~%"))))
