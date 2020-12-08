#!/bin/sh
# -*- scheme -*-
exec guile -l $0 -c "(apply main (cdr (command-line)))" "$@"
!#

;;
;; Advent Of Code 2020
;; Day 2 (Part 2)
;;
;; Aleix Conchillo Flaque <aconchillo@gmail.com>
;;

(use-modules (ice-9 textual-ports)
             (srfi srfi-1)
             (srfi srfi-9)
             (system base pmatch))

(define-record-type <password-rule>
  (make-password-rule char pos1 pos2)
  password-rule?
  (char password-rule-char)
  (pos1 password-rule-pos1)
  (pos2 password-rule-pos2))

(define (parse-password-line line)
  (let* ((parts (string-split line #\:))
         (part-rule (string-split (car parts) #\sp))
         (part-password (string-trim (cadr parts)))
         (part-pos (string-split (car part-rule) #\-))
         (part-char (string-ref (cadr part-rule) 0)))
    (cons (make-password-rule part-char
                              (string->number (car part-pos))
                              (string->number (cadr part-pos)))
          part-password)))

(define (xor a b)
  (or (and a (not b)) (and (not a) b)))

(define (validate-password rule password)
  (let ((char (password-rule-char rule))
        (pos1 (- (password-rule-pos1 rule) 1))
        (pos2 (- (password-rule-pos2 rule) 1)))
    (xor (char=? char (string-ref password pos1))
         (char=? char (string-ref password pos2)))))

(define (count-valid-passwords passwords)
  (count (lambda (elem) (validate-password (car elem) (cdr elem)))
         passwords))

(define (load-passwords port)
  (let loop ((entries '())
             (line (get-line port)))
    (cond
     ((eof-object? line) (reverse entries))
     (else (loop (cons (parse-password-line line) entries)
                 (get-line port))))))

(define (main . args)
  (pmatch args
    ((,file-name)
     (let ((passwords (call-with-input-file file-name load-passwords)))
       (display (count-valid-passwords passwords))))
    (else
     (format (current-error-port) "Usage: ./day-02-part2.scm <input-file>~%"))))
