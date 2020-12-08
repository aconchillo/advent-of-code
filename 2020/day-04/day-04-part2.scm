#!/bin/sh
# -*- scheme -*-
exec guile -l $0 -c "(apply main (cdr (command-line)))" "$@"
!#

;;
;; Advent Of Code 2020
;; Day 4 (Part 2)
;;
;; Aleix Conchillo Flaque <aconchillo@gmail.com>
;;

(use-modules (ice-9 match)
             (ice-9 regex)
             (ice-9 textual-ports)
             (srfi srfi-1)
             (system base pmatch))

(define (load-passport port)
  (define (parse-line lines)
    (let ((single (string-join lines)))
      (map (lambda (entry)
             (let ((l (string-split entry #\:)))
               ;; Convert to cons so we really have a key/value pair.
               (cons (car l) (cadr l))))
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

(define (byr-valid? value)
  (and (>= (string->number value) 1920)
       (<= (string->number value) 2002)))

(define (iyr-valid? value)
  (and (>= (string->number value) 2010)
       (<= (string->number value) 2020)))

(define (eyr-valid? value)
  (and (>= (string->number value) 2020)
       (<= (string->number value) 2030)))

(define (hgt-valid? value)
  (let ((m (string-match "^([0-9]+)(cm|in)$" value)))
    (cond
     ((not m) #f)
     (else
      (let ((height (match:substring m 1))
            (unit (match:substring m 2)))
        (match unit
          ("cm" (and (>= (string->number height) 150)
                     (<= (string->number height) 193)))
          ("in" (and (>= (string->number height) 59)
                     (<= (string->number height) 76)))))))))

(define (hcl-valid? value)
  (not (nil? (string-match "^#([0-9|a-f]){6}$" value))))

(define (ecl-valid? value)
  (not (nil? (string-match "^(amb|blu|brn|gry|grn|hzl|oth)$" value))))

(define (pid-valid? value)
  (not (nil? (string-match "^[0-9]{9}$" value))))

(define (is-field-valid? passport field)
  (let ((value (assoc-ref passport field)))
    (cond
     ((not value) #f)
     (else
      (match field
        ("byr" (byr-valid? value))
        ("iyr" (iyr-valid? value))
        ("eyr" (eyr-valid? value))
        ("hgt" (hgt-valid? value))
        ("hcl" (hcl-valid? value))
        ("ecl" (ecl-valid? value))
        ("pid" (pid-valid? value))
        (_ #f))))))

(define (is-passport-valid? passport)
  (and (is-field-valid? passport "byr")
       (is-field-valid? passport "iyr")
       (is-field-valid? passport "eyr")
       (is-field-valid? passport "hgt")
       (is-field-valid? passport "hcl")
       (is-field-valid? passport "ecl")
       (is-field-valid? passport "pid")))

(define (count-valid-passports passports)
  (count is-passport-valid? passports))

(define (main . args)
  (pmatch args
    ((,file-name)
     (let ((passports (call-with-input-file file-name load-passports)))
       (display (count-valid-passports passports))))
    (else
     (format (current-error-port) "Usage: ./day-04-part2.scm <input-file>~%"))))
