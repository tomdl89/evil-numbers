;;; evil-numbers-roman.el --- Increment/decrement roman numerals -*- lexical-binding: t -*-

;; Copyright (C) 2011 by Michael Markert
;;               2020 by Julia Path
;;               2024 by Tom Dalziel
;; Author: Tom Dalziel <tom_dl@hotmail.com>
;; Maintainer: Julia Path <julia@jpath.de>
;; Contributors: Tom Dalziel <tom_dl@hotmail.com>
;; URL: http://github.com/juliapath/evil-numbers
;; Git-Repository: git://github.com/juliapath/evil-numbers.git
;; Created: 2024-04-22
;; Version: 0.7
;; Package-Requires: ((emacs "24.1") (evil "1.2.0"))
;; Keywords: convenience tools

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; TODO

;; Known Bugs:
;; See http://github.com/juliapath/evil-numbers/issues

;;; Code:

(require 'evil)

(defvar evil-numbers/roman-subtractives
  '(("IV" 4)
    ("IX" 9)
    ("XL" 40)
    ("XC" 90)
    ("CD" 400)
    ("CM" 900)))

(defvar evil-numbers/roman-additives
  '(("I" 1)
    ("V" 5)
    ("X" 10)
    ("L" 50)
    ("C" 100)
    ("D" 500)
    ("M" 1000)))

(defvar evil-numbers/bad-pairs
  '((900 500) (900 400) (900 100)
    (90 50) (90 40) (90 10)
    (9 5) (9 4) (9 1)
    (500 500) (500 400) (400 100)
    (50 50) (50 40) (40 10)
    (5 5) (5 4) (4 1)
    (400 400) (40 40) (4 4)))

(defun evil-numbers/tokenize-roman (s)
  (let ((rx "\\(?1:IV\\|IX\\|XL\\|XC\\|CD\\|CM\\)\\|\\(?2:[IVXLCDM]\\)\\|\\(?3:.\\)")
        (index 0)
        case-fold-search
        tokens)
    (save-match-data
      (while (string-match rx s index)
        (cond ((match-string 1 s) ; Subtractive tokens like IV and CM
               (setq index (match-end 1))
               (push (cadr (assoc (match-string 1 s) evil-numbers/roman-subtractives))
                     tokens))
              ((match-string 2 s) ; Simple additive tokens like I and M
               (setq index (match-end 2))
               (push (cadr (assoc (match-string 2 s) evil-numbers/roman-additives))
                     tokens))
              ((match-string 3 s) ; Not a valid token
               (throw 'bad-digit (list :digit (match-string 3 s)))))))
    tokens))

(defun evil-numbers/roman->int (s)
  (catch 'bad-digit
    (let ((tokens (evil-numbers/tokenize-roman s))
          (sum 0)
          prev-token)
      (while tokens
        (if (or (member (list (car tokens) prev-token) evil-numbers/bad-pairs)
                (and prev-token (> prev-token (car tokens))))
            (throw 'bad-digit (list :pair (car tokens) prev-token))
          (setq sum (+ (car tokens) sum)))
        (setq prev-token (car tokens)
              tokens (cdr tokens)))
      sum)))

(defun evil-numbers/digit->roman (digit place)
  (if-let (arabic (and (string-match-p "[0-9]" digit) (string-to-number digit)))
      (pcase place
        (1 (nth arabic '("" "I" "II" "III" "IV" "V" "VI" "VII" "VIII" "IX")))
        (2 (nth arabic '("" "X" "XX" "XXX" "XL" "L" "LX" "LXX" "LXXX" "XC")))
        (3 (nth arabic '("" "C" "CC" "CCC" "CD" "D" "DC" "DCC" "DCCC" "CM")))
        (4 (make-string arabic ?M))
        (_ (throw 'bad-digit (list :place place))))
    (throw 'bad-digit (list :digit digit))))

(defun evil-numbers/int->roman (n)
  (let* ((s-num (mapcar #'string (number-to-string n)))
         (s-len (length s-num))
         (roman-form ""))
    ;; TODO positive?
    (catch 'bad-digit
      (while (< 0 s-len)
        (let ((roman-digit (evil-numbers/digit->roman (car s-num) s-len)))
          (setq roman-form (concat roman-form roman-digit)))
        (setq s-num (cdr s-num)
              s-len (1- s-len)))
      roman-form)))
