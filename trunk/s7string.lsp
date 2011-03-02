;;;  s7string.lsp
;;
;;   String related utilities
;;
;; ----------------------------------------------------------------------
;;
;;   Copyright (C) 2011  David Fernandes
;;                       <daugfernandes@aim.com>
;;
;;   This program is free software: you can redistribute it and/or modify
;;   it under the terms of the GNU General Public License as published by
;;   the Free Software Foundation, either version 3 of the License, or
;;   (at your option) any later version.
;;
;;   This program is distributed in the hope that it will be useful,
;;   but WITHOUT ANY WARRANTY; without even the implied warranty of
;;   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;   GNU General Public License for more details.
;;
;;   You should have received a copy of the GNU General Public License
;;   along with this program named license.txt.
;;   If not, see <http://www.gnu.org/licenses/>
;;
;; ----------------------------------------------------------------------

(defpackage :se7ening.string
  (:use :common-lisp)
  (:export :replicate
	   :make-empty-string
	   :capitalized-p
	   :last-char-of
	   :downcase
	   :replace-all
	   :alpha-p
	   :upper-p
	   :lower-p
	   :numeric-p
	   :alphanumeric-p
	   :specialchar-p))

(defun replicate (s n)
  "Replicates string `s´ `n´times."
  (if (<= n 0)
      ""
      (concatenate 'string s (replicate s (- n 1))))) 

(defun make-empty-string ()
  "Builds an empty string."
  (make-array 0
	      :fill-pointer 0
	      :adjustable   t
	      :element-type 'base-char))

(defun capitalized-p (s)
  "Tests if string s first char is upper-case."
  (upper-p (char s 0)))

(defun last-char-of (s)
  "Returns last char of `s'."
  (char s (1- (length s))))

(defmacro downcase (s)
  "Converts `s' to lower case."
  `(setf ,s (format nil "~(~a~)" ,s)))

;; as published in http://cl-cookbook.sourceforge.net/strings.html
(defun replace-all (string part replacement &key (test #'char=))
  "Returns a new string in which all the occurences of the part is replaced with replacement."
  (with-output-to-string 
    (out)
    (loop with part-length = (length part)
	  for old-pos = 0 then (+ pos part-length)
	  for pos = (search part string
			    :start2 old-pos
			    :test test)
	  do (write-string string out
			   :start old-pos
			   :end (or pos (length string)))
	  when pos do (write-string replacement out)
	  while pos))) 

;; some char utils

(defun alpha-p (c)
  "Tests for a alphabetic char."
  (or (upper-p c) (lower-p c)))

(defun upper-p (c)
  "Tests for upper-case."
  (or (and (char>= c #\A) (char<= c #\Z))
      (find c "ÃÁÀÂÕÓÒÔÍÌÊÉÈÚÙÇ")))

(defun lower-p (c)
  "Tests for lower case."
  (or (and (char>= c #\a) (char<= c #\z))
      (find c "ãáàâõóòôíìêéèúùç")))

(defun numeric-p (c)
  "Tests for a numeric char."
  (and (char>= c #\0) (char<= c #\9)))

(defun alphanumeric-p (c)
  "Tests for a alphanumeric char."
  (or (alpha-p c) (numeric-p c)))

(defun specialchar-p (c)
  "Tests for a symbol char."
  (null (or (alpha-p c) (numeric-p c))))
