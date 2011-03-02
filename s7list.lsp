;;;  s7list.lsp
;;
;;   List related utilities
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

(defpackage :se7ening.list
  (:use :common-lisp)
  (:export :string-to-list
	   :push-tail
	   :pop-tail
	   :tail
	   :split
	   :has))

(defun string-to-list (s)
  "Returns a list of s's chars."
  (loop for char across s collect char))
  
;;----------------------------------------------------------------------
(defun push-tail (element L)
  "Inserts `element' at the tail of list `L'."
  (if (null (cdr L))
    (setf (cdr L) (cons element nil))
    (push-tail element (cdr L))))

;;----------------------------------------------------------------------
(defmacro pop-tail (L)
  "Removes last element of list `L'"
  `(if (null (cdr ,L))
    (let ((tmp (car ,L)))
      (setf ,L nil)
      tmp)
    (pop-tail (cdr ,L))))

;;----------------------------------------------------------------------
(defun tail (L)
  "Last element of list `L'"
  (if (null (cdr L))
    L
    (tail (cdr L))))

;;----------------------------------------------------------------------
(defun split (string separator)
  "List with all the substrings of `string' separated by `separator'."
  (setf p (position separator string :test #'equal))
  (if
    (null p)
    (list string)
    (append (list (subseq string 0 p)) (split (subseq string (+ 1 p)) separator))))

;;----------------------------------------------------------------------
(defun has (L element eqf)
  "Search `L´ for `element´."
  (if (null L)
      nil
      (if
	  (funcall eqf element (car L))
	  t
	  (has (cdr L) element eqf))))
	  
