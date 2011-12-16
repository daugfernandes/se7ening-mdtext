;;;  s7store.lsp
;;
;;   Save and load operations
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

(defpackage :se7ening.store
  (:use :common-lisp)
  (:export :save-relation
	   :load-relation
	   :save-hash-table))

;; ======================================================================
;  For relations
;  (*) Based on Peter Siebel's PRATICAL COMMON LISP Chapter 2 sample code
;------------------------------------------------------------------------
(defun save-relation (relation filename)
  (with-open-file (out filename
                   :direction :output
                   :if-exists :supersede)
    (with-standard-io-syntax
      (print relation out)))
  t)

(defun load-relation (filename)
  (with-open-file (in filename)
    (with-standard-io-syntax
      (read in))))

;------------------------------------------------------------------------
;  For hash-tables adapted from:
;       www.lispworks.com
;------------------------------------------------------------------------
(defun save-hash-table (ht filename)
  (let ((rel ()))
    (maphash #'(lambda (key val)
		 (push (list key val) rel))
	     ht)
    (save-relation rel filename)))
    
; TODO: load-hash-table
