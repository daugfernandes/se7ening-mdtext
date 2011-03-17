;;;  s7sonetos.lsp
;;
;;   Parser para os sonetos de camoes em 
;;   --> http://pt.wikisource.org/wiki/Portal:Sonetos_de_Lu%C3%ADs_Vaz_de_Cam%C3%B5es
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

(load "s7list.lsp")
(load "s7string.lsp")
(load "s7store.lsp")
(load "s7mdtext.lsp")

(defpackage :se7ening.sonetos
  (:use :se7ening.mdtext))

(defun sonetos ()
  (loop for i in 
       (cdr 
	(getf 
	 (http-to-iln "http://pt.wikisource.org/wiki/Portal:Sonetos_de_Lu%C3%ADs_Vaz_de_Cam%C3%B5es")
	 :root))
     do
       (setf url  (concatenate 'string "http://pt.wikisource.org" (getf i :link)))
       (setf desc (getf i :desc))
       (print desc)))

(defun http-to-iln (url)
  "Opens a url and returns a list of its content's lines."
  (setf *k-chars-processed* 0)
  (ext:with-http-input (stream url)
    (stream-to-mdtext-iln stream #'html-li-state)))

(defun html-li-state (c)
  (when (char= c #\<)
    (next-state #'html-li-state1)))

(defun html-li-state1 (c)
  (cond
   ((char= c #\l)
    (next-state #'html-li-state2))
   (t
    (next-state #'html-li-state))))

(defun html-li-state2 (c)
  (cond
   ((char= c #\i)
    (next-state #'html-li-state3))
   (t
    (next-state #'html-li-state))))

(defun html-li-state3 (c)
  (cond
   ((char= c #\>)
    (next-state #'html-li-state4))
   (t
    (next-state #'html-li-state))))

(defun html-li-state4 (c)
  (cond
   ((char= c #\<)
    (next-state #'html-li-state5))
   (t
    (next-state #'html-li-state))))

(defun html-li-state5 (c)
  (cond
   ((char= c #\a)
    (next-state #'html-li-state6))
   (t
    (next-state #'html-li-state))))

(defun html-li-state6 (c)
  (cond
   ((char= c #\Space)
    (next-state #'html-li-state7))
   (t
    (next-state #'html-li-state))))

(defun html-li-state7 (c)
  (cond
   ((char= c #\h)
    (next-state #'html-li-state8))
   (t
    (next-state #'html-li-state))))

(defun html-li-state8 (c)
  (cond
   ((char= c #\r)
    (next-state #'html-li-state9))
   (t
    (next-state #'html-li-state))))

(defun html-li-state9 (c)
  (cond
   ((char= c #\e)
    (next-state #'html-li-state10))
   (t
    (next-state #'html-li-state))))

(defun html-li-state10 (c)
  (cond
   ((char= c #\f)
    (next-state #'html-li-state11))
   (t
    (next-state #'html-li-state))))

(defun html-li-state11 (c)
  (cond
   ((char= c #\=)
    (next-state #'html-li-state12))
   (t
    (next-state #'html-li-state))))

(defun html-li-state12 (c)
  (cond
   ((char= c #\")
    (make-node (list :link ))
    (next-state #'html-li-state13))
   (t
    (next-state #'html-li-state))))

(defun html-li-state13 (c)
  (cond
   ((char= c #\")
    (downcase *character-stack*)
    (use-chars-read *character-stack*)
    (next-state #'html-li-state14))
   (t
    (add-char c *character-stack*))))

(defun html-li-state14 (c)
  (when (char= c #\>)
    (next-state #'html-li-state15)))

(defun html-li-state15 (c)
  (cond
   ((char= c #\<)
    (push-tail :desc (car *tree*))
    (use-chars-read *character-stack*)
    (pop *tree*)
    (next-state #'html-li-state))
   (t
    (add-char c *character-stack*))))

