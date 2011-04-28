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

(defvar *k-sonetos* 0)
(defvar *k-estrofes* 0)

(defun sonetos ()
  (let ((book (list)))
    (setf *k-sonetos* 0)
    (loop for i in 
	 (cdr 
	  (getf 
	   (http-to-iln "http://pt.wikisource.org/wiki/Portal:Sonetos_de_Lu%C3%ADs_Vaz_de_Cam%C3%B5es" #'html-li-state)
	   :root))
       do
	 (setf book 
	       (append 
		book
		(list (getf
		 (getf 
		  (soneto 
		   (concatenate 'string "http://pt.wikisource.org" (getf i :link))
		   (getf i :desc))
		  :root)
		 :b)))))
    book))

(defun soneto (url nome)
  (incf *k-sonetos*)
  (http-to-iln url #'html-soneto-state))
  ;(list :root (list :b (list (list url) (list nome)))))

(defun soneto2 (l)
  (let ((book (list)))
    (setf *k-sonetos* 0)
    (loop for i in l do
	 (incf *k-sonetos*)
	 (push (getf (getf (http-to-iln (caar i) #'html-soneto-state) :root) :b) book))
    book))
       

(defun http-to-iln (url entry-state-fn)
  "Opens a url and returns a list of its content's lines."
  (setf *k-estrofes* 0)
  (ext:with-http-input (stream url)
    (stream-to-mdtext-iln stream entry-state-fn)))


"http://pt.wikisource.org/wiki/vossos_olhos,_senhora,_que_competem" "Vossos olhos, Senhora, que competem" 


;; state functions for getting the sonets it self
(defun html-soneto-state (c)
  (when (char= c #\")
    (next-state #'html-soneto-state1)))

(defun html-soneto-state1 (c)
  (if (char= c #\p)
      (next-state #'html-soneto-state2)
      (next-state #'html-soneto-state)))

(defun html-soneto-state2 (c)
  (if (char= c #\o)
      (next-state #'html-soneto-state3)
      (next-state #'html-soneto-state)))

(defun html-soneto-state3 (c)
  (if (char= c #\e)
      (next-state #'html-soneto-state4)
      (next-state #'html-soneto-state)))

(defun html-soneto-state4 (c)
  (if (char= c #\m)
      (next-state #'html-soneto-state5)
      (next-state #'html-soneto-state)))

(defun html-soneto-state5 (c)
  (if (char= c #\")
      (next-state #'html-soneto-state6)
      (next-state #'html-soneto-state)))

(defun html-soneto-state6 (c)
  (cond 
    ((char= c #\>)
     (make-node (list :c *k-sonetos*))
     (make-node (list :e (incf *k-estrofes*)))
     (setf *k-versos* 0)
     (next-state #'html-soneto-state7))
    (t
     (next-state #'html-soneto-state))))

(defun html-soneto-state7 (c)
  (cond
    ((char= c #\Linefeed)) ; keep state
    ((char= c #\Return))   ; keep state
    ((char= c #\<)
     (next-state #'html-soneto-state8))
    (t
     (add-char c *character-stack*))))

(defun html-soneto-state8 (c)
  (cond
    ((char= c #\b)
     (next-state #'html-soneto-state-br))
    ((char= c #\/)
     (next-state #'html-soneto-state-end))
    (t
     (next-state #'html-soneto-state-ignore-tag))))

(defun html-soneto-state-ignore-tag (c)
  (when (char= c #\>)
    (next-state #'html-soneto-state7)))

(defun html-soneto-state-br (c)
  (if (char= c #\r)
      (next-state #'html-soneto-state-br1)
      (next-state #'html-soneto-state-ignore-tag)))

(defun html-soneto-state-br1 (c)
  (if (char= c #\Space)
      (next-state #'html-soneto-state-br2)
      (next-state #'html-soneto-state-ignore-tag)))

(defun html-soneto-state-br2 (c)
  (if (char= c #\/)
      (next-state #'html-soneto-state-br3)
      (next-state #'html-soneto-state-ignore-tag)))

(defun html-soneto-state-br3 (c)
  (cond 
    ((char= c #\>)
     (cond 
       ((> (length *character-stack*) 0)
	(let ((aux *character-stack*))
	  (make-node (list :v (incf *k-versos*)))
	  (make-node (list :vall))
	  (use-chars-read aux)
	  (pop *tree*)
	  (phrase-to-mdtext-iln *character-stack*)
	  (pop *tree*)
	  (setf *character-stack* (make-empty-string)))
	(next-state #'html-soneto-state7))
       (t
	(pop *tree*)
	(make-node (list :e (incf *k-estrofes*)))
	(setf *k-versos* 0)
	(next-state #'html-soneto-state7))))
    (t
     (next-state #'html-soneto-state-ignore-tag))))

(defun html-soneto-state-end (c)
  (if (char= c #\d)
      (next-state #'html-soneto-state-end1)
      (next-state #'html-soneto-state-ignore-tag)))

(defun html-soneto-state-end1 (c)
  (if (char= c #\i)
      (next-state #'html-soneto-state-end2)
      (next-state #'html-soneto-state-ignore-tag)))

(defun html-soneto-state-end2 (c)
  (cond 
    ((char= c #\v)
     (let ((aux *character-stack*))
       (make-node (list :v (incf *k-versos*)))
       (make-node (list :vall))
       (use-chars-read aux)
       (pop *tree*)
       (phrase-to-mdtext-iln *character-stack*)
       (pop *tree*)
       (setf *character-stack* (make-empty-string)))
     (next-state #'html-soneto-state-trash))
    (t
     (next-state #'html-soneto-state-ignore-tag))))

(defun html-soneto-state-trash (c)
  (next-state #'html-soneto-state-trash))


;; state function for getting sonets url from list page

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

