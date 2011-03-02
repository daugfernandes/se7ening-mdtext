;;;  s7mdtext.lsp
;;
;;   Parser of text-file to mdtext ILN (Intermediate LIST notation)
;;   and related functions.
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

(defpackage :se7ening.mdtext
  (:use :common-lisp
	:common-lisp-user
	:se7ening.list
	:se7ening.string
	:se7ening.store))

;; ILN structures
(defvar *document*          nil "Document's root node.")
(defvar *tree*              nil "Stack of elements. (car *tree*) -> element in use.")
(defvar *state*             nil "Current state processing function.")
(defvar *character-stack*   nil "Characters read waiting to be processed.")

;; counters
(defvar *k-chapters*        0   "Chapters counter.")
(defvar *k-paragraphs*      0   "Paragraphs counter.")
(defvar *k-phrases*         0   "Phrases counter.")
(defvar *k-words*           0   "Words counter.")
(defvar *k-chars-processed* 0   "Character counter.")

(defvar *word-table*        nil "Unique words hash-table.")

;; Explore mdtext intermediate lisp notation (ILN)

(defun chapters (book)
  (cdr (getf book :root)))

(defun paragraphs (chapter)
  (cddr chapter))

(defun phrases (paragraph)
  (cddr paragraph))

(defun words (phrase)
  (cdddr phrase))

(defun get-chapter (book chapter-number)
  "Returns a chapter."
  (nth (- chapter-number 1) (chapters book)))

(defun get-paragraph (chapter paragraph-number)
  "Returns a paragraph."
  (nth (- paragraph-number 1) (paragraphs chapter)))

(defun get-phrase (paragraph phrase-number)
  "Returns a phrase."
  (nth (- phrase-number 1) (phrases paragraph)))

(defun get-word (phrase word-number)
  "Returns a word."
  (nth (- word-number 1) (words phrase)))

(defmacro insert-book-sql (stream book-id book-name)
  "Generates insert DDL for a book."
  `(format 
    ,stream 
    "INSERT INTO BOOKS(ID,NAME) VALUES(~a,'~a');~%" 
    ,book-id 
    ,book-name))

(defmacro insert-chapter-sql (stream book-id chapter-id chapter-number chapter-name)
  "Generates insert DDL for a chapter."
  `(format 
    ,stream 
    "INSERT INTO CHAPTERS(ID,K,BOOK_ID,NAME) VALUES(~a,~a,~a,'~a');~%" 
    ,chapter-id
    ,chapter-number
    ,book-id 
    ,chapter-name))

(defmacro insert-paragraph-sql (stream chapter-id paragraph-id paragraph-number paragraph-name)
  "Generates insert DDL for a paragraph."
  `(format 
    ,stream 
    "INSERT INTO PARAGRAPHS(ID,K,CHAPTER_ID,NAME) VALUES(~a,~a,~a,'~a');~%" 
    ,paragraph-id
    ,paragraph-number
    ,chapter-id 
    ,paragraph-name))

(defmacro insert-phrase-sql (stream paragraph-id phrase-id phrase-number phrase)
  "Generates insert DDL for a phrase."
  `(format 
    ,stream 
    "INSERT INTO PHRASES(ID,K,PARAGRAPH_ID,PHRASE) VALUES(~a,~a,~a,'~a');~%" 
    ,phrase-id
    ,phrase-number
    ,paragraph-id 
    (chars-sql ,phrase)))

(defmacro insert-word-sql (stream phrase-id word-id word-number)
  "Generates insert DDL for a word."
  `(format 
    ,stream 
    "INSERT INTO PHRASES_WORDS(PHRASE_ID,WORD_ID,K) VALUES(~a,~a,~a);~%" 
    ,phrase-id
    ,word-id
    ,word-number))

(defun words-to-sql (words-hash-table)
  (with-output-to-string (stream)
    (maphash #'(lambda (key val)
		 (format 
		  stream
		  "INSERT INTO WORDS(WORD,ID) VALUES('~a',~a);~%"
		  (chars-sql key)
		  val))
	     words-hash-table)))

(defun chars-sql (str)
  "Double some SQL problematic chars like ' and \"."
  (replace-all str "'" "''"))

(defun mdtext-iln-to-sql (book last-book-id last-chapter-id last-paragraph-id last-phrase-id)
  "Generates the complete insert DDL script for a complete book.
   Please note that as there is no direct connection to the database, 
   you'll have to manualy state last-ids already in used."

  (with-output-to-string (stream)

    (let ((k-chapter 0)
	  (k-paragraph 0)
	  (k-phrase 0)
	  (k-word 0))
      
      (insert-book-sql stream (incf last-book-id) "??")

      (dolist (chapter (chapters book)) 
	(progn 

	  (insert-chapter-sql 
	     stream 
	     last-book-id 
	     (incf last-chapter-id)
	     (incf k-chapter) 
	     (nth 1 chapter))

	  (setf k-paragraph 0)

	  (dolist (paragraph (paragraphs chapter))
	    (progn

	      (insert-paragraph-sql
	         stream
		 last-chapter-id
		 (incf last-paragraph-id)
		 (incf k-paragraph)
		 (nth 1 paragraph))

	      (setf k-phrase 0)

	      (dolist (phrase (phrases paragraph))
		(progn

		  (insert-phrase-sql
		     stream
		     last-paragraph-id
		     (incf last-phrase-id)
		     (incf k-phrase)
		     (getf (nth 1 (cdr phrase)) :vall))

		  (setf k-word 0)

		  (dolist (word-id (words phrase))

		    (insert-word-sql 
		       stream
		       last-phrase-id
		       word-id
		       (incf k-word))))))))))))


(defun mdtext-iln-to-txt (book)
  "Generates the `original' text.
   TODO: A way to the function how to format the output; as the format
   hard-coded here is specific for 'Os Lusíadas'."

  (with-output-to-string (stream)
    (dolist (chapter (chapters book)) 
      (progn 
	(format stream "~%~%Canto ~a:~%" (nth 1 chapter)) ; change as needed (Lusíadas specific)
	(dolist (paragraph (paragraphs chapter))
	  (progn
	    (format stream "~%~a~%" (nth 1 paragraph))
	    (dolist (phrase (phrases paragraph))
	      (format stream "~a~%" (getf (nth 1 (cdr phrase)) :vall)))))))))

;; TXT2ILN

(defun file-to-mdtext-iln (filename)
  "Parses an entire text file into mdtext-iln."
  (setf *k-chars-processed* 0)
  (with-open-file (stream filename :direction :input)
    (stream-to-mdtext-iln stream)))

(defun stream-to-mdtext-iln (stream)
  "Parses a stream of characters into mkdtext-iln."
  (setf *document* (list :root)
        *tree* (list *document*)
        *character-stack* (make-empty-string)
	*k-chapters* 0
	*k-words* 0
        *state* #'a-state
	*word-table* (make-hash-table :test 'equal))

  (make-node (list :b))
  
  (catch 'end-of-file
    (loop
      (let ((c (read-char stream nil :eof)))
	(progn
	  (if 
	   (eq c :eof)
	   (progn
	     (use-chars-read *character-stack*)
	     (throw 'end-of-file nil))
	   (funcall *state* c))))))
  *document*)

;; DFA - phrase

(defun phrase-to-mdtext-iln (str)
  "Parses a phrase's words into mdtext-iln."
  (setf *character-stack* (make-empty-string))
  (with-input-from-string (s str)
    (phrase-stream-to-mdtext-iln s)))

(defun phrase-stream-to-mdtext-iln (stream)
  "Parses a stream of characters into mdtext-iln."
  (next-state #'aa-state)

  (catch 'end-of-file
    (loop
      (let ((c (read-char stream nil :eof)))
	(progn
	  (if 
	   (eq c :eof)
	      (progn
		(downcase *character-stack*)
		(use-word *character-stack*)
		(throw 'end-of-file nil))
	   (funcall *state* c)))))))

;; DFA - states

(defun aa-state (c)
  (cond

    ((alpha-p c)
     (add-char c *character-stack*)
     (next-state #'bb-state))

    (t)))          ; else, keep state

(defun bb-state (c)
  (cond

   ((alpha-p c)
    (add-char c *character-stack*))  ; keep state

   ((char= c #\')
    (add-char c *character-stack*)
    (downcase *character-stack*)
    (use-word *character-stack*)
    (setf *state* #'aa-state))

   ((char= c #\-)
    (next-state #'cc-state))

   ((or (specialchar-p c) (char= c #\Space))
    (downcase *character-stack*)
    (use-word *character-stack*)
    (next-state #'aa-state))

   (t
     (error (format t "ERR @bb-state invalid char [~a] found." c)))))

(defun cc-state (c)
  (cond

   ((char= c #\-))  ; keep state

   ((alpha-p c)
    (add-char #\- *character-stack*)
    (add-char c *character-stack*)
    (next-state #'dd-state))

   (t
     (error (format t "ERR @cc-state invalid char [~a] found." c)))))

(defun dd-state (c)
  (cond

   ((alpha-p c)
    (add-char c *character-stack*))  ; keep state

   ((char= c #\-)
    (add-char c *character-stack*)
    (next-state #'ee-state))

   ((or (specialchar-p c) (char= c #\Space))
    (downcase *character-stack*)
    (use-word *character-stack*)
    (next-state #'aa-state))

   (t
     (error (format t "ERR @dd-state invalid char [~a] found." c)))))

(defun ee-state (c)
  (cond

   ((alpha-p c)
    (add-char c *character-stack*)
    (next-state #'ff-state))

   (t
     (error (format t "ERR @ee-state invalid char [~a] found." c)))))

(defun ff-state (c)
  (cond

   ((alpha-p c)
    (add-char c *character-stack*))  ; keep state

   ((or (specialchar-p c) (char= c #\Space))
    (downcase *character-stack*)
    (use-word *character-stack*)
    (next-state #'aa-state))

   (t
     (error (format t "ERR @ff-state invalid char [~a] found." c)))))

;; DFA - book states

(defun a-state (c)
  "a"
  (cond

    ((char= c #\$)
     (make-node (list :c (incf *k-chapters*)))
     (print (format t "~a " *k-chapters*))
     (setf *k-paragraphs* 0)
     (next-state #'b-state))

    (t
     (error (format t "ERR @a-state invalid char [~a] found." c)))))

(defun b-state (c)
  "b"
  (cond

    ((char= c #\Linefeed)
     (next-state #'c-state))

    (t
     (error (format t "ERR @b-state invalid char [~a] found." c)))))

(defun c-state (c)
  "c"
  (cond
    ((char= c #\Linefeed)
     (next-state #'d-state))
    (t
     (error (format t "ERR @c-state invalid char [~a] found." c)))))

(defun d-state (c)
  "d"
  (cond

    ((char= c #\Linefeed)) ; keep state

    ((char= c #\$)
     (pop *tree*)
     (make-node (list :c (incf *k-chapters*)))
     (print (format t "~a " *k-chapters*))
     (setf *k-paragraphs* 0)
     (next-state #'b-state))

    ((numeric-p c)
     (make-node (list :e (incf *k-paragraphs*)))
     (setf *k-phrases* 0)
     (next-state #'e-state)
     (add-char c *character-stack*))

    (t
     (error (format t "ERR @d-state invalid char [~a] found." c)))))

(defun e-state (c)
  "e"
  (cond

    ((numeric-p c) ; keep state
     (add-char c *character-stack*))

    ((char= c #\Linefeed)
     (setf *character-stack* (make-empty-string)) ; discard paragraph number from book
     (next-state #'f-state))

    (t
     (error (format t "ERR @e-state invalid char [~a] found." c)))))

(defun f-state (c)
  "f"
  (cond

    ((char= c #\Linefeed)) ; LF tolerant

    ((char= c #\$)
     (pop *tree*) ; paragraph
     (pop *tree*) ; chapter
     (make-node (list :c (incf *k-chapters*)))
     (print (format t "~a " *k-chapters*))
     (setf *k-paragraphs* 0)
     (next-state #'g-state))

    ((null (numeric-p c))
     (make-node (list :v (incf *k-phrases*)))
     (next-state #'g-state)
     (add-char c *character-stack*))

    (t
     (error (format t "ERR @f-state invalid char [~a] found." c)))))

(defun g-state (c)
  "g"
  (cond

    ((char= c #\Linefeed)
     (let ((aux *character-stack*))
       (make-node (list :vall))
       (use-chars-read aux)
       (pop *tree*)
       (phrase-to-mdtext-iln *character-stack*)
       (setf *character-stack* (make-empty-string)))
     (pop *tree*) ; phrase
     (next-state #'h-state))

    ((null (numeric-p c))
     (add-char c *character-stack*))

    (t
     (error (format t "ERR @g-state invalid char [~a] found." c)))))

(defun h-state (c)
  "h"
  (cond

    ((char= c #\Linefeed)
     (pop *tree*) ; paragraph
     (next-state #'d-state))

    ((null (numeric-p c))
     (make-node (list :v (incf *k-phrases*)))
     (next-state #'g-state)
     (add-char c *character-stack*))

    (t
     (error (format t "ERR @h-state invalid char [~a] found." c)))))

(defmacro next-state (state)
  "Sets next state."
  `(setf *state* ,state))

;; Intermediate list notation (ILN) utilities
     
(defun make-node (content)
  "Creates a new node." 
  (let ((node content))
    (push-tail node (car *tree*))  ;sets it in the document structure
    (push node *tree*)))           ;and in the document pushdown tree, e.g. "actual" element

(defmacro add-char (c char-stack)
  "Adds char c to string of chars not yet used."
  (incf *k-chars-processed*)
  `(vector-push-extend ,c ,char-stack))

(defmacro use-word (buffer)
  "Gives effective use to the characters read so far.
   Used in phrase DFA."
  `(cond
    ((> (length ,buffer) 0)
      (push-tail 
        (add-word (string-right-trim " " ,buffer) *word-table*)
        (car *tree*))
      (setf ,buffer (make-empty-string)))))

(defmacro use-chars-read (buffer)
  "Gives effective use to the characters read so far.
   Used in book DFA."
  `(cond
    ((> (length ,buffer) 0)
      (push-tail 
        (string-right-trim " " ,buffer)
        (car *tree*))
      (setf ,buffer (make-empty-string)))))

;;======================================================================
;;                            word-table utilities
;;======================================================================
(defmacro add-word (w ht)
  "Adds a new word to the hash-table or returns number of 
   already existing one."
  `(let ((entry (gethash ,w ,ht)))
     (if (null (nth-value 0 entry))
	 (nth-value 0 (setf (gethash ,w ,ht) (incf *k-words*)))
	 (nth-value 0 entry))))

