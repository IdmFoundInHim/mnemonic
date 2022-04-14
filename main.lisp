(defparameter *transcription* (make-hash-table :test #'equal))

(defmacro populate-transcription-table (&rest alternating-keys-values)
  `(progn
     ,@(loop for index-value
	  from 1 below (length alternating-keys-values)
	  by 2
	  collect `(setf (gethash ,(elt alternating-keys-values (- index-value 1)) *transcription*)
			 ,(elt alternating-keys-values index-value)))))

(populate-transcription-table
 #\j #\U+05d0
 #\b #\U+05d1
 #\g #\U+05d2
 #\d #\U+05d3
 #\h #\U+05d4
 #\v #\U+05d5
 #\z #\U+05d6
 #\H #\U+05d7
 #\c #\U+05d8
 #\y #\U+05d9
 #\k #\U+05db
 #\l #\U+05dc
 #\m #\U+05de
 #\n #\U+05e0
 #\s #\U+05e1
 #\J #\U+05e2
 #\f #\U+05e4
 #\x #\U+05e6
 #\q #\U+05e7
 #\r #\U+05e8
 #\w #\U+fb2a
 #\R #\U+fb2b
 #\t #\U+05ea

 #\B #\U+fb31
 #\G #\U+fb32
 #\D #\U+fb33
 ";h" #\U+fb34
 #\V #\U+fb35
 "vO" #\U+fb4b
 #\Z #\U+fb36
 #\C #\U+fb38
 #\Y #\U+fb39
 #\K #\U+fb3b
 "k " #\U+05da
 "K " #\U+fb3a
 #\L #\U+fb3c
 #\M #\U+fb3e
 "m " #\U+05dd
 ;;;; "M " #\U+fb3d
 #\N #\U+fb40
 "n " #\U+05df
 ;;;; "N " #\U+fb3f
 #\S #\U+fb41
 #\F #\U+fb44
 ;;;; May be good to add duplicates, so that
 ;;;; (#\F #\p) #\U+fb44
 ;;;; would work
 #\p  #\U+fb44
 "f " #\U+05e3
 "p " #\U+fb43
 "F " #\U+fb43
 #\X #\U+fb46
 "x " #\U+05e5
 ;;;; "X " #\U+fb45
 #\Q #\U+fb47
 #\W #\U+fb2c
 ";R" #\U+fb2d
 #\T #\U+fb4a


 #\; #\U+05b0
 ";e" #\U+05b1
 ";a" #\U+05b2
 ";o" #\U+05b3
 #\i #\U+05b4
 #\E #\U+05b5
 #\e #\U+05b6
 #\a #\U+05b7
 #\A #\U+05b8
 #\O #\U+05b9
 #\u #\U+05bb

 #\I (format nil "~a~a" #\U+05b4 #\U+05d9)

 ":d" #\U+05bc
 ":m" #\U+05bc
 ": " "")

(defun +nil (&rest cardinals)
  (loop for num in cardinals
     if (eq nil num) return nil
     summing num))

(defun transcription-reigon-handling (in-str)
  (with-output-to-string (out-str)
    (let ((chars-processed 0))
      (loop while chars-processed
	 do (let* ((start-reigon (search "::" in-str :start2 chars-processed))
		   (end-reigon (if start-reigon (search "::" in-str :start2 (1+ start-reigon)) nil)))
	      (princ (subseq in-str chars-processed start-reigon) out-str)
	      (if start-reigon (princ (transcribe-string (subseq in-str (+ 2 start-reigon) end-reigon)) out-str))
	      (setf chars-processed (+nil 2 end-reigon)))))))

(defun transcribe-string (english-str)
  (with-output-to-string (hebrew-str)
    (let (buf (read-position 0) (end-position (length english-str)))
      (loop while (< read-position end-position)
	   do (if (setf buf (gethash
			     (subseq english-str read-position
				     (if (> (+ 2 read-position) end-position)
					 nil
					 (+ 2 read-position)))
			     *transcription*))
		  (progn (princ buf hebrew-str)
			 (setf read-position (+ 2 read-position)))
		  (progn (princ
			  (or (gethash (char english-str read-position) *transcription*)
			      (char english-str read-position))
			  hebrew-str)
			 (setf read-position (1+ read-position))))))))