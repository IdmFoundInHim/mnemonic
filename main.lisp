#| Core functionality

Exports:
(fun) transcription-reigon-handler: takes user input as parameter, outputs
       transcribed text

Major private functions:
(macro) populate-transcription-table: builds up hash table used for transcribing
         text
transcribe-string: Used by transcription-reigon-handler
|#

(in-package :idmfoundinhim.mnemonic-transcription)

(defparameter *transcription* (make-hash-table :test #'equal))

(defun transcribe-string (english-str)
  "Transcribes parameter string with ability to look one character ahead"
  ;; The output string is everything printed to the HEBREW-STReam
  (with-output-to-string (hebrew-str)
    ;; Uses the read-head model so that either one or two characters can be
    ;; read at a time. Also uses buffer to limit to one hash-table lookup per
    ;; character.
    (let (buf (read-position 0) (end-position (length english-str)))
      (loop while (< read-position end-position)
	   do (if (setf buf (gethash
			     (subseq english-str read-position
				     (min (+ 2 read-position) end-position))
			     *transcription*))
		  (progn (princ buf hebrew-str) ; Appends to output string
			 (setf read-position (+ 2 read-position)))
		  (progn (princ (or
				 ;; Hash table lookup returns NIL if the key is
				 ;; not in the table
				 (gethash (char english-str read-position)
					  *transcription*)
				 (char english-str read-position))
				;; Appends to output
				hebrew-str)
			 (setf read-position (1+ read-position))))))))

(defun transcription-reigon-handling (in-str)
  "Breaks up alternating reigons to be transcribed and those to be skipped

  Looks for double colon to designate new reigons, starts in transcribed reigon"
  (with-output-to-string (out-str)
    (do*
     (;; Alternates modes of processing the substring
      (mode #'transcribe-string (if (eq #'transcribe-string mode)
				    (lambda (s) s) ; no transcription
				    #'transcribe-string))
      ;; #'subseq can handle indices up to *and including* the length of the
      ;; string. Since the double colon is two characters long, I can safely add
      ;; 2 to the index (which points to the first of the two colons).
      (chars-processed 0 (+ 2 next-double-colon))
      (next-double-colon (search "::" in-str)
			 (search "::" in-str :start2 (+ 2 next-double-colon))))
     ;; The loop runs as long as there is another double colon, and then
     ;; runs one last time for the chars after the last double colon
     ((not next-double-colon)
      (princ (funcall mode (subseq in-str chars-processed next-double-colon))
	     out-str))
      (princ (funcall mode (subseq in-str chars-processed next-double-colon))
	     out-str))))

(defmacro populate-transcription-table (&rest alternating-keys-values)
  "Automatically generates setf forms for *transcription* character mapping"
  `(progn
     ,@(loop for index-value
	  from 1 below (length alternating-keys-values)
	  by 2
	  collect `(setf (gethash ,(elt alternating-keys-values
					(- index-value 1))
				  *transcription*)
			 ,(elt alternating-keys-values index-value)))))

(populate-transcription-table
 #\j #.(code-char 1488)
 #\b #.(code-char 1489)
 #\g #.(code-char 1490)
 #\d #.(code-char 1491)
 #\h #.(code-char 1492)
 #\v #.(code-char 1493)
 #\z #.(code-char 1494)
 #\H #.(code-char 1495)
 #\c #.(code-char 1496)
 #\y #.(code-char 1497)
 #\k #.(code-char 1499)
 #\l #.(code-char 1500)
 #\m #.(code-char 1502)
 #\n #.(code-char 1504)
 #\s #.(code-char 1505)
 #\J #.(code-char 1506)
 #\f #.(code-char 1508)
 #\x #.(code-char 1510)
 #\q #.(code-char 1511)
 #\r #.(code-char 1512)
 #\w #.(code-char 64298)
 "ww" #.(code-char 64299)
 #\t #.(code-char 1514)

 #\B #.(code-char 64305)
 #\G #.(code-char 64306)
 #\D #.(code-char 64307)
 "hh" #.(code-char 64308)
 #\V #.(code-char 64309)
 "vO" #.(code-char 64331)
 #\Z #.(code-char 64310)
 #\C #.(code-char 64312)
 #\Y #.(code-char 64313)
 #\K #.(code-char 64315)
 "k " #.(code-char 1498)
 "K " #.(code-char 64314)
 #\L #.(code-char 64316)
 #\M #.(code-char 64318)
 "m " #.(code-char 1501)
 #\N #.(code-char 64320)
 "n " #.(code-char 1503)
 #\S #.(code-char 64321)
 #\F #.(code-char 64324)
 #\p #.(code-char 64324)
 "f " #.(code-char 1507)
 "p " #.(code-char 64323)
 "F " #.(code-char 64323)
 #\X #.(code-char 64326)
 "x " #.(code-char 1509)
 #\Q #.(code-char 64327)
 #\W #.(code-char 64300)
 "WW" #.(code-char 64301)
 ":w" #.(code-char 1428)
 ":W" #.(code-char 64329)
 #\T #.(code-char 64330)

 #\; #.(code-char 1456)
 ";e" #.(code-char 1457)
 ";a" #.(code-char 1458)
 ";o" #.(code-char 1459)
 #\i #.(code-char 1460)
 #\E #.(code-char 1461)
 #\e #.(code-char 1462)
 #\a #.(code-char 1463)
 #\A #.(code-char 1464)
 #\O #.(code-char 1465)
 #\u #.(code-char 1467)
 #\I #.(format nil "~a~a" (code-char 1460) (code-char 1497))
 #\U #.(code-char 64309)

 ":d" #.(code-char 1468)
 ":m" #.(code-char 1468)
 ": " "")
