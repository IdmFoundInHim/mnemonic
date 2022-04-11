(defparameter *transcription* (make-hash-table :test #'equal))
(setf (gethash #\j *transcription*) #\Hebrew_Letter_Alef)
(setf (gethash #\b *transcription*) #\Hebrew_Letter_Bet)
(setf (gethash #\a *transcription*) #\Hebrew_Point_Patah)
(setf (gethash ":d" *transcription*) #\Hebrew_Point_Dagesh_Or_Mapiq)
(setf (gethash #\; *transcription*) #\Hebrew_Point_Sheva)
(setf (gethash ";e" *transcription*) #\Hebrew_Point_Hataf_Segol)
(setf (gethash #\e *transcription*) #\Hebrew_Point_Segol)
(setf (gethash ": " *transcription*) "")

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