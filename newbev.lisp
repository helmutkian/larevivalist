(defpackage #:com.larevivalist.scraper.newbeverly
  (:nicknames #:larev.nb)
  (:use #:cl #:split-sequence)
  (:export #:scrape-shows))

(in-package #:com.larevivalist.scraper.newbeverly)

(defvar *nb-cal-url* "http://www.newbevcinema.com")
(defvar *nb-cal-dom* nil)

(defun get-dom (&optional (url *nb-cal-url*))
  (or *nb-cal-dom*
      (setf *nb-cal-dom* (ws:get-raw-dom url))))

(defun get-calendar-groups (dom)
  (ws:find-all dom :class "calendar_group"))

(defun get-months-&-days (group)
  (hk.utils.thrush:->> 
    (ws:find-first group :class "date")
    (ws:get-text)
    (first)
    (split-sequence #\&)
    (mapcar (lambda (x) (remove-if #'hk.utils.pred:space-char-p x)))
    (mapcar (lambda (d)
	      (let ((pos (position-if #'digit-char-p d)))
		(list (larev.utils:get-month-num (subseq d 0 pos))
		      (split-sequence #\, (subseq d pos))))))))

(defun get-times (showlisting)
  (hk.utils.thrush:->>
    (ws:find-first showlisting :class "time")
    (ws:get-text)
    (remove-if #'hk.utils.pred:space-char-p)
    (split-sequence #\;)
    (mapcar (lambda (x)
	      (let ((pos (position #\: x)))
		(list (split-sequence-if 
		       (lambda (c) 
			 (or (char= c #\/)
			     (char= c #\&)))
		       (subseq x 0 pos))
		      (split-sequence 
		       #\&
		       (subseq x (1+ pos)))))))))
						     
		
(defun get-title (showlisting)
  (ws:get-text (ws:find-first showlisting :tag :a)))

(defun get-days-times (dates showlisting)
  (ws:find-all entry :class "time"))
  

