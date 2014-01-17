(defpackage #:com.larevivalist.scraper.american-cinematheque
  (:nicknames #:larev.ac)
  (:use #:cl #:com.larevivalist.scraper.utils)
  (:export #:scrape-shows))

(in-package #:com.larevivalist.scraper.american-cinematheque)

(defvar *ac-cal-url* "http://americancinemathequecalendar.com/calendar")

(defvar *ac-cal-dom* nil)

(defun get-dom (&optional (url *ac-cal-url*))
  (if *ac-cal-dom*
      *ac-cal-dom*
      (setf *ac-cal-dom* (ws:get-raw-dom url))))

(defun get-non-empty-calendar-entries (dom)
  (remove-if (lambda (node)
	       (ws:find-first node :class "calendar-empty"))
	     (ws:find-all dom :class "inner")))

(defun get-theatres (entry)
  (let ((theatre-strs (mapcar (lambda (x) (ws:get-attrib :title x))
			      (ws:find-all entry :class "stripe"))))
    (mapcar (lambda (str)
	      (if (string= str "Key: EGYPTIAN EVENT")
		  "Egyptian Theatre"
		  "Aero Theatre"))
	    theatre-strs)))

(defun get-date (entry)
  (format-date (strip-whitespace 
		(ws:get-text (ws:find-first entry :class "day")))))

(defun get-titles (entry)
  (mapcar #'ws:get-text
	  (ws:find-all entry :tag :a)))

(defun get-times (entry)
  ;; Google Calendar requires a space between the time digits and the
  ;; am/pm designation
  (mapcar (lambda (tree) 
	    (let* ((text (ws:get-text tree))
		   (am-pm-pos (position-if #'alpha-char-p text)))
	      (concatenate 'string
			   (subseq text 0 am-pm-pos)
			   " "
			   (subseq text am-pm-pos))))
	  (ws:find-all entry :class "date-display-single")))

(defun get-links (entry)
  (let ((root-url "http://www.americancinemathequecalendar.com")
	(a-nodes (ws:find-all entry :tag :a)))
    (mapcar (lambda (node)
	      (concatenate 'string
			   root-url
			   (ws:a-href node)))
	    a-nodes)))

(defun collect-showtimes (dom)
  (loop with showtimes = (get-non-empty-calendar-entries dom)
        for entry in showtimes
        for date = (get-date entry)
        append (loop for title in (get-titles entry)
		     for time in (get-times entry)
		     for theatre in (get-theatres entry)
		     for link in (get-links entry)
		     collect (make-show :title title
					:date date
					:time time
					:theatre theatre
					:link link))))

(defun scrape-shows ()
  (collect-showtimes (get-dom)))