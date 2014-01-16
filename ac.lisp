(defpackage #:com.larevivalist.scraper.american-cinematheque
  (:nicknames #:larev.ac)
  (:use #:cl #:com.larevivalist.scraper.utils)
  (:export #:collect-showtimes))

(in-package #:com.larevivalist.scraper.american-cinematheque)

(defvar *ac-cal-dom* (ws:get-raw-dom "http://americancinemathequecalendar.com/calendar"))

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
  (mapcar #'ws:get-text
	  (ws:find-all entry :class "date-display-single")))

(defun collect-showtimes (dom)
  (loop with showtimes = (get-non-empty-calendar-entries dom)
        for entry in showtimes
        for date = (get-date entry)
        append (loop for title in (get-titles entry)
		     for time in (get-times entry)
		     for theatre in (get-theatres entry)
		     collect `(:title ,title
			       :date ,date
			       :time ,time
			       :theatre ,theatre))))