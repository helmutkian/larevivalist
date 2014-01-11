(defpackage #:com.larevivalist.scraper.american-cinemateque
  (:nicknames #:larev.ac)
  (:use #:cl #:com.larevivalist.scraper.utils)
  (:export #:collect-showtimes))

(in-package #:com.larevivalist.scraper.american-cinemateque)

(defvar *ac-cal-dom* (ws:get-processed-dom "http://americancinemathequecalendar.com/calendar"))

(defun get-non-empty-calendar-entries (dom)
  (remove-if (lambda (node)
	       (ws:find-first node :class "calendar-empty"))
	     (ws:find-all dom :class "inner")))

(defun get-theatre (event-str)
  (if (string= (second (split-sequence:split-sequence #\space event-str)) 
	       "EGYPTIAN")
      "Egyptian Theatre"
      "Aero Theatre"))

(defun format-showtime (entry)
  (let ((date (format-date (ws:text (ws:find-first entry :class "day"))))
	(title (ws:text (ws:find-first entry :tag :a)))
        (time (ws:text (ws:find-first entry :class "date-display-single")))
        (theatre (get-theatre 
		  (ws:get-attrib :title
				 (ws:find-first entry
						:class "stripe")))))
    `(:title ,title :date ,date :time ,time :theatre ,theatre)))

(defun collect-showtimes (cal)
  (loop for entry in (get-non-empty-calendar-entries cal)
        collect (format-showtime entry)))