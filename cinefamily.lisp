(defpackage #:com.larevivalist.scraper.cinefamily
  (:nicknames #:larev.cf)
  (:use #:cl #:com.larevivalist.scraper.utils)
  (:export #:scrape-shows))

(in-package #:com.larevivalist.scraper.cinefamily)

(defvar *cf-cal-url* "http://www.cinefamily.org/#calendar")
(defvar *cf-cal-dom* nil)

(defun get-dom (&optional (url *cf-cal-url*))
  (if *cf-cal-dom*
      *cf-cal-dom*
      (setf *cf-cal-dom*
	    (ws:get-processed-dom url))))

(defun get-calendar-entries (dom)
  (remove-if-not (lambda (entry)
		   (and (not (ws::get-attribs entry))
			;(ws:find-first entry :tag :small)
			))
		 (ws:find-all dom :tag :td)))

(defun get-showtimes (entry)
  "Returns an ALIST of a show title along with its time on a given
   calendar date entry"
  (mapcar #'list
	  (remove nil
		  (mapcar #'ws:get-text
			  (ws:find-all entry :class "event-block")))
	  (mapcar #'ws:get-text (ws:find-all entry :tag :small))))

(defun get-day (entry &key (month *month*) (year *year*))
  (ws:get-text (ws:find-first entry :class "dayHead")))

(defun format-showtimes (entry)
  (let ((date (format-date (get-day entry)))
	(showtimes (get-showtimes entry)))
    (loop for showtime in showtimes
          collect `(:title ,(first showtime) 
		    :date ,date 
                    :time ,(second showtime) 
                    :theatre "Cinefamily"))))

(defun collect-showtimes (cal-dom)
  (loop for entry in (get-calendar-entries cal-dom)
        for shows = (format-showtimes entry)
        append shows))

(defun scrape-shows ()
  (collect-shows (get-dom)))