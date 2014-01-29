(defpackage #:com.larevivalist.scraper.cinefamily
  (:nicknames #:larev.cf)
  (:use #:cl 
	#:com.larevivalist.scraper.utils
	#:hk.utils.thrush
	#:hk.utils.pred)
  (:export #:scrape-shows))

(in-package #:com.larevivalist.scraper.cinefamily)

(defvar *cf-cal-url* "http://www.cinefamily.org/#calendar")
(defvar *cf-cal-dom* nil)

(defun get-dom (&optional (url *cf-cal-url*))
  (or *cf-cal-dom*
      (setf *cf-cal-dom*
	    (ws:get-processed-dom url))))

(defun get-calendar-entries (dom &key (ignore-past-entries t))
  (remove-if-not (lambda (entry)
		   (and (not (ws::get-attribs entry))
			(or ignore-past-entries 
			    (ws:find-first entry :tag :small))))
		 (ws:find-all dom :tag :td)))

(defun get-showtimes (entry)
  "Returns an ALIST of a show title along with its time on a given
   calendar date entry"
  (mapcar #'list
	  (remove nil
		  (mapcar #'ws:get-text
			  (ws:find-all entry :class "event-block")))
	  (mapcar #'ws:get-text (ws:find-all entry :tag :small))))

(defun get-links (entry)
  (mapcar #'ws:a-href
	  (ws:find-all entry :tag :a)))

(defun get-month-year (dom)
  (let ((month-year 
	 (->>
	     (-> dom
	       (ws:find-first :attrib '((:id "EC_current-month")))
	       (ws:find-first :tag :h2)
	       (ws:get-text))
	   (mapcar (alexandria:curry #'remove-if #'space-char-p)))))
    (cons (get-month-num (car month-year))
	  (cdr month-year))))
    
(defun get-day (entry)
  (ws:get-text (ws:find-first entry :class "dayHead")))


(defun format-showtimes (entry month year)
  (loop with date = (format-date (get-day entry) :month month :year year)
        for showtime in (get-showtimes entry)
        for link in (get-links entry)
        collect (make-show :title (first showtime) 
			   :date date 
			   :time (second showtime) 
			   :theatre "Cinefamily"
			   :link link)))

(defun collect-showtimes (cal-dom ignore-past)
  (loop with (month year) = (get-month-year cal-dom)
        for entry in (get-calendar-entries cal-dom 
					   :ignore-past-entries ignore-past)
        for shows = (format-showtimes entry month year)
        append shows))

(defun scrape-shows (&key (ignore-past-shows t))
  (loop for cal-dom = (get-dom)
        append (collect-showtimes cal-dom ignore-past-shows)))