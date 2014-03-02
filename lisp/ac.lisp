(defpackage #:com.larevivalist.scraper.american-cinematheque
  (:nicknames #:larev.ac)
  (:use #:cl 
	#:com.larevivalist.scraper.utils
	#:hk.utils.thrush
	#:split-sequence)
  (:export #:scrape-shows))

(in-package #:com.larevivalist.scraper.american-cinematheque)

(defvar *cal-url* "http://americancinemathequecalendar.com/calendar")

(defvar *cal-dom* nil)

(defun get-next-months-calendar-dom (dom)
  (-> dom
    (ws:find-first :class "date-next")
    (ws:find-first :tag :a)
    (ws:a-href)
    (ws:get-raw-dom)))

(defun get-dom (&optional (url *cal-url*))
  (or *cal-dom*
      (-> url 
	(ws:get-raw-dom)
	(push *cal-dom*)
	(car)
	(get-next-months-calendar-dom)
	(push *cal-dom*))))
	  



(defun get-month-year (dom)
  (let ((month-year 
	 (split-sequence 
	  #\space
	  (-> dom
	    (ws:find-first :class "date-heading")
	    (ws:find-first :tag :h3)
	    (ws:get-text)))))
    (cons (get-month-num (car month-year))
	  (cdr month-year))))
	   
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

(defun get-date (entry dom)
  (destructuring-bind (month year) (get-month-year dom)
    (format-date (strip-whitespace 
		  (ws:get-text (ws:find-first entry :class "day")))
		 :month month
		 :year year)))

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
        for date = (get-date entry dom)
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
  (loop for cal-dom in (get-dom)
        append (collect-showtimes cal-dom)))