(defpackage #:com.larevivalist.scraper.utils
  (:nicknames #:larev.utils)
  (:use #:cl)
  (:export #:*month*
	   #:*year*
	   #:strip-whitespace
	   #:format-date
	   #:get-day-of-the-week
	   #:make-show
	   #:create-csv-file))

(in-package #:com.larevivalist.scraper.utils)

;;; ************************************************************
;;; Misc
;;; ************************************************************

(defun strip-whitespace (str)
  (remove-if (lambda (c) (or (char= c #\space) 
			     (char= c #\tab) 
			     (char= c #\newline)))
                 str))

;;; ************************************************************
;;; Date & Time
;;; ************************************************************

(defvar *month* 1)
(defvar *year* 2014)

(defun make-calendar (&key leap-year)
  "Returns a CADR-valued ALIST of months with their # of days"
  `((1 31)
    (2 ,(if leap-year 29 28))
    (3 31)
    (4 30)
    (5 31)
    (6 30)
    (7 31)
    (8 31)
    (9 30)
    (10 31)
    (11 30)
    (12 31)))

(defun make-week (&optional (first-day 'sunday))
  "Returns a circular generator of days of the week"
  (let* ((week-days '(sunday monday tuesday wednesday thursday friday saturday))
	 (week-coro
	  (cl-coop:with-coroutine ()
	    (do () (nil)
	      (dolist (day week-days)
		(cl-coop:yield day)))))
	 (begin t))
    (loop until (eql (funcall week-coro) first-day))
    (lambda ()
      (if begin
	  (progn (setf begin nil) first-day)
	  (funcall week-coro)))))
    
(defun get-day-of-the-week (month day &key (year-first-day 'wednesday) leap-year)
  "Returns which day of the week the given date falls on, calculated from
   what day of the week the year begins on (Wednesday for 2014)."
  (let ((week (make-week year-first-day)))
    (loop for (m num-days) in (make-calendar :leap-year leap-year)
	  do (loop for date from 1 to num-days
		   for week-day = (funcall week)
		   when (and (= m month)
			     (= date day))
		     do (return-from get-day-of-the-week week-day)))))
		   

(defun format-date (day &key (month *month*) (year *year*))
  (format nil "~a/~a/~a" month day year))

;;; ************************************************************
;;; Show Record
;;; ************************************************************

(defun make-show (&key title date time theatre link)
  "Constructor for show record"
  `(:title ,title
    :date ,date
    :time ,time
    :theatre ,theatre
    :link ,link))

(defmacro define-show-accessor (name)
  "(define-show-accessor title)
   =>
   (defun get-title (show) ...)"
  (let ((show-arg (gensym))
	(acc-name (alexandria:symbolicate 'get- name))
	(acc-key (intern (string name) "KEYWORD")))
    `(progn
       (declaim (inline ,acc-name))
       (defun ,acc-name (,show-arg)
	 (getf ,show-arg ,acc-key)))))

(define-show-accessor title)
(define-show-accessor date)
(define-show-accessor time)
(define-show-accessor theatre)
(define-show-accessor link)

;;; ************************************************************
;;; CSV
;;; ************************************************************

(defparameter *csv-header* 
  "Subject,Start Date,Start Time,Description,Location"
  "CSV header for Google Calendar")

(defun show->csv (show)
  "Converts show record to CSV string for export to Google Calendar"
  (format nil 
	  "\"~a @ ~a\",~a,~a,\"~a\",\"~a\""
	  (get-title show)
	  (get-theatre show)
	  (get-date show)
	  (get-time show)
	  (get-link show)
	  (get-theatre show)))

(defun create-csv-file (shows filepath)
  "Creates a CSV file from a list of show records"
  (with-open-file (csv-file
		   filepath
		   :direction :output
		   :if-exists :overwrite
		   :if-does-not-exist :create)
    (format csv-file "~a~%" *csv-header*)
    (dolist (show shows)
      (format csv-file "~a~%" (show->csv show)))))