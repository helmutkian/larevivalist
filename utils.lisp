(defpackage #:com.larevivalist.scraper.utils
  (:nicknames #:larev.utils)
  (:use #:cl)
  (:export #:*month*
	   #:*year*
	   #:strip-whitespace
	   #:format-date
	   #:make-show
	   #:create-csv-file))

(in-package #:com.larevivalist.scraper.utils)

(defvar *month* 1)
(defvar *year* 2014)

(defun strip-whitespace (str)
  (remove-if-not (lambda (c) (or (digit-char-p c) (alpha-char-p c)))
		 str))

(defun format-date (day &key (month *month*) (year *year*))
  (format nil "~a/~a/~a" month day year))

(defun make-show (&key title date time theatre)
  "Constructor for show record"
  `(:title ,title
    :date ,date
    :time ,time
    :theatre ,theatre))

(defmacro define-show-accessor (name)
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

(defun csv-header ()
  "Returns CSV header for Google Calendar"
  "Subject,Start Date,Start Time,Location")

(defun show->csv (show)
  "Converts show record to CSV string for export to Google Calendar"
  (format nil 
	  "\"~a\",~a,~a,~a"
	  (get-title show)
	  (get-date show)
	  (get-time show)
	  (get-theatre show)))

(defun create-csv-file (shows filepath)
  "Creates a CSV file from a list of show records"
  (with-open-file (csv-file
		   filepath
		   :direction :output
		   :if-exists :overwrite
		   :if-does-not-exist :create)
    (format csv-file "~a~%" (csv-header))
    (dolist (show shows)
      (format csv-file "~a~%" (show->csv show)))))