(defpackage #:com.larevivalist.scraper.utils
  (:nicknames #:larev.utils)
  (:uses #:cl)
  (:export #:*month*
	   #:*year*
	   #:strip-whitespace
	   #:format-date))

(in-package #:com.larivialist.scraper.utils)

(defvar *month* 1)
(defvar *year* 2014)

(defun strip-whitespace (str)
  (remove-if-not (lambda (c) (or (digit-char-p c) (alpha-char-p c)))
		 str))

(defun format-date (day &key (month *month*) (year *year*))
  (format nil "~a/~a/~a" month day year))