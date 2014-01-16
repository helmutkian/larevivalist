(defpackage #:com.larevivalist.scraper.utils
  (:nicknames #:larev.utils)
  (:use #:cl)
  (:export #:*month*
	   #:*year*
	   #:strip-whitespace
	   #:format-date
	   #:make-show))

(in-package #:com.larevivalist.scraper.utils)

(defvar *month* 1)
(defvar *year* 2014)

(defun strip-whitespace (str)
  (remove-if-not (lambda (c) (or (digit-char-p c) (alpha-char-p c)))
		 str))

(defun format-date (day &key (month *month*) (year *year*))
  (format nil "~a/~a/~a" month day year))

(defun make-show (&key title date time theatre)
  `(:title ,title
    :date ,date
    :time ,time
    :theatre ,theatre))


