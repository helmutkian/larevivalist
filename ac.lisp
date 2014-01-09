(defvar *ac-cal-dom* (ws:get-processed-dom "http://americancinemathequecalendar.com/calendar"))

(defvar *month* 1)
(defvar *year* 2014)

(defun get-non-empty-calendar-entries (dom)
  (remove-if (lambda (node)
	       (ws:find-first node :class "calendar-empty"))
	     (ws:find-all dom :class "inner")))

(defun strip-whitespace (str)
  (remove-if-not (lambda (c) (or (digit-char-p c) (alpha-char-p c)))
		 str))

(defun format-date (day-str)
  (concatenate 'string 
	       (write-to-string *month*)
	       "/"
	       (strip-whitespace day-str)
	       "/"
	       (write-to-string *year*)))

(defun format-theatre (event-str)
  (if (string= (second (split-sequence:split-sequence #\space event-str)) 
	       "EGYPTIAN")
      "Egyptian Theatre"
      "Aero Theatre"))

(defun format-showtime (entry)
  (let ((date (format-date (ws:text (ws:find-first entry :class "day"))))
	(title (ws:text (ws:find-first entry :tag :a)))
        (time (ws:text (ws:find-first entry :class "date-display-single")))
        (theatre (format-theatre 
		  (ws:get-attrib :title
				 (ws:find-first entry
						:class "stripe")))))
    `(:title ,title :date ,date :time ,time :theatre ,theatre)))

(defun collect-showtimes (cal)
  (loop for entry in (get-non-empty-calendar-entries cal)
        collect (format-showtime entry)))