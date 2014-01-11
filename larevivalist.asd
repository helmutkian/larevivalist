(asdf:defsystem #:larevivalist
  :depends-on ("cl-web-scrape")
  :components ((:file "utils")
	       (:file "ac"
		      :depends-on ("utils"))
	       (:file "cinefamily"
		      :depends-on ("utils"))))