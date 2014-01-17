(asdf:defsystem #:larevivalist
  :depends-on ("cl-web-scrape" "alexandria")
  :components ((:file "utils")
	       (:file "ac"
		      :depends-on ("utils"))
	       (:file "cinefamily"
		      :depends-on ("utils"))))