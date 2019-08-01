;;;; jamais-vu.asd

(asdf:defsystem #:jamais-vu
  :description "A Looper."
  :author "Nuno Trocado <http://nunotrocado.com>"
  :license  "Specify license here"
  :version "0.0.1"
  :serial t
  :depends-on (#:qtools
	       #:qtcore
	       #:qtgui
	       #:alexandria
	       #:cl-collider
	       #:usocket-server
	       #:cl-sndfile)
  :components ((:file "package")
               (:file "jamais-vu")
	       (:file "gui")))
