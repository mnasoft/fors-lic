;;;; fors-lic.asd

(asdf:defsystem #:fors-lic
  :description "Describe fors-lic here"
  :author "Your Name <your.name@example.com>"
  :license "Specify license here"
  :serial t
  :components (
	       (:file "package")
               (:file "fors-lic")
	       )
  :depends-on (:math :half-div)
  )

