;;;; fors-lic.asd

(defsystem #:fors-lic
  :description "Describe fors-lic here"
  :author "Nick Matvyeyev <mnasoft@gmail.com>"
  :license "GNU GENERAL PUBLIC LICENSE Version 3, 29 June 2007 or later"  
  :serial t
  :depends-on (#:ltk #:half-div #:math/appr)
  :components (
	       (:file "package")
               (:file "fors-lic")
	       )
  :depends-on (:math :half-div)
  )

