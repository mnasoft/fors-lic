;;;; fors-lic.asd

(defsystem "fors-lic"
  :description "Система предназначена для выполнения расчета перепадов
  давления топлива и расхода топлива через каналы форсунки подачи
  жидкого топлива."
  :author  "Nick Matvyeyev <mnasoft@gmail.com>"
  :license "GNU GENERAL PUBLIC LICENSE Version 3, 29 June 2007 or later"  
  :serial t
  :depends-on ("ltk" "half-div" "math")
  :components ((:module "src"
		:serial nil
                :components ((:file "fors-lic")))))

(defsystem "fors-lic/docs"
  :description "Зависимости для сборки документации"
  :author "Nick Matvyeyev <mnasoft@gmail.com>"
  :license "GNU GENERAL PUBLIC LICENSE Version 3, 29 June 2007 or later"  
  :depends-on ("fors-lic" "mnas-package" "codex"))
