;;;; defender.asd

(asdf:defsystem #:defender
  :description "Describe defender here"
  :author "Your Name <your.name@example.com>"
  :license "Specify license here"
  :depends-on (#:lispbuilder-sdl
               #:lispbuilder-sdl-mixer
               #:lispbuilder-sdl-ttf)
  :serial t
  :components ((:file "package")
               (:file "defender")))

