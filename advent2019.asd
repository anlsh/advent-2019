;;;; advent2019.asd

(asdf:defsystem #:advent2019
  :description "Describe advent2019 here"
  :author "Your Name <your.name@example.com>"
  :license  "Specify license here"
  :version "0.0.1"
  :serial t
  :depends-on (#:volt #:split-sequence)
  :components ((:file "package")
               (:file "computer" :depends-on ("package"))
               (:file "d1" :depends-on ("package"))
               (:file "d2" :depends-on ("package" "computer"))
               (:file "d3" :depends-on ("package"))
               (:file "d4" :depends-on ("package"))
               (:file "d5" :depends-on ("package" "computer"))))
