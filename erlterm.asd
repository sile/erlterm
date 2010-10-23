(in-package :asdf)

(defsystem erlterm
  :name "erlterm"
  :version "0.0.1"
  :author "Takeru Ohta"
  :description "Converting between Erlang term and Common lisp object."
  
  :serial t
  :components ((:file "package")
               (:file "util")
               (:file "constructor")
               (:file "decode")
               (:file "encode")))
