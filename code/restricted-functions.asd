(defsystem "restricted-functions"
  :description "Reasoning about functions with restricted argument types."
  :author "Marco Heisig <marco.heisig@fau.de>"
  :license "MIT"

  :depends-on
  ("alexandria"
   "closer-mop"
   "simplified-types"
   "trivia"
   "trivial-arguments"
   "trivial-garbage")

  :components
  ((:file "packages")
   (:file "generic-functions")
   (:file "function-lambda-lists")
   (:file "default-methods")
   (:file "infer-type")
   (:file "restricted-function")
   (:file "restricted-function-cache")
   (:file "restrict")))
