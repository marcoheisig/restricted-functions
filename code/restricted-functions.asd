(defsystem "restricted-functions"
  :description "Reasoning about functions with restricted argument types."
  :author "Marco Heisig <marco.heisig@fau.de>"
  :license "MIT"

  :depends-on
  ("alexandria"
   "closer-mop"
   "simplified-types"
   "trivia"
   "trivial-arguments")

  :components
  ((:file "packages")
   (:file "function-lambda-lists")
   (:file "protocol")
   (:file "function")
   (:file "restricted-function")
   (:file "simple-restricted-function")
   (:file "type-inference")))
