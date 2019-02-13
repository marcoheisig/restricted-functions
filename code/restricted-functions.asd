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

  :serial t
  :components
  ((:file "packages")
   (:file "function-lambda-lists")
   (:file "generic-functions")
   (:file "default-methods")
   (:file "restricted-function")
   (:file "simplified-type-inference")))
