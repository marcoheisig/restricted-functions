(defsystem "restricted-functions"
  :description "Reasoning about functions calls with restricted argument types."
  :author "Marco Heisig <marco.heisig@fau.de>"
  :license "MIT"

  :depends-on
  ("alexandria"
   "closer-mop"
   "introspect-environment"
   "trivia"
   "trivial-arguments")

  :components
  ((:file "packages")
   (:file "function-lambda-lists")
   (:file "type-inference")
   (:file "protocol")
   (:file "restricted-functions")
   (:file "restrict")

   (:module "inference-rules"
    :components
    ((:file "numbers")))))
