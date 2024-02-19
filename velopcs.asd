(defsystem "velopcs"
  :version "0.0.1"
  :author "goosecoid"
  :license "MIT"
  :depends-on (:dexador
               :lquery
               :str
               :alexandria
               :cl-slug
               :jonathan
               :clack
               :ningle
               :spinneret
               )
  :components ((:module "src"
                :components
                ((:file "package")
                 (:file "fetcher")
                 (:file "server"))))
  :description "Calculate your ultimate meme veloteam"
  :in-order-to ((test-op (test-op "velopcs/tests"))))
