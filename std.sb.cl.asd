;;;; 
;;
;;;; 

(asdf:defsystem #:std.sb.cl
  :description "A common standard library (SBCL)."
  :author "Mikhail Zislis <mikhail.zislis@gmail.com>"
  :license  "MIT License"
  :version "0.0.1"
  :serial t
  :components
  
  ((:module "base"
    :pathname ""
    :components ((:file "package")
                 (:file "util" :depends-on ("package"))))
   (:module "text"
    :components ((:file "package")
                 (:file "text" :depends-on ("package")))
    :depends-on ("base"))
   (:module "text.test"
    :components ((:file "package")
                 (:file "test" :depends-on ("package")))
    :depends-on ("text")))
  
  :depends-on ("fiveam"))
