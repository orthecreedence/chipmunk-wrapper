(asdf:defsystem :chipmunk-wrapper
  :author "Andrew Lyon <orthecreedence@gmail.com>"
  :licence "MIT"
  :version "0.1.0"
  :depends-on (#:cffi #:clipmunk)
  :components ((:file "package")
			   (:file "config" :depends-on ("package"))
               (:file "base" :depends-on ("config"))
			   (:file "body" :depends-on ("base"))
			   (:file "shape" :depends-on ("body"))
			   (:file "joint" :depends-on ("body"))
			   (:file "space" :depends-on ("body" "joint"))))
