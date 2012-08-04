(asdf:defsystem :chipmunk-wrapper
  :author "Andrew Lyon <orthecreedence@gmail.com>"
  :licence "MIT"
  :version "0.1.0"
  :depends-on (#:cffi #:clipmunk)
  :components ((:file "package")
               (:file "base" :depends-on ("package"))
			   (:file "shape" :depends-on ("base"))))
