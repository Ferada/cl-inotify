(in-package #:cl-user)

(eval-when (:load-toplevel :execute)
  (asdf:operate 'asdf:load-op 'cffi-grovel))

(asdf:defsystem cl-inotify
  :depends-on (#:cffi #:binary-types)
  :serial T
  :components ((:file "package")
	       (cffi-grovel:grovel-file "grovel")
	       (:file "inotify")))
