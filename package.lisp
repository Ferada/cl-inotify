(in-package #:cl-user)

(defpackage cl-notify
  (:use #:cl #:cffi)
  (:export #:inotify-event
	   #:inotify-event-wd
	   #:inotify-event-mask
	   #:inotify-event-cookie
	   #:inotify-event-name

	   #:inotify-read-raw-event
	   #:inotify-read-event
	   #:make-inotify
	   #:close-notify
	   #:watch
	   #:unwatch
	   ))
