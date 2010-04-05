(in-package #:cl-user)

(defpackage cl-inotify
  (:use #:cl #:cffi)
  (:export ;;; used types for documentation
	   #:inotify-add/read-flag
	   #:inotify-read-flag
	   #:inotify-add-flag

	   ;;; very raw
	   #:read-raw-event-from-stream

	   #:close-notify

	   ;;; event parsing functions
	   #:make-unregistered-notify
	   #:read-event-from-stream
	   #:watch-raw
	   #:unwatch-raw

	   ;;; enhanced functionality
	   #:make-notify
	   #:watchedp
	   #:watch
	   #:unwatch
	   #:event-availablep
	   #:read-event
	   #:next-event

	   ;;; convenience functions
	   #:list-watched
	   #:do-events
	   #:read-events
	   )
  (:documentation "A binding (not only?) for the LINUX inotify(7) API."))
