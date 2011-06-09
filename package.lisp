(in-package #:cl-user)

(defpackage cl-inotify
  (:use #:cl #:cffi)
  (:export ;;; used types for documentation
	   #:inotify-add/read-flag
	   #:inotify-read-flag
	   #:inotify-add-flag

	   ;;; very raw
	   #:read-raw-event-from-stream

	   #:close-inotify

	   ;;; event parsing functions
	   #:make-unregistered-inotify
	   #:read-event-from-stream
	   #:watch-raw
	   #:unwatch-raw

	   ;;; enhanced functionality
	   #:make-inotify
	   #:watchedp
	   #:watch
	   #:unwatch
	   #:event-availablep
	   #:read-event
	   #:next-event

	   ;;; convenience functions
	   #:list-watched
	   #:do-events
	   #:next-events)
  (:documentation "A binding (not only?) for the LINUX inotify(7) API."))
