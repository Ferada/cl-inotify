(in-package #:cl-user)

(defpackage cl-notify
  (:use #:cl #:cffi #:utils-frahm #:anaphora)
  (:export ;;; used types for documentation
	   #:inotify-add/read-flag
	   #:inotify-read-flag
	   #:inotify-add-flag

	   #:close-notify

	   ;;; event parsing functions
	   #:make-unregistered-notify
	   #:read-raw-event
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
  (:documentation "A binding for the LINUX inotify(7) API."))
