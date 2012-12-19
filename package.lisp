;;; -*- mode: lisp; syntax: common-lisp; coding: utf-8; package: cl-user; -*-

;; Copyright (c) 2011-12, Olof-Joachim Frahm
;; All rights reserved.

;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions
;; are met:

;; 1. Redistributions of source code must retain the above copyright
;; notice, this list of conditions and the following disclaimer.

;; 2. Redistributions in binary form must reproduce the above copyright
;; notice, this list of conditions and the following disclaimer in the
;; documentation and/or other materials provided with the distribution.

;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;; "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;; LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
;; A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
;; OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
;; SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
;; LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
;; DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
;; THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
;; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
;; OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

(in-package #:cl-user)

(defpackage cl-inotify
  (:use #:cl #:cffi)
  (:export ;;; used types for documentation
	   #:inotify-add/read-flag
	   #:inotify-read-flag
	   #:inotify-add-flag

	   ;;; very raw
	   #:read-raw-event-from-stream

	   ;;; basic stuff
	   #:close-inotify

	   ;;; inotify accessors
	   #:inotify-fd
	   #:inotify-stream
	   #:inotify-nonblocking

	   ;;; event parsing functions
	   #:make-unregistered-inotify
	   #:read-event-from-stream
	   #:watch-raw
	   #:unwatch-raw

	   ;;; event accessors
	   #:inotify-event-wd
	   #:inotify-event-mask
	   #:inotify-event-cookie
	   #:inotify-event-name

	   ;;; enhanced functionality
	   #:make-inotify
	   #:pathname-handle/flags
	   #:event-pathname/flags
	   #:watch
	   #:unwatch
	   #:event-availablep
	   #:read-event
	   #:next-event

	   ;;; convenience functions
	   #:list-watched
	   #:do-events
	   #:next-events

	   ;;; macros
	   #:with-inotify
	   #:with-unregistered-inotify
	   #:with-inotify-event-handler

	   #:run-inotify-program)
  (:documentation "A binding (not only?) for the LINUX inotify(7) API."))
