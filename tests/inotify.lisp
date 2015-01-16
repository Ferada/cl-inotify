;; -*- mode: lisp; syntax: common-lisp; coding: utf-8-unix; package: cl-inotify-tests; -*-

;; Copyright (c) 2011-15, Olof-Joachim Frahm
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

(in-package #:cl-inotify-tests)

(in-suite cl-inotify)

;; note that for all of these tests other programs might interfere, so
;; they might fail for no apparent reason

(def-test read-raw-event-from-stream.1 ()
  (let ((tmp (osicat-posix:mkdtemp
              (concatenate 'string
                           (namestring osicat:*temporary-directory*)
                           "cl-inotify-XXXXXX"))))
    (with-unregistered-inotify (inotify T (tmp :all-events))
      (ensure-directories-exist (concatenate 'string tmp "/foo/"))
      (unwind-protect
           (let ((available (event-available-p inotify)))
             (is-true available "No event was read")
             (when available
               (let ((event (read-raw-event-from-stream (inotify-stream inotify))))
                 (is-true event "No event was read")
                 (is (equal "foo" (inotify-event-name event))))))
        (osicat:delete-directory-and-files tmp)))))

(def-test watch.recorded ()
  "A single watch is recorded and the returned handle is valid."
  (with-inotify (inotify)
    (is (null (list-watched inotify)))
    (is (integerp (watch inotify #P"." :all-events)))
    (is (equal '(#P".") (list-watched inotify)))))

(def-test unwatch.pathname ()
  "Unwatching by pathname works."
  (with-inotify (inotify)
    (is (null (list-watched inotify)))
    (watch inotify #P"." :all-events)
    (is (not (null (list-watched inotify))))
    (unwatch inotify :pathname #P".")
    (is (null (list-watched inotify)))))

(def-test unwatch.handle ()
  "Unwatching by handle works."
  (with-inotify (inotify)
    (is (null (list-watched inotify)))
    (let ((handle (watch inotify #P"." :all-events)))
      (is (not (null (list-watched inotify))))
      (unwatch inotify :handle handle))
    (is (null (list-watched inotify)))))

(def-test unwatch.no-arguments ()
  "Unwatching needs exactly one of three keywords."
  (with-inotify (inotify)
    (signals error
      (unwatch inotify))))

(def-test unwatch.two-arguments ()
  "Unwatching needs exactly one of three keywords."
  (with-inotify (inotify)
    (signals error
      (unwatch inotify :pathname #P"." :handle 42))))
