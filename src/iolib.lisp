;;; -*- mode: lisp; syntax: common-lisp; coding: utf-8-unix; package: cl-inotify; -*-

;; Copyright (c) 2011-13, Olof-Joachim Frahm
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

(in-package #:cl-inotify)

(defun run-inotify-event-handler (watch event-handler &key (nonblocking T) (registered T))
  "Registers an INOTIFY queue and runs EVENT-HANDLER with it as only
parameter whenever input happens."
  (let ((inotify (funcall (if registered #'make-inotify #'make-unregistered-inotify) nonblocking)))
    (unwind-protect
         (iolib:with-event-base (event-base)
           (dolist (watch watch)
             (apply #'watch inotify watch))
           (flet ((events (&rest args)
                    (declare (ignore args))
                    (do-events (event inotify :blocking-p NIL)
                      (funcall event-handler inotify event))))
             (iolib:set-io-handler event-base (inotify-fd inotify) :read #'events)
             (iolib:event-dispatch event-base)))
      (close-inotify inotify))))

(defun run-inotify-program (program args rest directories flags
                            &key event-handler (registered T))
  "Runs a program and records all matched events in all DIRECTORIES using
FLAGS.  If EVENT-HANDLER is set, it is instead called with every available
event.

PROGRAM, ARGS and REST are the arguments to SB-EXT:RUN-PROGRAM.  REST is
passed on verbatim except for the WAIT parameter, which is set to false.

PROGRAM may also be a FUNCTION, in which case it is called with
\(ARGS . REST) as arguments and has to return a process object like from
SB-EXT:RUN-PROGRAM.  The process also shouldn't be spawned with WAIT set.

DIRECTORIES is a list of directory arguments for WATCH/-RAW.

Returns the process structure and if EVENT-HANDLER wasn't set, a LIST of
recorded events as second value."
  (let (events)
    (flet ((events (inotify)
             (do-events (event inotify)
               (if event-handler
                   (funcall event-handler event)
                   (push event events)))))
      (let ((inotify (if registered (make-inotify) (make-unregistered-inotify))))
        (unwind-protect
             (progn
               (let ((register (if registered #'watch #'watch-raw)))
                 (mapcar (lambda (directory)
                           (funcall register inotify directory flags))
                         directories))
               (let ((process (etypecase program
                                (string
                                 (apply #'sb-ext:run-program program args :wait NIL rest))
                                (function
                                 (apply program args rest)))))
                 (loop
                   while (sb-ext:process-alive-p process)
                   do (events inotify))
                 (events inotify)
                 (if event-handler
                     process
                     (values process (nreverse events)))))
          (close-inotify inotify))))))
