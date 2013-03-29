;;; -*- mode: lisp; syntax: common-lisp; coding: utf-8-unix; package: cl-inotify; -*-

;; Copyright (c) 2011, Olof-Joachim Frahm
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

(include "sys/inotify.h")

(constant (in-cloexec       "IN_CLOEXEC"))
(constant (in-nonblock      "IN_NONBLOCK"))
(constant (in-access        "IN_ACCESS"))
(constant (in-modify        "IN_MODIFY"))
(constant (in-attrib        "IN_ATTRIB"))
(constant (in-close-write   "IN_CLOSE_WRITE"))
(constant (in-close-nowrite "IN_CLOSE_NOWRITE"))
(constant (in-close         "IN_CLOSE"))
(constant (in-open          "IN_OPEN"))
(constant (in-moved-from    "IN_MOVED_FROM"))
(constant (in-moved-to      "IN_MOVED_TO"))
(constant (in-move          "IN_MOVE"))
(constant (in-create        "IN_CREATE"))
(constant (in-delete        "IN_DELETE"))
(constant (in-delete-self   "IN_DELETE_SELF"))
(constant (in-move-self     "IN_MOVE_SELF"))
(constant (in-unmount       "IN_UNMOUNT"))
(constant (in-q-overflow    "IN_Q_OVERFLOW"))
(constant (in-ignored       "IN_IGNORED"))
(constant (in-onlydir       "IN_ONLYDIR"))
(constant (in-dont-follow   "IN_DONT_FOLLOW"))
(constant (in-mask-add      "IN_MASK_ADD"))
(constant (in-isdir         "IN_ISDIR"))
(constant (in-oneshot       "IN_ONESHOT"))
(constant (in-all-events    "IN_ALL_EVENTS"))
