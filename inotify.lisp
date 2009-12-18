;; Copyright (C) 2009 Olof-Joachim Frahm

;; This program is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the
;; Free Software Foundation; either version 3 of the License, or (at your
;; option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General
;; Public License for more details.

;; You should have received a copy of the GNU General Public License along
;; with this program; if not, see <http://www.gnu.org/licenses/>.

(in-package #:cl-notify)

(defbitfield (inotify-flag :uint32)
  (:access #.in-access)
  (:modify #.in-modify)
  (:attrib #.in-attrib)
  (:close-write #.in-close-write)
  (:close-nowrite #.in-close-nowrite)
  (:close #.in-close)
  (:open #.in-open)
  (:moved-from #.in-moved-from)
  (:moved-to #.in-moved-to)
  (:move #.in-move)
  (:create #.in-create)
  (:delete #.in-delete)
  (:delete-self #.in-delete-self)
  (:move-self #.in-move-self)
  (:unmount #.in-unmount)
  (:q-overflow #.in-q-overflow)
  (:ignored #.in-ignored)
  (:onlydir #.in-onlydir)
  (:dont-follow #.in-dont-follow)
  (:mask-add #.in-mask-add)
  (:isdir #.in-isdir)
  (:oneshot #.in-oneshot)
  (:all-events #.in-all-events))

(deftype inotify-add/read-flag ()
  "Shared valid flags for the WATCH and READ-EVENT functions."
  '(member
    :access :attrib 
    :close-write :close-nowrite :close
    :create :delete :delete-self
    :modify
    :move-self :moved-from :moved-to :move
    :open :all-events))

(deftype inotify-add-flag ()
  "Valid flags for the WATCH function."
  '(or inotify-add/read-flag
    (member :dont-follow :mask-add :oneshot :onlydir)))

(deftype inotify-read-flag ()
  "Valid flags which are returned from READ-EVENT."
  '(or inotify-add/read-flag
    (member :ignored :isdir :q-overflow :unmount)))

(defcfun ("inotify_init" c-inotify-init) :int
  "Initialises a new inotify event queue.")

(defcfun ("inotify_add_watch" c-inotify-add-watch) :int
  "Watches a path on a event queue."
  (fd :int)
  (pathname :string)
  (mask inotify-flag))

(defcfun ("inotify_rm_watch" c-inotify-rm-watch) :int
  "Removes a watched path from a event queue."
  (fd :int)
  (wd :int))

(binary-types:define-signed int #.(cffi:foreign-type-size :int))

(binary-types:define-binary-struct inotify-event ()
  "An inotify native event structure.
WD is the watch/file descriptor,
MASK is the (parsed) combination of events,
COOKIE is a unique integer which connects related events,
NAME optionally identifies a file relative to a watched directory."
  (wd 0 :binary-type int)
  (mask 0 :binary-type binary-types:u32)
  (cookie 0 :binary-type binary-types:u32)
  (name NIL))

(defstruct (inotify-instance
	     (:constructor make-inotify-instance ())
	     (:conc-name inotify-))
  "Contains the stream and file descriptor for a inotify instance."
  fd
  stream
  nonblocking)

;;;; initialisation and stuff

(defun init-endian ()
  "Initialises endianess for the BINARY-TYPES library."
  (setf binary-types:*endian*
	#+little-endian :little-endian
	#+big-endian :big-endian
	#-(or little-endian big-endian) (error "unknown endianess")))

;; um, in what evel-when should this be wrapped?
(init-endian)

;;;; basic wrapping of the API

(defun read-raw-event-from-stream (stream)
  "Reads a raw event from the inotify stream."
  (let* ((event (binary-types:read-binary 'inotify-event stream))
	 (len (binary-types:read-binary 'binary-types:u32 stream)))
    (when (plusp len)
      (with-slots (name) event
	(setf name (binary-types:read-binary-string stream
						    :size len
						    :terminators '(0)))))
    event))

(defun read-event-from-stream (stream)
  "Reads a event from the inotify stream and converts bitmasks on reading."
  (let ((event (read-raw-event-from-stream stream)))
    (with-slots (mask) event
      (setf mask (foreign-bitfield-symbols 'inotify-flag mask)))
    event))

(defun set-nonblocking (fd nonblocking)
  (let ((flags (sb-posix:fcntl fd sb-posix:f-getfl)))
    ;; an error is raised if this fails, so we don't have to do it ourselves
    (sb-posix:fcntl fd sb-posix:f-setfl
		    (funcall (if nonblocking #'logior #'logxor)
			     flags sb-posix:o-nonblock))))

(defun init-unregistered-notify (notify &optional (nonblocking T))
  "Creates a new inotify event queue.  If NONBLOCKING is set (default),
the file descriptor is set to non-blocking I/O."
  (let ((result (c-inotify-init)))
    (when (minusp result)
      (error "inotify_init failed: ~A" result))
    (with-slots (fd stream (non-block nonblocking)) notify
      (unwind-protect
	   ;; file descriptor is collected with auto-close
	   (progn
	     (setf fd result)
	     (when nonblocking
	       (set-nonblocking fd T)
	       (setf non-block nonblocking))
	     (setf stream
		   (sb-sys:make-fd-stream
		    fd
		    :input T
		    :element-type '(unsigned-byte 8)
		    :name (format NIL "inotify event queue ~A" fd)
		    :auto-close T
		    :buffering (if nonblocking :none :full))))
	;; if stream is constructed, gc'ing it will cleanup the file descriptor
	(unless stream
	  (sb-posix:close fd)))))
  notify)

(defun make-unregistered-notify ()
  (init-unregistered-notify (make-inotify-instance)))

(defun close-notify (notify)
  "Closes the inotify event queue."
  (close (inotify-stream notify))
  (values))

(defun watch-raw (notify pathname flags)
  "Adds PATHNAME (either pathname or string) to be watched.  FLAGS
determines how exactly (see inotify(7) for detailed information).
Returns a handle which can be used with UNWATCH."
  (let ((path (princ-to-string pathname))
	result)
    (setf result (c-inotify-add-watch
		  (inotify-fd notify)
		  path
		  (typecase flags
		    (list (foreign-bitfield-value 'inotify-flag flags))
		    (keyword (foreign-bitfield-value 'inotify-flag
						     (list flags)))
		    (T flags))))
    (when (minusp result)
      (error "inotify_add_watch failed: ~A" result))
    result))

(defun unwatch-raw (notify handle)
  "Disables watching the path associated with HANDLE."
  (let ((result (c-inotify-rm-watch (inotify-fd notify) handle)))
    (when (minusp result)
      (error "inotify_rm_watch failed: ~A" result)))
  (values))

;;;; support functions, making life easier

(defstruct (registered-inotify-instance
	     (:include inotify-instance)
	     (:constructor make-registered-inotify-instance ())
	     (:conc-name inotify-))
  "Additionally to the information in INOTIFY-INSTANCE, records watched
paths in a dictionary."
  watched)

(defun make-notify (&optional (nonblocking T))
  (let ((result (make-registered-inotify-instance)))
    (init-unregistered-notify result nonblocking)
    (with-slots (watched) result
      (setf watched (make-hash-table :test 'equal)))
    result))

(defun watchedp (notify pathname)
  "Returns the tuple (HANDLE . FLAGS) if PATHNAME is being watched by NOTIFY,
else NIL."
  (awhen (gethash pathname (inotify-watched notify))
    (values (car it) (cdr it))))

;; TODO: handle additional flags, save only list of flags
(defun watch (notify pathname flags)
  (let ((handle (watch-raw notify pathname flags)))
    (with-slots (watched) notify
      (setf (gethash pathname watched) (cons handle flags)))
    handle))

(defun unwatch (notify &key pathname handle)
  (unless (or pathname handle)
    (error "either PATHNAME or HANDLE has to be specified"))
  (if handle
      (unwatch-raw notify handle)
      (let ((handle (watchedp notify pathname)))
	(unless handle
	  (error "PATHNAME ~S isn't being watched" pathname))
	(unwatch-raw notify handle)
	(remhash pathname (inotify-watched notify))))
  (values))

(defun list-watched (notify)
  "Returns a list of all watched pathnames in particular order."
  (let (result)
    (maphash (lambda (k v)
	       (declare (ignore v))
	       (push k result))
	     (inotify-watched notify))
    result))

(defun unix-eagainp (fd-stream)
  "Returns T on a FD-STREAM, if trying to read raised a EAGAIN error."
  (multiple-value-bind (result error)
      (sb-unix:unix-read (sb-sys:fd-stream-fd fd-stream) NIL 0)
    (declare (ignore result))
    (= error sb-unix:eagain)))

(defun event-availablep (notify)
  "Returns T if an event is available on the queue."
  (if (inotify-nonblocking notify)
      (not (unix-eagainp (inotify-stream notify)))
      (listen (inotify-stream notify))))

(defun read-event (notify)
  "Reads an event from the queue.  Blocks if no event is available."
  (read-event-from-stream (inotify-stream notify)))

(defun next-event (notify)
  "Reads an event from the queue.  Returns NIL if none is available."
  (when (event-availablep notify)
    (read-event notify)))

(defmacro! do-events ((var o!notify) &body body)
  "Loops BODY with VAR bound to the events retrieved from NOTIFY.  The macro
uses NEXT-EVENT, so that reading an event won't block."
  `(loop as ,var = (next-event ,g!notify)
      while ,var
      do (progn ,.body)))

(defun next-events (notify)
  "Reads all available events from the queue.  Returns a list of events."
  (let (result)
   (do-events (event notify)
     (push event result))
   (nreverse result)))
