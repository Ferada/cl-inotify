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

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun read-new-value (&optional (stream *query-io*))
    "READs a value from the STREAM and returns it (wrapped in a list)."
    (format stream "Enter a new value: ~%")
    (list (read *query-io*))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun init-endian ()
    "Initialises the endianess for the BINARY-TYPES library.  Is automatically
called when the library is loaded."
    (setf binary-types:*endian*
	  (restart-case #+little-endian :little-endian
			#+big-endian :big-endian
			#-(or little-endian big-endian) (error "unknown endianess")
			(use-value (value)
			  :report "Enter a correct value (either :LITTLE-ENDIAN or :BIG-ENDIAN)."
			  :interactive read-new-value
			  ;; TODO: better way to test for correct value/retry values?
			  (case value
			    ((:little-endian :big-endian) value)
			    (T (error "wrong value supplied (not :LITTLE-ENDIAN or :BIG-ENDIAN)"))))))))

;; initialise the endianess
(eval-when (:compile-toplevel :load-toplevel :execute)
  (init-endian))

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
  "Enables or disables NONBLOCKING mode on a file descriptor FD."
  (let ((flags (sb-posix:fcntl fd sb-posix:f-getfl)))
    ;; an error is raised if this fails, so we don't have to do it ourselves
    (sb-posix:fcntl fd sb-posix:f-setfl
		    (funcall (if nonblocking #'logior #'logxor)
			     flags sb-posix:o-nonblock)))
  (values))

(defun init-unregistered-notify (notify &optional (nonblocking T))
  "Creates a new inotify event queue.  If NONBLOCKING is set (default),
the file descriptor is set to non-blocking I/O."
  (let ((result (c-inotify-init)))
    (when (minusp result)
      (perror "inotify_init failed"))
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
  "Creates a new unregistered NOTIFY instance."
  (init-unregistered-notify (make-inotify-instance)))

(defun close-notify (notify)
  "Closes the inotify event queue."
  (close (inotify-stream notify))
  (values))

(defun perror (prefix-string)
  #+sbcl (sb-int:simple-perror prefix-string)
  #-(or sbcl) (error prefix-string))

(defun ensure-list (arg)
  (if (listp arg) arg `(,arg)))

(defun translate-keyword-flags (flags)
  (typecase flags
    ((or keyword list)
     (foreign-bitfield-value 'inotify-flag (ensure-list flags)))
    (T flags)))

(defun watch-raw (notify pathname flags)
  "Adds PATHNAME (either of type PATHNAME or STRING) to be watched.  FLAGS
determines how exactly (see inotify(7) for detailed information) and can
be of type LIST, KEYWORD or raw a raw numerical value (which isn't checked
for validity).  Returns a handle which can be used with UNWATCH-RAW."
  (let ((path (etypecase pathname
		(string pathname)
		(pathname (namestring pathname))))
	(result (c-inotify-add-watch (inotify-fd notify)
				     path (translate-keyword-flags flags))))
    (when (minusp result)
      (perror "inotify_add_watch failed"))
    result))

(defun unwatch-raw (notify handle)
  "Stops watching the path associated with a HANDLE established by WATCH-RAW."
  (let ((result (c-inotify-rm-watch (inotify-fd notify) handle)))
    (when (minusp result)
      (perror "inotify_rm_watch failed")))
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
  "Creates a new registered NOTIFY instance.  In NONBLOCKING mode, the file
descriptor is set to non-blocking mode."
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
  "Adds PATHNAME (either pathname or string) to be watched and records the
watched paths.  FLAGS determines how exactly (see inotify(7) for detailed
information).  Returns a handle which can be used with UNWATCH."
  (let ((handle (watch-raw notify pathname flags)))
    (with-slots (watched) notify
      (setf (gethash pathname watched) (cons handle flags)))
    handle))

(defun unwatch (notify &key pathname handle)
  "Disables watching the path associated with the supplied HANDLE or PATHNAME."
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
  (loop
     for pathname being each hash-key in (inotify-watched notify)
     collect pathname))

(defun unix-eagain-p (fd-stream)
  "Returns T on a FD-STREAM if trying to read from the stream raised a
EAGAIN error."
  (multiple-value-bind (result error)
      (sb-unix:unix-read (sb-sys:fd-stream-fd fd-stream) NIL 0)
    (declare (ignore result))
    (= error sb-unix:eagain)))

(defun event-available-p (notify)
  "Returns T if an event is available on the queue."
  (if (inotify-nonblocking notify)
      (not (unix-eagain-p (inotify-stream notify)))
      (listen (inotify-stream notify))))

(defun read-event (notify)
  "Reads an event from the queue.  Blocks if no event is available."
  (read-event-from-stream (inotify-stream notify)))

(defun next-event (notify)
  "Reads an event from the queue.  Returns NIL if none is available."
  (when (event-available-p notify)
    (read-event notify)))

(defmacro! do-events ((var o!notify) &body body)
  "Loops BODY with VAR bound to the next events retrieved from NOTIFY.
The macro uses NEXT-EVENT, so reading an event won't block and the returns
terminates if no events are available."
  `(loop
      with ,var
      while (event-available-p ,g!notify)
      do (progn
	   (setf ,var (read-event ,g!notify))
	   ,.body)))

(defun next-events (notify)
  "Reads all available events from the queue.  Returns a LIST of events."
  (loop
     while (event-available-p notify)
     collect (read-event notify)))
