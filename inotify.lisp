(in-package #:cl-notify)

(defbitfield (inotify-flags :uint32)
  (:in-access #.in-access)
  (:in-modify #.in-modify)
  (:in-attrib #.in-attrib)
  (:in-close-write #.in-close-write)
  (:in-close-nowrite #.in-close-nowrite)
  (:in-close #.in-close)
  (:in-open #.in-open)
  (:in-moved-from #.in-moved-from)
  (:in-moved-to #.in-moved-to)
  (:in-move #.in-move)
  (:in-create #.in-create)
  (:in-delete #.in-delete)
  (:in-delete-self #.in-delete-self)
  (:in-move-self #.in-move-self)
  (:in-unmount #.in-unmount)
  (:in-q-overflow #.in-q-overflow)
  (:in-ignored #.in-ignored)
  (:in-onlydir #.in-onlydir)
  (:in-dont-follow #.in-dont-follow)
  (:in-mask-add #.in-mask-add)
  (:in-isdir #.in-isdir)
  (:in-oneshot #.in-oneshot)
  (:in-all-events #.in-all-events))

(defcfun "inotify_init" :int)

(defcfun "inotify_add_watch" :int
  (fd :int)
  (pathname :string)
  (mask inotify-flags))

(defcfun "inotify_rm_watch" :int
  (fd :int)
  (wd :int))

(binary-types:define-signed int #.(cffi:foreign-type-size :int))

(binary-types:define-binary-struct inotify-event ()
  (wd 0 :binary-type int)
  (mask 0 :binary-type binary-types:u32)
  (cookie 0 :binary-type binary-types:u32)
  (name NIL))

(defstruct (inotify-instance (:constructor make-inotify-instance (fd stream)))
  fd
  stream)

(defun init-endian ()
  (setf binary-types:*endian*
	#+little-endian :little-endian
	#+big-endian :big-endian
	#-(or little-endian big-endian) (error "unknown endianess")))

(init-endian)

(defun inotify-read-raw-event (stream)
  (let* ((event (binary-types:read-binary 'inotify-event stream))
	 (len (binary-types:read-binary 'binary-types:u32 stream)))
    (when (> len 0)
      (with-slots (name) event
	(setf name
	      (binary-types:read-binary-string stream :size len :terminators '(0)))))
    event))

(defun inotify-read-event (stream)
  (let ((event (inotify-read-raw-event stream)))
    (with-slots (mask) event
      (setf mask (foreign-bitfield-symbols 'inotify-flags mask)))
    event))

(defun make-notify ()
  (let* ((fd (inotify-init)))
    (when (< fd 0)
      (error "inotify_init failed: ~A" fd))
    ;; file descriptor is collected with auto-close
    (make-inotify-instance
     fd
     (sb-sys:make-fd-stream fd
			    :input T
			    :element-type '(unsigned-byte 8)
			    :name (format NIL "inotify event queue ~A" fd)
			    :auto-close T))))

(defun close-notify (notify)
  (close (inotify-instance-stream notify))
  (values))

(defun watch (notify pathname flags)
  (let ((path (princ-to-string pathname))
	result)
    (setf result
	  (inotify-add-watch (inotify-instance-fd notify)
			     path
			     (if (listp flags)
				 (foreign-bitfield-value 'inotify-flags flags)
				 flags)))
    (when (< result 0)
      (error "inotify_add_watch failed: ~A" result))
    result))

(defun unwatch (notify handle)
  (let ((result (inotify-rm-watch (inotify-instance-fd notify) handle)))
    (when (< result 0)
      (error "inotify_rm_watch failed: ~A" result))
    (values)))
