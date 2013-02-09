CL-INOTIFY - Interface to the Linux inotify(7) API.

Copyright (C) 2011-12 Olof-Joachim Frahm

Released under a Simplified BSD license.

Working, but unfinished.

Implementations currently running on: SBCL.

Uses CFFI, binary-types (from [my Github][1] or see [CLiki][2]) and
trivial-utf-8.  Doesn't use iolib, because I don't need most of the
functionality, although it might gain us some implementation
independence (patches which can be conditionally compiled are most
welcome; in any case patches are welcome).

A similar package is at [stassats Github][3].

This document helps only with the aspects of this binding, so reading
the man-page and other information on the inotify-facility may be
needed.  Reading the next sections and the docstrings of exported
symbols should get you going, otherwise the source itself may also be of
some value.


# REPLSHOT

Macros make keeping track easier, so the following example is
straightforward:

    > (with-inotify (inotify T ("." :all-events))
    >   (do-events (event inotify :blocking-p T)
    >     (format T "~A~%" event)))
    > =>
    > #S(CL-INOTIFY::INOTIFY-EVENT :WD 1 :MASK (CREATE) :COOKIE 0 :NAME .zshist.LOCK)
    > #S(CL-INOTIFY::INOTIFY-EVENT :WD 1 :MASK (OPEN) :COOKIE 0 :NAME .zshist)
    > #S(CL-INOTIFY::INOTIFY-EVENT :WD 1 :MASK (MODIFY) :COOKIE 0 :NAME .zshist)
    > #S(CL-INOTIFY::INOTIFY-EVENT :WD 1 :MASK (CLOSE-WRITE) :COOKIE 0 :NAME .zshist)
    > #S(CL-INOTIFY::INOTIFY-EVENT :WD 1 :MASK (DELETE) :COOKIE 0 :NAME .zshist.LOCK)
    > ...

(Tilde-expansion has to happen at another level, else I would've used
that.)

The first parameter is (per convention) the symbol to which the queue is
bound, the second is the parameter to `MAKE-INOTIFY`.  The `&REST` list
consists of parameter lists for the `WATCH`-function, which is called
for every list before the `&BODY` is executed.  We don't actually need
to `UNWATCH` every watched path as closing the queue will also take care
of that.


# LOWER-LEVEL USAGE EXAMPLE

You don't have to use macros: all functionality is available in function
form, although some care should be taken as currently no cleanup handler
is registered for opened queues, or rather their file handles.

    > (use-package '#:cl-inotify)
    > (defvar *tmp*)
    > (setf *tmp* (make-notify))
    > (watch *tmp* "/var/tmp/" :all-events)
    > (next-events *tmp*)
    > (close-inotify *tmp*)


# HOWTO

So this section deals in depth with the various bits which make the
examples above tick.


After loading the library use `MAKE-INOTIFY` to create a new event
queue.  The `NONBLOCKING` argument sets the `SB-POSIX:O-NONBLOCK` bit on
the stream so we don't block while reading.  Nevertheless,
`EVENT-AVAILABLE-P` works either way (by using `CL:LISTEN`, or a custom
function which works directly on the file descriptor).

The result of `MAKE-INOTIFY` is used with `WATCH` and `UNWATCH`, the first
being used to watch a file or directory, the second to stop watching
it.  The `FLAGS` parameter of `WATCH` is described in the notify(7)
man-page; you can use a combination of the flags (as keywords) to create
a suitable bitmask.  The types `INOTIFY-ADD/READ-FLAG`,
`INOTIFY-READ-FLAG` and `INOTIFY-ADD-FLAG` are also defined and can be
examined.

For example, to watch for modified or closed files in a directory, call
`(WATCH inotify "foo/" '(:modify :close))`.

The result of `WATCH` is a handle (currently a `FIXNUM`, but I wouldn't
rely on that) which can be fed to `UNWATCH` and can be translated from
events with `EVENT-PATHNAME/FLAGS`.

To finally get the events from the queue, use `READ-EVENT` (which
blocks) or `NEXT-EVENT` (which doesn't block).  `EVENT-AVAILABLEP` does
what it should do, `NEXT-EVENTS` retrieves all currently available
events as a list and `DO-EVENTS` (nonblocking) iterates over available
events.

The enhanced API registers all watched paths in a hashtable, so you can
use `PATHNAME-HANDLE/FLAGS` to check if a pathname (exact match) is
being watched and `LIST-WATCHED` to return all watched paths as a list.
`EVENT-PATHNAME/FLAGS` may be used to get the pathname and flags for a
read event.

`UNWATCH` has to be called with the path or the handle of the watched
file or directory (a path will be looked up in the same table as with
`PATHNAME-HANDLE/FLAGS`). 


The raw API, which doesn't register watched paths, consists of
`READ-RAW-EVENT-FROM-STREAM`, `READ-EVENT-FROM-STREAM`, `WATCH-RAW` and
`UNWATCH-RAW`.  They are just a thin wrapper around the C functions, but
they're exported in case someone doesn't like the upper layers.


# EVENT-BASED PROCESSING

In case you want to use `epoll` or `select` on the event queue you can
access the file descriptor yourself and then use the normal functions
afterwards.  Currently no such functionality is integrated here, however
the following sketch shows how something can be accomplished using
iolib:

    (with-unregistered-inotify (inotify T ("." :all-events))
      (flet ((inotify-input (&rest rest)
               (declare (ignore rest))
               (format T "~{~A~%~}" (next-events inotify))))
        (iolib:with-event-base (event-base)
          (iolib:set-io-handler event-base (inotify-fd inotify) :read #'inotify-input)
          (iolib:event-dispatch event-base))))

Note that we perform all inotify business only when something happens in
that directory, so instead of doing nothing, we could actually do useful
work, e.g. communicating with a process:  This snippet was extracted
from a function which uses behaviour to monitor a LaTeX process for
written files to get the output file name without relying on heuristics
about the generated filename.  As it stands you have to split this into
threads, or use `IOLIB:EVENT-DISPATCH` with a timeout while periodically
checking the process status.


# REFERENCE

Here follows a list of valid keywords for the `INOTIFY-FLAG` type:

* `:ACCESS`
* `:MODIFY`
* `:ATTRIB`
* `:CLOSE-WRITE`
* `:CLOSE-NOWRITE`
* `:CLOSE`
* `:OPEN`
* `:MOVED-FROM`
* `:MOVED-TO`
* `:MOVE`
* `:CREATE`
* `:DELETE`
* `:DELETE-SELF`
* `:MOVE-SELF`
* `:UNMOUNT`
* `:Q-OVERFLOW`
* `:IGNORED`
* `:ONLYDIR`
* `:DONT-FOLLOW`
* `:MASK-ADD`
* `:ISDIR`
* `:ONESHOT`
* `:ALL-EVENTS`


The `INOTIFY-EVENT` structure has the slots `WD`, `MASK`, `COOKIE` and
`NAME` (with default `CONC-NAME`: `INOTIFY-EVENT-`).

The `INOTIFY-INSTANCE` structure has the slots `FD`, `STREAM` and
`NONBLOCKING` with `CONC-NAME` `INOTIFY-`.

The `REGISTERED-INOTIFY-INSTANCE` includes the previous structure and
only adds the `WATCHED` slot under the same `CONC-NAME`.


# TODO

- more functionality to examine read events
- extend to other APIs?
- make things more implementation independent (partly done, still needs
  fd-streams everywhere, or skip them entirely)
- (maybe) don't use the libc for this, direct syscall
- (maybe) add iolib replacement for io functions
- the nonblocking mode is pretty useless, because for one the READ
  functions still block and also LISTEN seems to work just fine and it's
  not even needed for multiplexing, so why keep this in?

[1]: https://github.com/Ferada/binary-types
[2]: http://www.cliki.net/Binary-types
[3]: https://github.com/stassats/inotify
