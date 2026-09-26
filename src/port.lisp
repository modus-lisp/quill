;;;; port.lisp — everything quill needs from the implementation, and nothing else.
;;;;
;;;; A line editor needs three things a portable program cannot do by itself: take the
;;;; terminal out of line-buffered mode, read one byte with a timeout (to tell a lone ESC
;;;; from the start of an escape sequence), and know whether stdin is a terminal at all.
;;;; Those are the functions below, each with a branch per implementation.  Everything
;;;; else in quill is ordinary Common Lisp.
;;;;
;;;; Deliberately NOT here: the terminal's size.  That is asked of the terminal itself with
;;;; a cursor-position report (see TERMINAL-COLUMNS in editor.lisp), which needs no ioctl
;;;; and picks up a resize at the next prompt.

(in-package #:quill)

(defvar *saved-termios* nil "The terminal's settings before RAW-ON, restored by RAW-OFF.")

(defun tty-p (&optional (fd 0))
  "True if FD is a terminal."
  #+sbcl (= 1 (sb-unix:unix-isatty fd))
  #-sbcl (progn fd nil))

(defun raw-on ()
  "Put the terminal into the mode an editor needs: no line buffering, no echo, no signals
   from keys (Ctrl-C and Ctrl-Z arrive as bytes and quill decides what they mean), and CR
   kept distinct from LF so Enter and Ctrl-J can do different things.  Output processing
   is left ON, so a newline written still returns the carriage."
  #+sbcl
  (let ((tio (sb-posix:tcgetattr 0)))
    (setf *saved-termios* (sb-posix:tcgetattr 0))
    (setf (sb-posix:termios-lflag tio)
          (logandc2 (sb-posix:termios-lflag tio)
                    (logior sb-posix:icanon sb-posix:echo sb-posix:isig sb-posix:iexten)))
    (setf (sb-posix:termios-iflag tio)
          (logandc2 (sb-posix:termios-iflag tio) (logior sb-posix:icrnl sb-posix:ixon)))
    (let ((cc (sb-posix:termios-cc tio)))
      (setf (aref cc sb-posix:vmin) 1 (aref cc sb-posix:vtime) 0))
    (sb-posix:tcsetattr 0 sb-posix:tcsanow tio)
    t)
  #-sbcl (error "quill: raw mode is not implemented for this Lisp yet (see port.lisp)"))

(defun raw-off ()
  "Restore the terminal to how RAW-ON found it."
  (when *saved-termios*
    #+sbcl (ignore-errors (sb-posix:tcsetattr 0 sb-posix:tcsanow *saved-termios*))
    (setf *saved-termios* nil)))

(defun read-byte-timeout (&optional timeout)
  "The next byte from stdin, or NIL if TIMEOUT seconds pass first (or at end of input).
   TIMEOUT NIL blocks.  Reads the file descriptor directly rather than *STANDARD-INPUT*:
   that stream buffers, and a buffered byte is invisible to a readiness check, which would
   make a lone ESC look like one still waiting for the rest of its sequence."
  #+sbcl
  (when (or (null timeout) (sb-sys:wait-until-fd-usable 0 :input timeout))
    (let ((buf (make-array 1 :element-type '(unsigned-byte 8))))
      (sb-sys:with-pinned-objects (buf)
        (let ((n (sb-unix:unix-read 0 (sb-sys:vector-sap buf) 1)))
          (and n (= n 1) (aref buf 0))))))
  #-sbcl (progn timeout (error "quill: raw reads are not implemented for this Lisp yet")))

(defun getenv (name)
  "The value of environment variable NAME, or NIL."
  #+sbcl (sb-ext:posix-getenv name)
  #-sbcl (progn name nil))

(defun suspend-self ()
  "Stop this process as Ctrl-Z would (SIGTSTP), returning when it is continued."
  #+sbcl (sb-posix:kill 0 sb-posix:sigtstp)
  #-sbcl nil)
