;;;; quill.asd — a line editor in pure Common Lisp.
(asdf:defsystem "quill"
  :description "A terminal line editor in pure Common Lisp: Emacs keys, multi-line input,
persistent history with reverse search, bracketed paste, completion and paren matching.
No FFI and no dependencies.  The one per-implementation file is src/port.lisp (raw mode
and timed reads); the terminal's size is asked OF the terminal, so no ioctl is needed."
  :author "ynniv"
  :license "MIT"
  :version "0.1.0"
  :depends-on ((:feature :sbcl "sb-posix"))
  :serial t
  :components ((:module "src"
                :serial t
                :components ((:file "package")
                             (:file "port")
                             (:file "keys")
                             (:file "buffer")
                             (:file "history")
                             (:file "render")
                             (:file "editor"))))
  :in-order-to ((asdf:test-op (asdf:test-op "quill/test"))))

(asdf:defsystem "quill/test"
  :depends-on ("quill")
  :components ((:module "t" :components ((:file "tests"))))
  :perform (asdf:test-op (o c) (uiop:symbol-call :quill.test :run-tests)))
