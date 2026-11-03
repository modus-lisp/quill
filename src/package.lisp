;;;; package.lisp

(defpackage #:quill
  (:use #:cl)
  (:export
   ;; the one call most programs need
   #:read-line-edited
   ;; ... or driven by a program that is handed its keys as events
   #:start-line #:feed-line #:line-width
   ;; history
   #:history #:make-history #:history-add #:history-entries #:load-history
   ;; extending
   #:*default-keymap* #:make-keymap #:bind
   #:session #:session-buffer #:session-history #:insert #:redisplay
   ;; the editing model, for callers that want to drive it themselves
   #:editor #:make-editor #:editor-text #:editor-point
   ;; terminal
   #:tty-p #:terminal-columns))
