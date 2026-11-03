;;;; tests.lisp — quill's pure layers, and whole sessions driven from byte strings.
;;;;
;;;; Everything here runs without a terminal.  RUN-SESSION builds a real session with a byte
;;;; source in place of the keyboard and a string stream in place of the screen, then runs
;;;; the same decode/dispatch loop READ-LINE-EDITED does -- so keys, commands, history and
;;;; rendering are all exercised; only raw mode is not.

(defpackage #:quill.test (:use #:cl #:quill) (:export #:run-tests))
(in-package #:quill.test)

(defvar *pass* 0) (defvar *fail* 0)

(defmacro is (name form expected)
  `(let ((got (handler-case ,form (error (e) (list :error (princ-to-string e))))))
     (if (equalp got ,expected)
         (incf *pass*)
         (progn (incf *fail*) (format t "~&FAIL ~a~%  expected ~s~%  got      ~s~%" ,name ,expected got)))))

(defun utf8 (string)
  (loop for c across string for code = (char-code c)
        append (cond ((< code #x80) (list code))
                     ((< code #x800) (list (logior #xC0 (ash code -6)) (logior #x80 (logand code #x3F))))
                     ((< code #x10000) (list (logior #xE0 (ash code -12)) (logior #x80 (logand (ash code -6) #x3F))
                                             (logior #x80 (logand code #x3F))))
                     (t (list (logior #xF0 (ash code -18)) (logior #x80 (logand (ash code -12) #x3F))
                              (logior #x80 (logand (ash code -6) #x3F)) (logior #x80 (logand code #x3F)))))))

(defun bytes (&rest parts)
  "Parts are strings (UTF-8 encoded), integers (raw bytes) or :esc."
  (coerce (loop for p in parts
                append (cond ((stringp p) (utf8 p))
                             ((eq p :esc) (list 27))
                             (t (list p))))
          '(vector (unsigned-byte 8))))

(defun keys (&rest parts)
  (let ((src (quill::byte-source (apply #'bytes parts))))
    (loop for k = (quill::decode-key src) while k collect k)))

(defun run-session (octets &key history (width 40) completer (prompt "> ") initial)
  "Run a session over OCTETS.  Returns (values text status editor output)."
  (let* ((out (make-string-output-stream))
         (s (quill::%make-session :buffer (make-editor :text (or initial "")) :history history
                                  :prompt prompt :keymap *default-keymap* :completer completer
                                  :output out :next (quill::byte-source octets) :width width)))
    (loop until (quill::session-done s)
          do (let ((k (quill::%next-key s)))
               (if k (quill::dispatch s k)
                   (setf (quill::session-done s) t (quill::session-status s) :eof)))
             (unless (quill::session-done s) (quill::redisplay s)))
    (let ((text (editor-text (quill::session-buffer s))))
      (when (and history (eq (quill::session-status s) :ok)) (history-add history text))
      (values text (quill::session-status s) (quill::session-buffer s)
              (get-output-stream-string out)))))

(defun text-of (&rest parts) (values (run-session (apply #'bytes parts))))

(defun run-tests ()
  (setf *pass* 0 *fail* 0)

  ;; ---- keys ----
  (is "printable" (keys "ab") '(#\a #\b))
  (is "utf-8 input" (keys "é北🙂") (list #\é #\北 (code-char #x1F642)))
  (is "enter vs ctrl-j" (keys 13 10) '(:enter :newline))
  (is "control letters" (keys 1 5 11) '(:c-a :c-e :c-k))
  (is "backspace both ways" (keys 127 8) '(:backspace :backspace))
  (is "arrows (CSI)" (keys :esc "[A" :esc "[B" :esc "[C" :esc "[D") '(:up :down :right :left))
  (is "arrows (SS3)" (keys :esc "OA" :esc "OD") '(:up :left))
  (is "home/end/delete" (keys :esc "[H" :esc "[F" :esc "[3~" :esc "[1~" :esc "[4~")
      '(:home :end :delete :home :end))
  (is "ctrl-arrows" (keys :esc "[1;5C" :esc "[1;5D") '(:ctrl-right :ctrl-left))
  (is "meta keys" (keys :esc "b" :esc "f" :esc "<" :esc 127) '(:m-b :m-f :m-< :m-backspace))
  (is "lone escape" (keys :esc) '(:escape))
  (is "cursor report" (keys :esc "[24;80R") '((:cursor-pos 24 80)))
  (is "bracketed paste" (keys :esc "[200~" "a" 13 "b 北" :esc "[201~" "x")
      (list (list :paste (format nil "a~cb 北" #\Return)) #\x))
  (is "unknown sequence is ignored, not fatal" (keys :esc "[99z" "a") '(:unknown #\a))

  ;; ---- editing ----
  (is "type" (text-of "hello" 13) "hello")
  (is "backspace" (text-of "helo" 127 "lo" 13) "hello")    ; helo, delete o, type lo -> hello
  (is "move and insert" (text-of "hllo" 1 6 "e" 13) "hello") ; C-a, C-f, e
  (is "delete forward" (text-of "hexllo" 2 2 2 2 :esc "[3~" 13) "hello")
  (is "kill to end, yank" (text-of "hello world" 1 6 6 6 6 6 11 25 25 13) "hello world world")
  (is "consecutive kills accumulate" (text-of "a b c" 23 23 25 13) "a b c")
  (is "kill word back" (text-of "one two" 23 13) "one ")
  (is "word motion" (text-of "one two" :esc "b" "X" 13) "one Xtwo")
  (is "transpose" (text-of "ab" 20 13) "ba")
  (is "C-u kills to line start" (text-of "abc" 2 21 13) "c")

  ;; ---- multi-line ----
  (is "C-j inserts a newline, Enter submits" (text-of "ab" 10 "cd" 13) (format nil "ab~%cd"))
  (is "up/down move within lines" (text-of "abcd" 10 "xy" :esc "[A" "Q" 13)
      (format nil "abQcd~%xy"))
  (is "goal column kept across a short line" (text-of "abcdef" 10 "x" 10 "ghijkl" 2 2 2
                                                      :esc "[A" :esc "[A" "Q" 13)
      (format nil "abcQdef~%x~%ghijkl"))
  (is "C-a/C-e are per logical line" (text-of "ab" 10 "cd" 1 "X" 5 "Y" 13)
      (format nil "ab~%XcdY"))
  (is "paste keeps newlines, never submits" (text-of :esc "[200~" "one" 13 10 "two" :esc "[201~" 13)
      (format nil "one~%two"))

  ;; ---- history ----
  (let ((h (make-history)))
    (run-session (bytes "first" 13) :history h)
    (run-session (bytes "second" 13) :history h)
    (is "history recorded" (coerce (history-entries h) 'list) '("first" "second"))
    (is "up recalls newest" (values (run-session (bytes :esc "[A" 13) :history h)) "second")
    (is "up twice" (values (run-session (bytes :esc "[A" :esc "[A" 13) :history h)) "first")
    (is "down returns the draft" (values (run-session (bytes "draft" :esc "[A" :esc "[B" 13) :history h))
        "draft")
    (is "duplicates not repeated" (length (history-entries h)) 4)  ; + "second" (recalled) + "draft"
    (is "blank lines not recorded" (progn (run-session (bytes "   " 13) :history h)
                                          (length (history-entries h)))
        4)
    (is "reverse search" (values (run-session (bytes 18 "fir" 13) :history h)) "first")
    (is "reverse search then edit" (values (run-session (bytes 18 "fir" 5 "!" 13) :history h)) "first!")
    (is "reverse search cancelled" (values (run-session (bytes "keep" 18 "fir" 7 13) :history h)) "keep"))

  (let* ((file (format nil "/tmp/quill-hist-~d.txt" (random 1000000)))
         (h (make-history :file file)))
    (run-session (bytes "a\\b" 13) :history h)
    (run-session (bytes "multi" 10 "line" 13) :history h)
    (is "history file round-trips escapes and newlines"
        (coerce (history-entries (make-history :file file)) 'list)
        (list "a\\b" (format nil "multi~%line")))
    (ignore-errors (delete-file file)))

  ;; ---- status ----
  (is "C-d on empty line is EOF" (nth-value 1 (run-session (bytes 4))) :eof)
  (is "C-d with text deletes" (text-of "ab" 1 4 13) "b")
  (is "C-c interrupts" (nth-value 1 (run-session (bytes "abc" 3))) :interrupt)

  ;; ---- completion ----
  (flet ((comp (text point)
           (let* ((start (or (position #\Space text :end point :from-end t) -1))
                  (word (subseq text (1+ start) point)))
             (values (remove-if-not (lambda (c) (eql 0 (search word c)))
                                    '("apple" "apricot" "banana"))
                     (1+ start)))))
    (is "unique completion" (values (run-session (bytes "eat b" 9 13) :completer #'comp)) "eat banana")
    (is "common prefix" (values (run-session (bytes "a" 9 13) :completer #'comp)) "ap")
    (is "second tab lists, text unchanged"
        (multiple-value-bind (text st ed out) (run-session (bytes "ap" 9 9 13) :completer #'comp)
          (declare (ignore st ed))
          (list text (and (search "apricot" out) t)))
        '("ap" t)))

  ;; ---- rendering ----
  (flet ((rows (prompt text point width)
           (multiple-value-list (quill::layout prompt text point width))))
    (is "simple" (rows "> " "abc" 3 20) '(("> abc") 0 5))
    (is "wraps at width" (rows "> " "abcdefgh" 8 6) '(("> abcd" "efgh") 1 4))
    (is "exactly full row: cursor on a new row" (rows "> " "abcd" 4 6) '(("> abcd" "") 1 0))
    (is "exactly full row then newline: no blank row"
        (rows "> " (format nil "abcd~%x") 6 6) '(("> abcd" "  x") 1 3))
    (is "continuation aligns with prompt" (rows ">>> " (format nil "a~%b") 3 20) '((">>> a" "    b") 1 5))
    (is "wide characters take two columns" (rows "" "北京" 2 20) '(("北京") 0 4))
    (is "wide char never splits across rows" (rows "" "ab北" 3 3) '(("ab" "北") 1 2))
    (is "control char shown as ^X" (rows "" (string (code-char 1)) 1 20) '(("^A") 0 2))
    (is "prompt colour doesn't count" (quill::visible-width (format nil "~c[1;32m> ~c[0m" #\Escape #\Escape)) 2))

  (is "paren match" (quill::matching-open-paren "(a (b) c)" 9) 0)
  (is "inner paren match" (quill::matching-open-paren "(a (b) c)" 6) 3)
  (is "no paren" (quill::matching-open-paren "abc" 3) nil)

  ;; ---- READ-LINE-EDITED itself, on an :INPUT source (an SSH channel, a window) ----
  (flet ((edited (octets &rest args)
           (let ((out (make-string-output-stream)))
             (multiple-value-bind (text status)
                 (apply #'read-line-edited :input (quill::byte-source octets) :output out args)
               (list text status (get-output-stream-string out))))))
    (let ((r (edited (bytes "(+ 1" 10 " 2)" 127 "3)" 13) :prompt "> " :width 40)))
      (is "input: edited and accepted" (subseq r 0 2) (list (format nil "(+ 1~% 23)") :ok))
      (is "input: prompt drawn, paste mode on and off"
          (list (and (search "> " (third r)) t)
                (and (search (format nil "~c[?2004h" #\Escape) (third r)) t)
                (and (search (format nil "~c[?2004l" #\Escape) (third r)) t))
          '(t t t)))
    (is "input: width asked of the terminal, keys typed before the answer kept"
        (subseq (edited (bytes "ab" :esc "[1;33R" "c" 13)) 0 2) '("abc" :ok))
    (is "input: C-d on an empty line" (subseq (edited (bytes 4) :width 40) 0 2) '("" :eof))
    (is "input: C-c" (subseq (edited (bytes "x" 3) :width 40) 0 2) '("" :interrupt))
    (is "input: C-z rings rather than stopping this process"
        (subseq (edited (bytes "a" 26 "b" 13) :width 40) 0 2) '("ab" :ok))
    (is "input: the source ending is end of input" (subseq (edited (bytes "ab") :width 40) 0 2)
        '("" :eof)))

  (format t "~&quill: ~d passed, ~d failed~%" *pass* *fail*)
  (zerop *fail*))
