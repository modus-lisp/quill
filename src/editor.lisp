;;;; editor.lisp — keys to commands, and the loop that reads a line.
;;;;
;;;; A KEYMAP maps key events (see keys.lisp) to commands.  A command is either a symbol
;;;; naming one of the buffer operations in buffer.lisp (called with the EDITOR), a symbol
;;;; naming a session command defined here (called with the SESSION), or any function of the
;;;; session -- which is how a program adds its own keys.

(in-package #:quill)

(defstruct (session (:constructor %make-session))
  (buffer (make-editor))
  history
  (prompt "") continuation
  completer
  keymap
  (match-parens t)
  (output *standard-output*)
  (next #'read-byte-timeout)      ; the byte source
  (local t)                       ; on this process's own terminal (raw mode, Ctrl-Z apply)
  (pending '())                   ; key events read ahead while asking the terminal its size
  (width 80)
  (cursor-row 0)                  ; where RENDER left the cursor, from the drawing's first row
  (done nil) (status :ok))

(defparameter *buffer-commands*
  '(delete-backward delete-forward forward-char backward-char beginning-of-line end-of-line
    beginning-of-buffer end-of-buffer forward-word backward-word kill-line kill-to-line-start
    kill-word-backward kill-word-forward yank transpose-chars)
  "Commands that take the EDITOR rather than the session.")

(defparameter *vertical-commands* '(up-or-history down-or-history)
  "Commands after which the goal column is kept rather than forgotten.")

;;; ---- output ---------------------------------------------------------------------------

(defun %out (s string)
  (write-string string (session-output s))
  (force-output (session-output s)))

(defun redisplay (s &key (point (editor-point (session-buffer s))) prompt text highlight)
  "Redraw the prompt and text.  PROMPT, TEXT and POINT may be overridden, for modes like
   reverse search that show something other than the buffer."
  (let* ((ed (session-buffer s))
         (tx (or text (editor-text ed)))
         (hl (or highlight
                 (and (session-match-parens s) (null text) (matching-open-paren tx point)))))
    (multiple-value-bind (bytes crow)
        (render (or prompt (session-prompt s)) tx point (session-width s)
                :continuation (session-continuation s) :highlight hl
                :previous-cursor-row (session-cursor-row s))
      (%out s bytes)
      (setf (session-cursor-row s) crow))))

;;; ---- reading keys ---------------------------------------------------------------------

(defun %next-key (s)
  (if (session-pending s) (pop (session-pending s)) (decode-key (session-next s))))

(defun terminal-columns (s &key (default 80))
  "The terminal's width, asked of the terminal: park the cursor at the far right and ask
   where it is (CSI 6 n).  No ioctl, so nothing implementation-specific, and a resize is
   seen at the next prompt.  Keys typed while the answer is on its way are kept, not lost.
   DEFAULT (or $COLUMNS) if the terminal does not answer."
  (%out s (format nil "~c7~c[999C~c[6n~c8" #\Escape #\Escape #\Escape #\Escape))
  (let ((next (session-next s)) (seen '()))
    (loop
      (let ((k (decode-key (lambda (&optional tmo) (funcall next (or tmo 0.3))))))
        (cond ((null k)
               (setf (session-pending s) (nreverse seen))
               (return (or (ignore-errors (parse-integer (getenv "COLUMNS"))) default)))
              ((and (consp k) (eq (first k) :cursor-pos))
               (setf (session-pending s) (nreverse seen))
               (return (max 2 (third k))))
              (t (push k seen)))))))

;;; ---- session commands -----------------------------------------------------------------

(defun accept-line (s) (setf (session-done s) t (session-status s) :ok))

(defun newline (s) (insert (session-buffer s) (string #\Newline)))

(defun delete-or-eof (s)
  "End of input on an empty line; otherwise delete forward, as C-d does in a shell."
  (let ((ed (session-buffer s)))
    (if (zerop (length (editor-text ed)))
        (setf (session-done s) t (session-status s) :eof)
        (delete-forward ed))))

(defun interrupt (s)
  (setf (session-done s) t (session-status s) :interrupt))

(defun %show-history-entry (s entry)
  (let ((ed (session-buffer s)))
    (setf (editor-text ed) entry (editor-point ed) (length entry) (editor-goal ed) nil)))

(defun up-or-history (s)
  "Up a line within multi-line text; from the first line, the previous history entry."
  (let ((ed (session-buffer s)))
    (unless (previous-line ed)
      (let ((h (session-history s)))
        (let ((e (and h (history-step h (editor-text ed) -1))))
          (if e (%show-history-entry s e) (%out s (string (code-char 7)))))))))

(defun down-or-history (s)
  "Down a line within multi-line text; from the last line, the next history entry."
  (let ((ed (session-buffer s)))
    (unless (next-line ed)
      (let ((h (session-history s)))
        (let ((e (and h (history-step h (editor-text ed) +1))))
          (when e (%show-history-entry s e)))))))

(defun clear-screen (s)
  (%out s (format nil "~c[H~c[2J" #\Escape #\Escape))
  (setf (session-cursor-row s) 0))

(defun suspend (s)
  "Ctrl-Z: give the terminal back, stop, and pick up again when continued.  On a terminal
   at the end of an :INPUT source there is no process to stop: a bell."
  (unless (session-local s)
    (return-from suspend (%out s (string (code-char 7)))))
  (let ((ed (session-buffer s)))
    (redisplay s :point (length (editor-text ed)))
    (%out s (format nil "~c~c" #\Return #\Linefeed))
    (raw-off)
    (suspend-self)
    (raw-on)
    (setf (session-cursor-row s) 0
          (session-width s) (terminal-columns s))))

(defun %common-prefix (strings)
  (let ((p (first strings)))
    (dolist (s (rest strings) p)
      (setf p (subseq p 0 (or (mismatch p s) (min (length p) (length s))))))))

(defvar *last-was-complete* nil)

(defun complete (s)
  "TAB: ask the completer for candidates for the text before point.  One candidate is
   inserted; several insert what they share, and a second TAB lists them."
  (let* ((ed (session-buffer s)) (fn (session-completer s)))
    (if (null fn)
        (insert ed "    ")                               ; no completer: TAB is indentation
        (multiple-value-bind (cands start) (funcall fn (editor-text ed) (editor-point ed))
          (let* ((p (editor-point ed)) (start (or start p))
                 (word (subseq (editor-text ed) start p)))
            (cond
              ((null cands) (%out s (string (code-char 7))))
              (t
               (let ((prefix (if (rest cands) (%common-prefix cands) (first cands))))
                 (if (> (length prefix) (length word))
                     (progn (%kill-region-silently ed start p) (insert ed prefix))
                     (when (and (rest cands) *last-was-complete*)
                       (%list-candidates s cands)))))))))))

(defun %kill-region-silently (ed start end)
  (let ((tx (editor-text ed)))
    (setf (editor-text ed) (concatenate 'string (subseq tx 0 start) (subseq tx end))
          (editor-point ed) start)))

(defun %list-candidates (s cands)
  "Print CANDS in columns below the input, then start a fresh drawing under them."
  (let* ((ed (session-buffer s))
         (colw (+ 2 (reduce #'max cands :key #'visible-width)))
         (per-row (max 1 (floor (session-width s) colw))))
    (redisplay s :point (length (editor-text ed)))
    (%out s (format nil "~c~c" #\Return #\Linefeed))
    (loop for c in cands for i from 1
          do (%out s c)
             (%out s (make-string (- colw (visible-width c)) :initial-element #\Space))
             (when (zerop (mod i per-row)) (%out s (format nil "~c~c" #\Return #\Linefeed))))
    (unless (zerop (mod (length cands) per-row)) (%out s (format nil "~c~c" #\Return #\Linefeed)))
    (setf (session-cursor-row s) 0)))

(defun reverse-search (s)
  "C-r: search history backward as you type.  C-r again finds an older match; Enter accepts
   the match as the line; C-g or Escape puts back what was there; any other key takes the
   match into the buffer and then does what that key does."
  (let* ((ed (session-buffer s)) (h (session-history s))
         (orig-text (editor-text ed)) (orig-point (editor-point ed))
         (pattern "") (match orig-text) (idx nil))
    (unless h (return-from reverse-search (%out s (string (code-char 7)))))
    (flet ((show (failing)
             (let ((pos (or (and (plusp (length pattern)) (search pattern match)) (length match))))
               (redisplay s :prompt (format nil "(~:[~;failing ~]reverse-i-search)`~a': "
                                            failing pattern)
                            :text match :point pos)))
           (find-from (before)
             (multiple-value-bind (e i) (history-search h pattern :before before)
               (when e (setf match e idx i)) e)))
      (show nil)
      (loop
        (let ((k (%next-key s)) (failing nil))
          (cond
            ((null k) (return))
            ((characterp k)
             (setf pattern (concatenate 'string pattern (string k)))
             (setf failing (not (find-from (if idx (1+ idx) (length (history-entries h)))))))
            ((eq k :backspace)
             (when (plusp (length pattern))
               (setf pattern (subseq pattern 0 (1- (length pattern))) idx nil)
               (if (plusp (length pattern))
                   (find-from (length (history-entries h)))
                   (setf match orig-text))))
            ((eq k :c-r) (setf failing (not (find-from (or idx (length (history-entries h)))))))
            ((member k '(:c-g :escape))
             (setf (editor-text ed) orig-text (editor-point ed) orig-point)
             (return))
            ((eq k :enter)
             (setf (editor-text ed) match (editor-point ed) (length match))
             (accept-line s) (return))
            (t                                           ; leave search, then act on the key
             (setf (editor-text ed) match (editor-point ed) (length match))
             (push k (session-pending s))
             (return)))
          (show failing))))))

;;; ---- keymaps --------------------------------------------------------------------------

(defun make-keymap (&optional parent)
  "A keymap, starting from a copy of PARENT's bindings."
  (let ((km (make-hash-table :test 'equal)))
    (when parent (maphash (lambda (k v) (setf (gethash k km) v)) parent))
    km))

(defun bind (keymap key command)
  "Bind KEY to COMMAND in KEYMAP.  COMMAND is a function of the session, or a symbol."
  (setf (gethash key keymap) command))

(defparameter *default-keymap*
  (let ((km (make-keymap)))
    (loop for (keys cmd) in
          '(((:enter) accept-line)            ((:newline) newline)
            ((:backspace :c-h) delete-backward) ((:delete) delete-forward)
            ((:c-d) delete-or-eof)
            ((:left :c-b) backward-char)      ((:right :c-f) forward-char)
            ((:home :c-a) beginning-of-line)  ((:end :c-e) end-of-line)
            ((:m-b :ctrl-left :meta-left) backward-word)
            ((:m-f :ctrl-right :meta-right) forward-word)
            ((:up :c-p) up-or-history)        ((:down :c-n) down-or-history)
            ((:m-< :page-up) beginning-of-buffer) ((:m-> :page-down) end-of-buffer)
            ((:c-k) kill-line)                ((:c-u) kill-to-line-start)
            ((:c-w :m-backspace) kill-word-backward) ((:m-d) kill-word-forward)
            ((:c-y) yank)                     ((:c-t) transpose-chars)
            ((:c-l) clear-screen)             ((:c-r) reverse-search)
            ((:tab) complete)
            ((:c-c) interrupt)                ((:c-z) suspend))
          do (dolist (k keys) (bind km k cmd)))
    km)
  "The bindings a new session starts from.  Copy it with MAKE-KEYMAP to add your own.")

(defun %normalize-paste (text)
  "A paste as text to insert: CR LF and lone CR become newlines."
  (with-output-to-string (o)
    (let ((i 0) (n (length text)))
      (loop while (< i n) do
        (let ((c (char text i)))
          (cond ((and (char= c #\Return) (< (1+ i) n) (char= (char text (1+ i)) #\Linefeed))
                 (write-char #\Newline o) (incf i 2))
                ((char= c #\Return) (write-char #\Newline o) (incf i))
                (t (write-char c o) (incf i))))))))

(defun dispatch (s key)
  "Act on one key event."
  (let* ((ed (session-buffer s))
         (cmd (and (not (characterp key)) (gethash key (session-keymap s)))))
    (cond
      ((characterp key) (insert ed (string key)) (setf cmd 'self-insert))
      ((and (consp key) (eq (first key) :paste)) (insert ed (%normalize-paste (second key)))
       (setf cmd 'paste))
      ((null cmd) nil)                                  ; unbound, :unknown, stray reports
      ((functionp cmd) (funcall cmd s))
      ((member cmd *buffer-commands*) (funcall cmd ed))
      (t (funcall cmd s)))
    (unless (member cmd *vertical-commands*) (setf (editor-goal ed) nil))
    (setf *last-was-complete* (eq cmd 'complete))
    (setf (editor-last ed) cmd)))

;;; ---- the loop -------------------------------------------------------------------------

(defun read-line-edited (&key (prompt "") continuation history completer
                              (keymap *default-keymap*) (initial "") (match-parens t)
                              (output *standard-output*) input width)
  "Read one line, with editing, from the terminal.  Returns (values text status), STATUS
   being :OK, :EOF (C-d on an empty line) or :INTERRUPT (C-c).

   PROMPT may contain colour escapes.  CONTINUATION prefixes the second and later lines of
   multi-line input (C-j inserts a newline; Enter submits).  HISTORY is a HISTORY; an
   accepted, non-blank line is added to it.  COMPLETER, if given, is called as
   (funcall completer text point) and returns (values candidates start), the candidates
   replacing TEXT from START to point.

   When stdin is not a terminal there is nothing to edit: PROMPT is written and a plain line
   read, so the same call serves a piped session.

   INPUT is a terminal at the end of something other than this process's stdin -- an SSH
   channel, a window on the same machine: a function (&optional timeout) -> the next byte,
   or NIL when TIMEOUT seconds pass first or the input has ended (TIMEOUT NIL waits).  The
   terminal is taken to be in raw mode already, and nothing here touches this process's.
   WIDTH, when given, is the terminal's width; otherwise the terminal is asked."
  (unless (or input (tty-p 0))
    (write-string prompt output) (force-output output)
    (let ((line (read-line *standard-input* nil)))
      (return-from read-line-edited (if line (values line :ok) (values "" :eof)))))
  (let ((s (%make-session :buffer (make-editor :text initial) :history history
                          :prompt prompt :continuation continuation :completer completer
                          :keymap keymap :match-parens match-parens :output output
                          :next (or input #'read-byte-timeout) :local (null input))))
    (when (session-local s) (raw-on))
    (%out s (format nil "~c[?2004h" #\Escape))            ; bracketed paste on
    (unwind-protect
         (progn
           (setf (session-width s) (or width (terminal-columns s)))
           (redisplay s)
           (loop until (session-done s)
                 do (let ((k (%next-key s)))
                      (if (null k)
                          (setf (session-done s) t (session-status s) :eof)
                          (dispatch s k)))
                    (unless (session-done s) (redisplay s)))
           ;; leave the cursor below the finished input, not in the middle of it
           (let ((ed (session-buffer s)))
             (setf (session-match-parens s) nil)
             (redisplay s :point (length (editor-text ed)))
             (when (eq (session-status s) :interrupt) (%out s "^C"))
             (%out s (format nil "~c~c" #\Return #\Linefeed))))
      (%out s (format nil "~c[?2004l" #\Escape))
      (when (session-local s) (raw-off)))
    (let ((text (editor-text (session-buffer s))))
      (case (session-status s)
        (:ok (when history (history-add history text)) (values text :ok))
        (:interrupt (values "" :interrupt))
        (t (values "" :eof))))))
