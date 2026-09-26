;;;; render.lisp — where every character lands on screen, and the bytes that put it there.
;;;;
;;;; Pure: given the prompt, the text, point and the terminal's width, LAYOUT returns the
;;;; screen rows and the cursor's row and column; RENDER returns the escape-sequence string
;;;; that redraws them.  Nothing is written here, so all of it can be checked against expected
;;;; strings.
;;;;
;;;; The terminal is assumed to speak the VT100/xterm subset every emulator in use speaks:
;;;; cursor up (CSI n A), right (CSI n C), clear to end of screen (CSI J), SGR for the paren
;;;; highlight.  No terminfo: the sequences below are the ones terminfo would have returned.

(in-package #:quill)

(defun char-width (c)
  "Columns C occupies.  0 for combining marks, 2 for East Asian wide characters and emoji.
   An approximation of wcwidth over the ranges that come up in practice."
  (let ((code (char-code c)))
    (cond ((or (<= #x0300 code #x036F) (<= #x200B code #x200F) (<= #xFE00 code #xFE0F)
               (<= #x1AB0 code #x1AFF) (<= #x20D0 code #x20FF))
           0)
          ((or (<= #x1100 code #x115F) (<= #x2E80 code #x303E) (<= #x3041 code #x33FF)
               (<= #x3400 code #x4DBF) (<= #x4E00 code #x9FFF) (<= #xA000 code #xA4CF)
               (<= #xAC00 code #xD7A3) (<= #xF900 code #xFAFF) (<= #xFE30 code #xFE4F)
               (<= #xFF00 code #xFF60) (<= #xFFE0 code #xFFE6)
               (<= #x1F300 code #x1F64F) (<= #x1F900 code #x1F9FF) (<= #x1FA70 code #x1FAFF)
               (<= #x20000 code #x3FFFD))
           2)
          (t 1))))

(defun %display (c)
  "What C looks like on screen: a tab as spaces, other control characters as ^X."
  (let ((code (char-code c)))
    (cond ((char= c #\Tab) "    ")
          ((or (< code 32) (= code 127)) (format nil "^~c" (code-char (logxor code 64))))
          (t (string c)))))

(defun %display-width (c)
  (let ((code (char-code c)))
    (cond ((char= c #\Tab) 4)
          ((or (< code 32) (= code 127)) 2)
          (t (char-width c)))))

(defun visible-width (s)
  "Columns S occupies, skipping escape sequences -- so a prompt can carry colour."
  (let ((w 0) (i 0) (n (length s)))
    (loop while (< i n) do
      (let ((c (char s i)))
        (cond ((and (char= c #\Escape) (< (1+ i) n) (char= (char s (1+ i)) #\[))
               (incf i 2)                                       ; CSI ... final byte
               (loop while (and (< i n) (not (<= #x40 (char-code (char s i)) #x7E))) do (incf i))
               (incf i))
              ((and (char= c #\Escape) (< (1+ i) n) (char= (char s (1+ i)) #\]))
               (incf i 2)                                       ; OSC ... BEL
               (loop while (and (< i n) (char/= (char s i) (code-char 7))) do (incf i))
               (incf i))
              (t (incf w (char-width c)) (incf i)))))
    w))

(defun layout (prompt text point width &key continuation highlight)
  "Screen rows for PROMPT followed by TEXT on a terminal WIDTH columns wide.  Logical lines
   after the first start with CONTINUATION (default: blanks as wide as the prompt, so the
   text lines up).  HIGHLIGHT is an index into TEXT to draw in reverse video.
   Returns (values rows cursor-row cursor-col)."
  (let* ((width (max 2 width))
         (cont (or continuation (make-string (visible-width prompt) :initial-element #\Space)))
         (rows (list (make-string-output-stream)))
         (row 0) (col (visible-width prompt))
         (pending nil)                    ; the last row is exactly full; the NEXT char wraps
         (crow 0) (ccol 0))
    (write-string prompt (first rows))
    (flet ((new-row (lead)
             (push (make-string-output-stream) rows) (incf row)
             (write-string lead (first rows)) (setf col (visible-width lead) pending nil))
           (note-cursor ()
             (if pending (setf crow (1+ row) ccol 0) (setf crow row ccol col))))
      (loop for i from 0 below (length text)
            for c = (char text i)
            do (when (= i point) (note-cursor))
               (if (char= c #\Newline)
                   (new-row cont)                  ; a newline ends a full row without a blank one
                   (let ((w (%display-width c)))
                     (when pending (new-row ""))
                     (when (> (+ col w) width) (new-row ""))
                     (if (eql i highlight)
                         (format (first rows) "~c[7m~a~c[27m" #\Escape (%display c) #\Escape)
                         (write-string (%display c) (first rows)))
                     (incf col w)
                     (when (>= col width) (setf pending t)))))
      (when (= point (length text))
        (note-cursor)
        (when pending (new-row ""))))             ; the cursor needs a row to stand on
    (values (nreverse (mapcar #'get-output-stream-string rows)) crow ccol)))

(defun render (prompt text point width &key continuation highlight (previous-cursor-row 0))
  "The bytes that erase the previous drawing and draw this one.  PREVIOUS-CURSOR-ROW is where
   the cursor was left last time, counted from the first row of the drawing.  Returns
   (values string cursor-row), the second to be passed back next time."
  (multiple-value-bind (rows crow ccol)
      (layout prompt text point width :continuation continuation :highlight highlight)
    (let ((last-row (1- (length rows))))
      (values
       (with-output-to-string (o)
         (when (plusp previous-cursor-row) (format o "~c[~dA" #\Escape previous-cursor-row))
         (format o "~c~c[J" #\Return #\Escape)
         (loop for (r . more) on rows do (write-string r o) (when more (format o "~c~c" #\Return #\Linefeed)))
         ;; from the end of the last row to the cursor, by relative moves only -- they are right
         ;; however far the screen has scrolled
         (when (> last-row crow) (format o "~c[~dA" #\Escape (- last-row crow)))
         (write-char #\Return o)
         (when (plusp ccol) (format o "~c[~dC" #\Escape ccol)))
       crow))))
