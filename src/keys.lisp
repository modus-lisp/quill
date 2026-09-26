;;;; keys.lisp — bytes in, key events out.
;;;;
;;;; A pure function of a byte source, so it can be tested with a vector as easily as a
;;;; terminal.  The source is a function (&optional timeout) -> byte or NIL.
;;;;
;;;; Events are either a CHARACTER (something to insert) or a keyword naming a key, so a
;;;; keymap is just an EQUAL hash table:
;;;;
;;;;   :enter :newline :tab :backspace :escape :delete :insert
;;;;   :up :down :left :right :home :end :page-up :page-down
;;;;   :ctrl-left :ctrl-right :meta-left :meta-right
;;;;   :c-a ... :c-z          control letters (except Tab, Ctrl-J and Enter, named above)
;;;;   :m-<char>              Meta (Alt / ESC-prefix) plus a character, e.g. :m-b :m-<
;;;;   :m-backspace
;;;;   (:paste "text")        a bracketed paste, delivered whole
;;;;   (:cursor-pos row col)  the terminal answering a position query
;;;;   :unknown               an escape sequence quill does not recognise; ignored

(in-package #:quill)

(defparameter *escape-timeout* 0.05
  "How long after an ESC to wait for the rest of a sequence before deciding it was the ESC
   key on its own.  Short enough that Escape feels immediate, long enough for a sequence
   arriving over ssh.")

(defun %utf8-rest (lead next)
  "Finish decoding a UTF-8 character whose first byte is LEAD."
  (multiple-value-bind (n init)
      (cond ((= (logand lead #xE0) #xC0) (values 1 (logand lead #x1F)))
            ((= (logand lead #xF0) #xE0) (values 2 (logand lead #x0F)))
            ((= (logand lead #xF8) #xF0) (values 3 (logand lead #x07)))
            (t (values 0 nil)))
    (if (null init)
        #\?                                        ; a stray continuation byte
        (let ((code init))
          (dotimes (i n)
            (let ((b (funcall next *escape-timeout*)))
              (if (and b (= (logand b #xC0) #x80))
                  (setf code (logior (ash code 6) (logand b #x3F)))
                  (return-from %utf8-rest #\?))))
          (or (and (< code char-code-limit) (not (<= #xD800 code #xDFFF)) (code-char code))
              #\?)))))

(defun %meta-key (ch)
  (case ch
    ((#\Rubout #\Backspace) :m-backspace)
    (t (intern (format nil "M-~a" (string-upcase (string ch))) :keyword))))

(defun %read-csi (next)
  "After ESC [, read parameters and the final byte.  Returns (values params-string final)."
  (let ((params (make-string-output-stream)))
    (loop
      (let ((b (funcall next *escape-timeout*)))
        (cond ((null b) (return (values (get-output-stream-string params) nil)))
              ((<= #x40 b #x7E) (return (values (get-output-stream-string params)
                                                (code-char b))))
              (t (write-char (code-char b) params)))))))

(defun utf8-octets-to-string (octets)
  "OCTETS decoded from UTF-8, leniently: a malformed sequence becomes #\?."
  (let ((pos 0) (n (length octets)))
    (with-output-to-string (out)
      (loop while (< pos n) do
        (let ((lead (aref octets pos)))
          (incf pos)
          (write-char (if (< lead #x80)
                          (code-char lead)
                          (%utf8-rest lead (lambda (&optional tmo) (declare (ignore tmo))
                                             (when (< pos n) (prog1 (aref octets pos) (incf pos))))))
                      out))))))

(defun %read-paste (next)
  "Everything up to the closing ESC [ 2 0 1 ~.  Collected as raw bytes and decoded once at
   the end, so a multi-byte character can never be split by the terminator check."
  (let* ((end (map 'vector #'char-code (format nil "~c[201~~" #\Escape)))
         (el (length end))
         (buf (make-array 256 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0)))
    (loop
      (let ((b (funcall next 1.0)))
        (when (null b) (return))                   ; the terminal never closed it: keep what came
        (vector-push-extend b buf)
        (when (and (>= (fill-pointer buf) el)
                   (not (mismatch buf end :start1 (- (fill-pointer buf) el))))
          (setf (fill-pointer buf) (- (fill-pointer buf) el))
          (return))))
    (utf8-octets-to-string buf)))

(defun %csi-key (params final next)
  (flet ((mod-of (p) (let ((semi (position #\; p)))
                       (and semi (ignore-errors (parse-integer p :start (1+ semi)))))))
    (let ((m (mod-of params)))
      (case final
        ((#\A #\B #\C #\D)
         (let ((base (ecase final (#\A :up) (#\B :down) (#\C :right) (#\D :left))))
           (cond ((and (eql m 5) (eq base :right)) :ctrl-right)
                 ((and (eql m 5) (eq base :left)) :ctrl-left)
                 ((and (eql m 3) (eq base :right)) :meta-right)
                 ((and (eql m 3) (eq base :left)) :meta-left)
                 (t base))))
        (#\H :home)
        (#\F :end)
        (#\R (let ((semi (position #\; params)))       ; cursor position report
               (if semi
                   (list :cursor-pos
                         (or (ignore-errors (parse-integer params :end semi)) 0)
                         (or (ignore-errors (parse-integer params :start (1+ semi))) 0))
                   :unknown)))
        (#\~ (let ((n (ignore-errors (parse-integer params :junk-allowed t))))
               (case n
                 ((1 7) :home) ((4 8) :end) (2 :insert) (3 :delete)
                 (5 :page-up) (6 :page-down)
                 (200 (list :paste (%read-paste next)))
                 (t :unknown))))
        (t :unknown)))))

(defun decode-key (next)
  "Read one key event from NEXT.  NIL at end of input."
  (let ((b (funcall next)))
    (cond
      ((null b) nil)
      ((= b 13) :enter)
      ((= b 10) :newline)
      ((= b 9) :tab)
      ((or (= b 127) (= b 8)) :backspace)
      ((= b 27)
       (let ((b2 (funcall next *escape-timeout*)))
         (cond
           ((null b2) :escape)
           ((= b2 (char-code #\[))
            (multiple-value-bind (params final) (%read-csi next)
              (if final (%csi-key params final next) :escape)))
           ((= b2 (char-code #\O))                    ; SS3: arrows in application mode
            (let ((b3 (funcall next *escape-timeout*)))
              (case (and b3 (code-char b3))
                (#\A :up) (#\B :down) (#\C :right) (#\D :left) (#\H :home) (#\F :end)
                (t :unknown))))
           ((= b2 27) :escape)
           (t (%meta-key (if (< b2 #x80) (code-char b2) (%utf8-rest b2 next)))))))
      ((< b 32) (intern (string-upcase (format nil "C-~a" (code-char (+ b 96)))) :keyword))
      ((< b #x80) (code-char b))
      (t (%utf8-rest b next)))))

(defun byte-source (octets)
  "A byte source over OCTETS, for tests: returns the next one, NIL when they run out."
  (let ((i 0))
    (lambda (&optional timeout)
      (declare (ignore timeout))
      (when (< i (length octets)) (prog1 (aref octets i) (incf i))))))
