;;;; history.lisp — what was entered before, and walking back through it.
;;;;
;;;; Entries are kept oldest first.  Browsing starts past the newest entry; the text you were
;;;; in the middle of typing is saved when you step away from it and handed back when you
;;;; step forward past the newest, so arrowing up to look and back down loses nothing.
;;;;
;;;; On disk, one entry per line, with backslash and newline escaped -- a multi-line entry
;;;; stays one entry across a restart instead of coming back as several.

(in-package #:quill)

(defstruct (history (:constructor %make-history))
  (entries (make-array 0 :adjustable t :fill-pointer 0))
  (index nil)                     ; which entry is shown while browsing, or NIL
  (draft nil)                     ; what was being typed when browsing began
  (file nil)                      ; where entries are appended, or NIL for memory only
  (max 1000))

(defun %escape-entry (s)
  (with-output-to-string (o)
    (loop for c across s do
      (case c (#\\ (write-string "\\\\" o)) (#\Newline (write-string "\\n" o)) (t (write-char c o))))))

(defun %unescape-entry (s)
  (with-output-to-string (o)
    (let ((i 0) (n (length s)))
      (loop while (< i n) do
        (let ((c (char s i)))
          (if (and (char= c #\\) (< (1+ i) n))
              (progn (write-char (case (char s (1+ i)) (#\n #\Newline) (t (char s (1+ i)))) o)
                     (incf i 2))
              (progn (write-char c o) (incf i))))))))

(defun load-history (h)
  "Read H's file into it, if there is one."
  (let ((file (history-file h)))
    (when (and file (probe-file file))
      (with-open-file (in file :external-format :utf-8)
        (loop for line = (read-line in nil) while line
              when (plusp (length line))
                do (vector-push-extend (%unescape-entry line) (history-entries h)))))
    ;; keep only the newest MAX
    (let* ((e (history-entries h)) (extra (- (length e) (history-max h))))
      (when (plusp extra)
        (setf (history-entries h)
              (make-array (history-max h) :adjustable t :fill-pointer (history-max h)
                                          :initial-contents (subseq e extra)))))
    h))

(defun make-history (&key file (max 1000))
  "A history, loaded from FILE if given; entries added later are appended to it."
  (load-history (%make-history :file (and file (pathname file)) :max max)))

(defun history-add (h line)
  "Record LINE, unless it is blank or repeats the newest entry.  Ends any browsing."
  (setf (history-index h) nil (history-draft h) nil)
  (let ((e (history-entries h)))
    (unless (or (zerop (length (string-trim '(#\Space #\Tab #\Newline) line)))
                (and (plusp (length e)) (string= line (aref e (1- (length e))))))
      (vector-push-extend line e)
      (when (history-file h)
        (ignore-errors
          (ensure-directories-exist (history-file h))
          (with-open-file (out (history-file h) :direction :output :if-exists :append
                                                :if-does-not-exist :create :external-format :utf-8)
            (write-line (%escape-entry line) out))))))
  line)

(defun history-step (h current direction)
  "The entry DIRECTION (-1 older, +1 newer) from the one shown, given the text CURRENTLY in
   the editor.  NIL if there is nowhere to go.  Stepping newer than the newest returns the
   draft that was being typed when browsing began."
  (let* ((e (history-entries h)) (n (length e))
         (i (history-index h)))
    (when (null i) (setf (history-draft h) current))
    (let ((next (if (null i)
                    (and (minusp direction) (plusp n) (1- n))
                    (+ i direction))))
      (cond ((null next) nil)
            ((minusp next) nil)                              ; already at the oldest
            ((>= next n)                                     ; back past the newest
             (setf (history-index h) nil)
             (or (history-draft h) ""))
            (t (setf (history-index h) next) (aref e next))))))

(defun history-search (h pattern &key (before (length (history-entries h))))
  "The newest entry before index BEFORE containing PATTERN, as (values entry index), or NIL."
  (let ((e (history-entries h)))
    (loop for i from (1- (min before (length e))) downto 0
          when (search pattern (aref e i))
            do (return (values (aref e i) i)))))
