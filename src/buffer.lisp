;;;; buffer.lisp — the text being edited, and every edit, with no terminal in sight.
;;;;
;;;; An EDITOR is a string, a point (an index into it, 0..length), a kill ring of one, and
;;;; the goal column that vertical motion tries to keep.  Every command below changes those
;;;; and nothing else, so the whole editing behaviour can be tested with strings.
;;;;
;;;; The text may contain newlines: that is how multi-line input is represented.  "Line"
;;;; below means a LOGICAL line (between newlines), not a screen row -- wrapping is the
;;;; renderer's business.

(in-package #:quill)

(defstruct (editor (:constructor make-editor (&key (text "") (point (length text)))))
  (text "" :type string)
  (point 0 :type fixnum)
  (kill "" :type string)
  (goal nil)                     ; column vertical motion is aiming for, or NIL
  (last nil))                    ; the previous command, so consecutive kills accumulate

(defun %set (ed text point)
  (setf (editor-text ed) text
        (editor-point ed) (max 0 (min point (length text)))))

;;; ---- insertion and deletion -----------------------------------------------------------

(defun insert (ed string)
  "Insert STRING at point and move past it."
  (let ((tx (editor-text ed)) (p (editor-point ed)))
    (%set ed (concatenate 'string (subseq tx 0 p) string (subseq tx p)) (+ p (length string)))))

(defun delete-backward (ed)
  (let ((tx (editor-text ed)) (p (editor-point ed)))
    (when (plusp p) (%set ed (concatenate 'string (subseq tx 0 (1- p)) (subseq tx p)) (1- p)))))

(defun delete-forward (ed)
  (let ((tx (editor-text ed)) (p (editor-point ed)))
    (when (< p (length tx)) (%set ed (concatenate 'string (subseq tx 0 p) (subseq tx (1+ p))) p))))

;;; ---- horizontal motion ----------------------------------------------------------------

(defun forward-char (ed)  (%set ed (editor-text ed) (1+ (editor-point ed))))
(defun backward-char (ed) (%set ed (editor-text ed) (1- (editor-point ed))))

(defun line-start (text point)
  "Index where the logical line containing POINT begins."
  (let ((nl (position #\Newline text :end point :from-end t))) (if nl (1+ nl) 0)))

(defun line-end (text point)
  "Index where the logical line containing POINT ends (before its newline)."
  (or (position #\Newline text :start point) (length text)))

(defun beginning-of-line (ed) (%set ed (editor-text ed) (line-start (editor-text ed) (editor-point ed))))
(defun end-of-line (ed)       (%set ed (editor-text ed) (line-end (editor-text ed) (editor-point ed))))
(defun beginning-of-buffer (ed) (%set ed (editor-text ed) 0))
(defun end-of-buffer (ed)       (%set ed (editor-text ed) (length (editor-text ed))))

(defun word-char-p (c) (or (alphanumericp c) (char= c #\_)))

(defun %word-forward-index (text p)
  "Skip non-word characters, then a word."
  (let ((n (length text)))
    (loop while (and (< p n) (not (word-char-p (char text p)))) do (incf p))
    (loop while (and (< p n) (word-char-p (char text p))) do (incf p))
    p))

(defun %word-backward-index (text p)
  (loop while (and (plusp p) (not (word-char-p (char text (1- p))))) do (decf p))
  (loop while (and (plusp p) (word-char-p (char text (1- p)))) do (decf p))
  p)

(defun forward-word (ed)  (%set ed (editor-text ed) (%word-forward-index (editor-text ed) (editor-point ed))))
(defun backward-word (ed) (%set ed (editor-text ed) (%word-backward-index (editor-text ed) (editor-point ed))))

;;; ---- vertical motion, within the text -------------------------------------------------

(defun %move-line (ed direction)
  "Move to the previous (-1) or next (+1) logical line, keeping the goal column.  Returns
   NIL without moving if there is no such line -- which is the caller's cue to go to
   history instead."
  (let* ((tx (editor-text ed)) (p (editor-point ed))
         (bol (line-start tx p))
         (goal (or (editor-goal ed) (- p bol))))
    (let ((target-bol
            (if (minusp direction)
                (and (plusp bol) (line-start tx (1- bol)))
                (let ((eol (line-end tx p))) (and (< eol (length tx)) (1+ eol))))))
      (when target-bol
        (%set ed tx (min (+ target-bol goal) (line-end tx target-bol)))
        (setf (editor-goal ed) goal)
        t))))

(defun previous-line (ed) (%move-line ed -1))
(defun next-line (ed)     (%move-line ed +1))

;;; ---- killing and yanking --------------------------------------------------------------

(defun %kill (ed start end &key prepend)
  "Remove START..END into the kill ring.  Consecutive kills accumulate, the way Emacs does,
   so that several C-k in a row yank back as one piece."
  (let* ((tx (editor-text ed)) (piece (subseq tx start end))
         (accumulate (member (editor-last ed) '(kill-line kill-to-line-start kill-word-backward
                                                kill-word-forward))))
    (setf (editor-kill ed) (cond ((not accumulate) piece)
                                 (prepend (concatenate 'string piece (editor-kill ed)))
                                 (t (concatenate 'string (editor-kill ed) piece))))
    (%set ed (concatenate 'string (subseq tx 0 start) (subseq tx end)) start)))

(defun kill-line (ed)
  "Kill to the end of the logical line; at the end of a line, kill the newline itself."
  (let* ((tx (editor-text ed)) (p (editor-point ed)) (eol (line-end tx p)))
    (%kill ed p (if (and (= p eol) (< eol (length tx))) (1+ eol) eol))))

(defun kill-to-line-start (ed)
  (let ((p (editor-point ed))) (%kill ed (line-start (editor-text ed) p) p :prepend t)))

(defun kill-word-backward (ed)
  (let ((p (editor-point ed))) (%kill ed (%word-backward-index (editor-text ed) p) p :prepend t)))

(defun kill-word-forward (ed)
  (let ((p (editor-point ed))) (%kill ed p (%word-forward-index (editor-text ed) p))))

(defun yank (ed) (insert ed (editor-kill ed)))

(defun transpose-chars (ed)
  "Swap the characters around point (at the end of the text, the two before it)."
  (let* ((tx (copy-seq (editor-text ed))) (p (editor-point ed)) (n (length tx)))
    (when (>= n 2)
      (let ((i (cond ((= p 0) 1) ((= p n) (1- n)) (t p))))
        (rotatef (char tx (1- i)) (char tx i))
        (%set ed tx (1+ i))))))

;;; ---- paren matching -------------------------------------------------------------------

(defun matching-open-paren (text point)
  "If the character before POINT closes a paren, the index of the one it matches, else NIL.
   Strings and comments are not understood: this is a visual aid, not a reader."
  (when (and (plusp point) (member (char text (1- point)) '(#\) #\] #\})))
    (let* ((close (char text (1- point)))
           (open (ecase close (#\) #\() (#\] #\[) (#\} #\{)))
           (depth 0))
      (loop for i from (- point 2) downto 0
            for c = (char text i)
            do (cond ((char= c close) (incf depth))
                     ((char= c open) (if (zerop depth) (return i) (decf depth))))))))
