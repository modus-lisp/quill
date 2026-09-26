# quill

A terminal line editor in pure Common Lisp. No FFI, no dependencies.

```lisp
(let ((h (quill:make-history :file "~/.myapp-history")))
  (multiple-value-bind (text status)
      (quill:read-line-edited :prompt "> " :history h)
    ...))   ; status is :ok, :eof (C-d on an empty line) or :interrupt (C-c)
```

- Emacs keys: C-a/C-e, M-b/M-f, C-k/C-u/C-w/M-d with a kill ring that accumulates, C-y, C-t
- Multi-line input: C-j inserts a newline, Enter submits; up/down move between lines
  and fall through to history at the first/last line
- History that survives restarts (multi-line entries stay one entry), C-r reverse search
- Bracketed paste: a pasted block is inserted whole and never submits itself
- Completion (`:completer`), paren matching, wide characters, prompts with colour
- When stdin isn't a terminal it's a plain `read-line`, so piped sessions work unchanged

Keys are bound with `(quill:bind keymap key command)` on a copy of `*default-keymap*`;
a command is any function of the session.

## Porting

Everything is portable CL except `src/port.lisp`: raw mode, a timed one-byte read, and
isatty. The terminal's width is asked of the terminal itself (a cursor-position report),
so no ioctl is needed. SBCL is implemented; other Lisps need those few functions.

## Tests

```sh
sbcl --eval '(asdf:test-system :quill)'   # pure layers + whole sessions from byte strings
python3 t/pty-test.py                      # under a real pseudo-terminal
```
