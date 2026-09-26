#!/usr/bin/env python3
"""quill under a real pseudo-terminal.

tests.lisp drives sessions from byte vectors; this checks the parts that only exist on a
terminal: raw mode (Enter arriving as CR, Ctrl-C as a byte rather than a signal), the
width query answered by the terminal, a lone ESC told apart from a sequence by timing,
bracketed paste switched on, and the terminal put back the way it was found.

We are the terminal: whatever quill writes lands here, and we answer its CSI 6n with a
cursor position report.  Usage: python3 t/pty-test.py   (exit status 0 = all passed)
"""
import os, pty, re, select, sys, time

HERE = os.path.dirname(os.path.abspath(__file__))
ROOT = os.path.dirname(HERE)
WIDTH = 30

DRIVER = f"""
(require :asdf)
(push #p"{ROOT}/" asdf:*central-registry*)
(asdf:load-system :quill)
(defun canon-p () (logtest sb-posix:icanon (sb-posix:termios-lflag (sb-posix:tcgetattr 0))))
(let ((h (quill:make-history)))
  (loop
    (multiple-value-bind (text status) (quill:read-line-edited :prompt "q> " :history h)
      (format t "RESULT ~s ~s canon=~a~%" text status (canon-p))
      (finish-output)
      (when (eq status :eof) (return)))))
"""

import tempfile
driver = tempfile.NamedTemporaryFile("w", suffix=".lisp", delete=False)
driver.write(DRIVER)
driver.close()

pid, fd = pty.fork()
if pid == 0:
    os.execvp("sbcl", ["sbcl", "--noinform", "--no-userinit", "--non-interactive",
                       "--load", driver.name])

out = bytearray()        # everything quill wrote since the last RESULT

def pump(timeout):
    r, _, _ = select.select([fd], [], [], timeout)
    if not r:
        return False
    try:
        data = os.read(fd, 4096)
    except OSError:
        return False
    out.extend(data)
    if b"\x1b[6n" in data:                       # the width query: answer it like a terminal
        os.write(fd, f"\x1b[1;{WIDTH}R".encode())
    return True

def wait_for(pattern, timeout=60):
    end = time.time() + timeout
    while time.time() < end:
        m = re.search(pattern, bytes(out))   # a snapshot: a match on the bytearray reads it lazily
        if m:
            return m
        pump(0.1)
    return None

def result(keys, pause=0.0):
    """Type KEYS (a list of byte strings, PAUSE apart) and return the next RESULT line."""
    del out[:]
    if not wait_for(rb"q> "):
        return ("<no prompt>", "", "", bytes(out))
    for k in keys:
        os.write(fd, k)
        if pause:
            time.sleep(pause)
    m = wait_for(rb'RESULT "((?:[^"\\]|\\.)*)" :(\w+) canon=(\w+)')
    if not m:
        return ("<no result>", "", "", bytes(out))
    screen = bytes(out[:m.start()])
    del out[:m.end()]
    text = m.group(1).decode().replace("\r\n", "\n")   # the tty adds the CR on output
    return (text, m.group(2).decode(), m.group(3).decode(), screen)

passed = failed = 0
def check(name, got, want):
    global passed, failed
    if got == want:
        passed += 1
    else:
        failed += 1
        print(f"FAIL {name}\n  expected {want!r}\n  got      {got!r}")

ESC = b"\x1b"
r = result([b"hello\r"])
check("plain line, Enter as CR", r[:3], ("hello", "OK", "T"))
check("bracketed paste enabled", b"\x1b[?2004h" in r[3], True)

r = result([b"one", b"\n", b"two\r"])
check("C-j is a newline, not submit", r[:2], ("one\ntwo", "OK"))

r = result([b"abc", b"\x01", b"X\r"])
check("C-a", r[:2], ("Xabc", "OK"))

r = result([ESC + b"[A", b"\r"])
check("up recalls history", r[:2], ("Xabc", "OK"))

r = result([b"keep", ESC + b"b", b"Z\r"])
check("ESC b together is Meta-b", r[:2], ("Zkeep", "OK"))

r = result([b"keep", ESC, b"b", b"Z\r"], pause=0.2)
check("ESC, a pause, then b is Escape then b", r[:2], ("keepbZ", "OK"))

r = result([b"abc", b"\x03"])
check("C-c is a byte, not SIGINT", r[:2], ("", "INTERRUPT"))

r = result([ESC + b"[200~line1\rline2" + ESC + b"[201~", b"\r"])
check("paste does not submit", r[:2], ("line1\nline2", "OK"))

# a line longer than the terminal wraps: quill must have used the width we reported
r = result([b"x" * 40 + b"\r"])
check("long line accepted", r[:2], ("x" * 40, "OK"))
check("wrapped at the reported width",
      re.search(rb"q> x{%d}\r+\nx" % (WIDTH - 3), r[3]) is not None, True)

r = result([b"\x04"])
check("C-d on empty line is EOF", r[:3], ("", "EOF", "T"))
check("bracketed paste disabled on exit", b"\x1b[?2004l" in r[3], True)

try:
    os.waitpid(pid, 0)
except ChildProcessError:
    pass
os.unlink(driver.name)
print(f"quill pty: {passed} passed, {failed} failed")
sys.exit(1 if failed else 0)
