;;;; Moire demo in Scheme

;;;; Copyright MMXXI Arthur A. Gleckler.

;;;; See MIT license in "LICENSE" file.

(define-library (moire)
  (cond-expand (chibi (import (chibi pathname))))
  (export moire)
  (import (scheme base))
  (import (nanovg-repl))
  (include "moire.scm"))