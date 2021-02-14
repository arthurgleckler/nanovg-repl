;;;; Stomachion demo in Scheme: library declaration

;;;; Copyright MMXXI Arthur A. Gleckler.

;;;; See MIT license in "LICENSE" file.

(define-library (stomachion)
  (cond-expand (chibi (import (chibi pathname))))
  (export stomachion)
  (import (scheme base))
  (import (nanovg-repl))
  (include "stomachion.scm"))