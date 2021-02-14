;;;; Image demo in Scheme: library declaration

;;;; Copyright MMXXI Arthur A. Gleckler.

;;;; See MIT license in "LICENSE" file.

(define-library (image)
  (cond-expand (chibi (import (chibi pathname))))
  (export image-demo)
  (import (scheme base))
  (import (nanovg-repl))
  (include "image.scm"))