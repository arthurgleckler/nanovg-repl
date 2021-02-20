;;;; Image demo in Scheme: library declaration

;;;; Copyright MMXXI Arthur A. Gleckler.

;;;; See MIT license in "LICENSE" file.

(define-library (image)
  (cond-expand
   (chibi (import (chibi pathname)))
   (mit (import (only (mit legacy runtime)
		      enough-namestring
		      merge-pathnames
		      unspecific))
	(begin
	  (define (path-resolve path directory)
	    (enough-namestring (merge-pathnames path directory))))))
  (export image-demo)
  (import (scheme base))
  (import (nanovg-repl))
  (include "image.scm"))