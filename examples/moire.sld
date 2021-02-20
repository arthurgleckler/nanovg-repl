;;;; Moire demo in Scheme

;;;; Copyright MMXXI Arthur A. Gleckler.

;;;; See MIT license in "LICENSE" file.

(define-library (moire)
  (cond-expand
   (chibi (import (chibi pathname)))
   (mit (import (only (mit legacy runtime)
		      enough-namestring
		      merge-pathnames
		      unspecific))
	(begin
	  (define (path-resolve path directory)
	    (enough-namestring (merge-pathnames path directory))))))
  (export moire)
  (import (scheme base))
  (import (nanovg-repl))
  (include "moire.scm"))