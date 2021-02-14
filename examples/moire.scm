;;;; Moire demo in Scheme

;;;; Copyright MMXXI Arthur A. Gleckler.

;;;; See MIT license in "LICENSE" file.

(cond-expand
 (mit (declare (usual-integrations))
      (define (path-resolve path directory)
	(enough-namestring (merge-pathnames path directory)))))

(define (moire repl-root)
  (let ((repl-pathname (path-resolve "build/repl" repl-root))
	(width 1000)
	(height 1000))
    (parameterize ((current-nanovg-window
		    (make-nanovg-window repl-pathname 800 800 "Moire")))
      (nanovg-cleanup
       (lambda ()
	 (let loop ((t 0))
	   (cond ((window-should-close?)
		  (shutdown))
		 (else
		  (let-values (((fb-width fb-height) (frame-buffer-size))
			       ((window-width win-height) (window-size)))
		    (let ((line-width (+ 1 (remainder t 10)))
			  (px-ratio (/ fb-width window-width))
			  (step (+ 1 (remainder t 100))))
		      (viewport 0 0 fb-width fb-height)
		      (clear-color 0 1 0 1)
		      (clear 1 1 1)
		      (begin-frame window-width win-height px-ratio)
		      (rotate 0.05)
		      (stroke-color 255 0 0 255)
		      (stroke-width line-width)
		      (begin-path)
		      (do ((x 0 (+ x step)))
			  ((>= x width))
			(move-to x 0)
			(line-to (- width x) width))
		      (do ((y 0 (+ y step)))
			  ((>= y height))
			(move-to 0 y)
			(line-to height (- height y)))
		      (stroke)
		      (end-frame)
		      (swap-buffers)
		      (poll-events))
		    (loop (+ t 1)))))))))))