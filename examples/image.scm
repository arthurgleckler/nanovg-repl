;;;; Image demo in Scheme

;;;; Copyright MMXXI Arthur A. Gleckler.

;;;; See MIT license in "LICENSE" file.

(cond-expand
 (mit (declare (usual-integrations))))

(define (image-demo repl-root)
  (let* ((repl-pathname (path-resolve "build/repl" repl-root))
	 (width 800)
	 (height width))
    (parameterize ((current-nanovg-window
		    (make-nanovg-window repl-pathname 800 800 "image")))
      (nanovg-cleanup
       (lambda ()
	 (let ((image (create-image
		       (path-resolve "examples/balisp.png" repl-root)
		       6)))
	   (let loop ((t 0))
	     (cond ((window-should-close?)
		    (shutdown))
		   (else
		    (let-values (((fb-width fb-height) (frame-buffer-size))
				 ((window-width win-height) (window-size)))
		      (let ((angle (* t 0.0042))
			    (px-ratio (/ fb-width window-width)))
			(viewport 0 0 fb-width fb-height)
			(clear-color 0 0 0 1)
			(clear 1 1 1)
			(begin-frame window-width win-height px-ratio)
			(translate (/ width 2) (/ height 2))
			(rotate angle)
			(translate (- (/ width 2)) (- (/ height 2)))
			(scale 12 12)
			(let ((image-paint
			       (image-pattern 0 0 79.8 60 0 image 1)))
			  (fill-paint image-paint)
			  (begin-path)
			  (rounded-rect 0 0 width height 5)
			  (fill-paint image-paint)
			  (fill)
			  (unregister image-paint))
			(end-frame)
			(swap-buffers)
			(poll-events)))
		    (loop (+ t 1)))))))))))