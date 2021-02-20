;;;; Stomachion demo in Scheme

;;;; Copyright MMXXI Arthur A. Gleckler.

;;;; See MIT license in "LICENSE" file.

(cond-expand
 (mit (declare (usual-integrations))))

(define (line-segments x0 y0 x1 y1 . rest)
  (move-to x0 y0)
  (let loop ((x x1)
	     (y y1)
	     (rest rest))
    (line-to x y)
    (unless (null? rest)
      (loop (car rest) (cadr rest) (cddr rest)))))

(define (polygon x0 y0 x1 y1 . rest)
  (apply line-segments x0 y0 x1 y1 rest)
  (line-to x0 y0))

(define (filled-polygon x0 y0 x1 y1 . rest)
  (begin-path)
  (apply polygon x0 y0 x1 y1 rest)
  (fill))

(define (process-events width height time)
  (let loop ((w width)
	     (h height)
	     (t time))
    (dispatch-event
     (no-events (values w h t))
     ((mouse-button button action mods)
      (mouse-position-events (= action 1))
      (loop w h t))
     ((mouse-position xpos ypos) (loop w h ypos))
     ((window-size-changed w h) (loop w w t))
     (else (loop w h t)))))

(define (stomachion-outline)
  (stroke-color 255 255 255 255)
  (stroke-width 0.02)
  (begin-path)
  (line-segments 0 0 12 0 12 12 0 12 0 0 12 12 6 0 0 12 2 2)
  (line-segments 3 6 3 12 2 8)
  (line-segments 6 0 6 12 9 6 12 4)
  (line-segments 9 6 12 6)
  (stroke))

(define (stomachion-polygons)
  (let ((gradient-1
	 (radial-gradient 6 6 0 6 1 0 0 255 255 0 0 200))
	(gradient-2
	 (radial-gradient 6 6 0 6 0 1 0 255 0 255 0 200))
	(gradient-3
	 (radial-gradient 6 6 0 6 0 0 1 255 0 0 255 200)))
    (fill-paint gradient-1)
    (filled-polygon 0 0 6 0 4 4)
    (fill-paint gradient-2)
    (filled-polygon 0 0 2 2 0 12)
    (fill-paint gradient-3)
    (filled-polygon 2 2 4 4 0 12)
    (unregister gradient-1)
    (unregister gradient-2)
    (unregister gradient-3))
  (fill-color 127 0 0 200)
  (filled-polygon 0 12 3 12 2 8)
  (fill-color 0 127 0 200)
  (filled-polygon 3 6 3 12 2 8)
  (fill-color 0 0 127 200)
  (filled-polygon 3 6 4 4 6 6 6 12 3 12)
  (fill-color 63 0 0 200)
  (filled-polygon 6 0 6 6 4 4)
  (fill-color 0 63 0 200)
  (filled-polygon 6 0 9 6 8 8 6 6)
  (fill-color 0 0 63 200)
  (filled-polygon 6 6 8 8 6 12)
  (fill-color 192 0 0 200)
  (filled-polygon 9 6 12 12 8 8)
  (fill-color 0 192 0 200)
  (filled-polygon 8 8 12 12 6 12)
  (fill-color 0 0 192 200)
  (filled-polygon 6 0 12 0 12 4 9 6)
  (fill-color 255 0 0 200)
  (filled-polygon 9 6 12 4 12 6)
  (fill-color 0 255 0 200)
  (filled-polygon 9 6 12 12 8 8)
  (fill-color 0 0 255 200)
  (filled-polygon 9 6 12 6 12 12))

(define (stomachion repl-root)
  (let ((repl-pathname (path-resolve "build/repl" repl-root))
	(width 800)
	(height 800))
    (parameterize ((current-nanovg-window
		    (make-nanovg-window repl-pathname
					width
					height
					"Stomachion")))
      (nanovg-cleanup
       (lambda ()
	 (create-font
	  "subset"
	  (path-resolve "examples/subset-GFSNeohellenic-Bold.ttf" repl-root))
	 (mouse-button-events #t)
	 (window-size-change-events #t)
	 (let loop ((w width)
		    (h height)
		    (t 0))
	   (cond ((window-should-close?)
		  (shutdown))
		 (else
		  (let-values (((w h t) (process-events w h t))
			       ((fb-width fb-height) (frame-buffer-size))
			       ((window-width window-height) (window-size)))
		    (let ((px-ratio (/ fb-width window-width)))
		      (viewport 0 0 fb-width fb-height)
		      (clear-color 0 0 0 1)
		      (clear 1 1 1)
		      (begin-frame window-width window-height px-ratio)
		      (font-face "subset")
		      (font-size 50)
		      (text 10 50 "Ὀστομάχιον")
		      (translate (/ w 2) (/ h 2))
		      (rotate (* t 0.01))
		      (translate (- (/ w 2)) (- (/ h 2)))
		      (scale (/ w 12) (/ h 12))
		      (stomachion-polygons)
		      (stomachion-outline)
		      (end-frame)
		      (swap-buffers)
		      (poll-events))
		    (loop w h (+ t 1)))))))))))