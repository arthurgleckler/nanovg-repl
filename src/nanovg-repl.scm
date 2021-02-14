;;;; NanoVG-REPL from Scheme

;;;; Copyright MMXXI Arthur A. Gleckler.

;;;; See MIT license in "LICENSE" file.

(cond-expand
 (chibi
  (define (assert boolean . error-arguments)
    (when (not boolean) (apply error error-arguments)))
  (define unspecific 'unspecific))
 (mit
  (declare (usual-integrations))
  (load-option 'synchronous-subprocess)))

(cond-expand
 (chibi
  (define (make-eq-hash-table)
    (make-hash-table (make-eq-comparator))))
 (mit #f))				; This works around a bug in MIT Scheme.

;;; Window

(define-record-type nanovg-window
    (%make-nanovg-window events fifo pid stderr stdin stdout)
    nanovg-window?
  (events nvgw/events)
  (fifo nvgw/fifo)
  (pid nvgw/pid)
  (stderr nvgw/stderr)
  (stdin nvgw/stdin)
  (stdout nvgw/stdout))

(define current-nanovg-window (make-parameter #f))

;;; Write arguments.  Read results.

(define (write-nanovg-boolean boolean)
  (write-char #\space)
  (write (if boolean 1 0)))

(define (write-nanovg-byte byte)
  (assert (and (exact-integer? byte) (<= 0 byte 255)))
  (write-char #\space)
  (write byte))

(define (write-nanovg-float float)
  (assert (real? float))
  (write-char #\space)
  (write (if (integer? float)
	     float
	     (inexact float))))

(define (write-nanovg-int int)
  (assert (exact-integer? int))
  (write-char #\space)
  (write int))

(define (write-nanovg-string string)
  (assert string? string)
  (write-char #\space)
  (let ((bytes (bytevector-length (string->utf8 string))))
    (write bytes)
    (write-char #\space)
    (write-string string)))

(define (read-and-assert expected-type? type-name)
  (lambda ()
    (let ((result (read)))
      (assert (expected-type? result)
	      "Unexpected type."
	      result
	      type-name)
      result)))

(define (read-boolean)
  (let ((result (read)))
    (assert (or (= result 0) (= result 1))
	    "Must be 0 or 1."
	    result)
    (= result 1)))
(define read-float (read-and-assert real? "float"))
(define (read-float-vector size)
  (let ((result (make-vector size)))
    (do ((i 0 (+ i 1)))
	((= i size))
      (let ((element (read)))
	(assert (real? element) "Expected vector of floats." element)
	(vector-set! result i element)))
    result))
(define read-int (read-and-assert exact-integer? "int"))

(define-syntax left-to-right
  (syntax-rules ()
    ((_ wrap () (element ...)) (wrap element ...))
    ((_ wrap (x . rest) (element ...))
     (let ((y x)) (left-to-right wrap rest (element ... y))))))

(define (skip-whitespace)
  (let next-character ()
    (let ((character (peek-char)))
      (when (and (not (eof-object? character))
		 (case character
		   ((#\n #\r #\space #\t) #t)
		   (else #f)))
	(read-char)
	(next-character)))))

(define-syntax read-values
  (syntax-rules (boolean float float-vector int)
    ((_ () (reader ...))
     (begin (skip-whitespace)
	    (left-to-right values (reader ...) ())))
    ((_ (boolean . rest) (reader ...))
     (read-values rest (reader ... (read-boolean))))
    ((_ ((float-vector size) . rest) (reader ...))
     (read-values rest (reader ... (read-float-vector size))))
    ((_ (float . rest) (reader ...))
     (read-values rest (reader ... (read-float))))
    ((_ (int . rest) (reader ...))
     (read-values rest (reader ... (read-int))))))

(define-syntax write-values
  (syntax-rules (boolean byte float int string)
    ((_ () (writer ...) (argument ...)) (begin writer ... (newline)))
    ((_ ((boolean arg) . rest) (writer ...) (argument ...))
     (write-values rest
		   (writer ... (write-nanovg-boolean arg))
		   (argument ... arg)))
    ((_ ((byte arg) . rest) (writer ...) (argument ...))
     (write-values rest
		   (writer ... (write-nanovg-byte arg))
		   (argument ... arg)))
    ((_ ((float arg) . rest) (writer ...) (argument ...))
     (write-values rest
		   (writer ... (write-nanovg-float arg))
		   (argument ... arg)))
    ((_ ((int arg) . rest) (writer ...) (argument ...))
     (write-values rest
		   (writer ... (write-nanovg-int arg))
		   (argument ... arg)))
    ((_ ((string arg) . rest) (writer ...) (argument ...))
     (write-values rest
		   (writer ... (write-nanovg-string arg))
		   (argument ... arg)))))

(define-syntax define-command*
  (syntax-rules ()
    ((_ (name (arg-type argument) ...) body ...)
     (define (name argument ...)
       (parameterize ((current-output-port
		       (nvgw/stdout (current-nanovg-window))))
	 (write 'name)
	 (write-values ((arg-type argument) ...) () ()))
       body ...))))

(define-syntax define-command
  (syntax-rules (returns)
    ((_ (name (arg-type argument) ...))
     (define-command* (name (arg-type argument) ...)))
    ((_ (name (arg-type argument) ...)
	(returns (return-type return-name) ...))
     (define-command* (name (arg-type argument) ...)
       (let ((window (current-nanovg-window)))
	 (flush-commands)
	 (parameterize ((current-input-port (nvgw/stdin window)))
	   (read-values (return-type ...) ())))))))

(define (flush-commands)
  (flush-output-port (nvgw/stdout (current-nanovg-window))))

;;; Read and handle events.

(define event-reader-table (make-eq-hash-table))

(define-syntax define-event
  (syntax-rules ()
    ((_ (name (type argument) ...))
     (hash-table-set! event-reader-table
		      'name
		      (lambda ()
			(call-with-values
			    (lambda () (read-values (type ...) ()))
			  list))))))

(define (read-nanovg-event)
  (parameterize ((current-input-port (nvgw/events (current-nanovg-window))))
    (if (char-ready?)
	(let ((event-name (read)))
	  (assert (symbol? event-name) "Event name must be a symbol." event-name)
	  (let ((arguments ((hash-table-ref event-reader-table event-name))))
	    (read-char)
	    (values event-name arguments)))
	(values #f #f))))

(define-syntax dispatch-clauses
  (syntax-rules (else)
    ((_ arguments (no-events-body ...) (case-clause ...) ())
     (let-values (((event-name arguments) (read-nanovg-event)))
       (case event-name
	 ((#f) unspecific no-events-body ...)
	 case-clause ...)))
    ((_ arguments
	(no-events-body ...)
	(case-clause ...)
	((else clause-body ...)))
     (dispatch-clauses arguments
		       (no-events-body ...)
		       (case-clause ... (else unspecific clause-body ...))
		       ()))
    ((_ arguments
	(no-events-body ...)
	(case-clause ...)
	(((name arg ...) clause-body ...) . rest))
     (dispatch-clauses arguments
		       (no-events-body ...)
		       (case-clause
			...
			((name)
			 (apply (lambda (arg ...) clause-body ...) arguments)))
		       rest))))

(define-syntax dispatch-event
  (syntax-rules (no-events)
    ((_ (no-events body ...) clause ...)
     (dispatch-clauses arguments (body ...) () (clause ...)))
    ((_ clause ...)
     (dispatch-clauses arguments () () (clause ...)))))

;;; Commands

(define-command (add-fallback-font (string base-font) (string fallback-font)))
(define-command (add-fallback-font-id (int base-font) (int fallback-font)))
(define-command (arc (float cx)
		     (float cy)
		     (float r)
		     (float a0)
		     (float a1)
		     (int dir)))
(define-command (arc-to (float x1)
			(float y1)
			(float x2)
			(float y2)
			(float radius)))
(define-command (begin-frame (float window-width)
			     (float window-height)
			     (float device-pixel-ratio)))
(define-command (begin-path))
(define-command (bezier-to (float c1x)
			   (float c1y)
			   (float c2x)
			   (float c2y)
			   (float x)
			   (float y)))
(define-command (box-gradient (float x)
			      (float y)
			      (float w)
			      (float h)
			      (float r)
			      (float f)
			      (byte icolr)
			      (byte icolg)
			      (byte icolb)
			      (byte icola)
			      (byte ocolr)
			      (byte ocolg)
			      (byte ocolb)
			      (byte ocola))
  (returns (int paint-id)))
(define-command (circle (float cx) (float cy) (float r)))
(define-command (clear (byte color-buffer-bit)
		       (byte depth-buffer-bit)
		       (byte stencil-buffer-bit)))
(define-command (clear-color (float r) (float g) (float b) (float a)))
(define-command (close-path))
(define-command (close-window))
(define-command (create-font (string name) (string filename))
  (returns (int font-id)))
(define-command (create-font-at-index (string name)
				      (string filename)
				      (int index))
  (returns (int font-id)))
(define-command (create-image (string filename) (int image-flags))
  (returns (int image-id)))
(define-command (current-transform)
  (returns ((float-vector 6) transform)))
(define-command (delete-image (int image)))
(define-command (ellipse (float cx) (float cy) (float rx) (float ry)))
(define-command (end-frame))
(define-command (fill))
(define-command (fill-color (byte r) (byte g) (byte b) (byte a)))
(define-command (fill-paint (int paint-id)))
(define-command (find-font (string name))
  (returns (int font-id)))
(define-command (font-blur (float blur)))
(define-command (font-face (string font)))
(define-command (font-face-id (int font)))
(define-command (font-size (float size)))
(define-command (frame-buffer-size)
  (returns (int fb-width) (int fb-height)))
(define-command (global-alpha (float alpha)))
(define-command (image-pattern (float cx)
			       (float cy)
			       (float w)
			       (float h)
			       (float angle)
			       (int image)
			       (float alpha))
  (returns (int paint-id)))
(define-command (image-size (int image))
  (returns (int w) (int h)))
(define-command (intersect-scissor (float x) (float y) (float w) (float h)))
(define-command (key-input-events (boolean on)))
(define-command (linear-gradient (float sx)
				 (float sy)
				 (float ex)
				 (float ey)
				 (byte icolr)
				 (byte icolg)
				 (byte icolb)
				 (byte icola)
				 (byte ocolr)
				 (byte ocolg)
				 (byte ocolb)
				 (byte ocola))
  (returns (int paint-id)))
(define-command (line-cap (int cap)))
(define-command (line-join (int join)))
(define-command (line-to (float x) (float y)))
(define-command (miter-limit (float limit)))
(define-command (mouse-button-events (boolean on)))
(define-command (mouse-position-events (boolean on)))
(define-command (move-to (float x) (float y)))
(define-command (path-winding (int dir)))
(define-command (ping (string s)))
(define-command (poll-events))
(define-command (quad-to (float cx) (float cy) (float x) (float y)))
(define-command (radial-gradient (float cx)
				 (float cy)
				 (float inr)
				 (float outr)
				 (byte icolr)
				 (byte icolg)
				 (byte icolb)
				 (byte icola)
				 (byte ocolr)
				 (byte ocolg)
				 (byte ocolb)
				 (byte ocola))
  (returns (int paint-id)))
(define-command (rect (float x) (float y) (float w) (float h)))
(define-command (reset))
(define-command (reset-fallback-fonts (string base-font)))
(define-command (reset-fallback-fonts-id (int base-font)))
(define-command (reset-scissor))
(define-command (reset-transform))
(define-command (restore))
(define-command (rotate (float angle)))
(define-command (rounded-rect (float x)
			      (float y)
			      (float w)
			      (float h)
			      (float r)))
(define-command (rounded-rect-varying (float x)
				      (float y)
				      (float w)
				      (float h)
				      (float rad-top-left)
				      (float rad-top-right)
				      (float rad-bottom-right)
				      (float rad-bottom-left)))
(define-command (save))
(define-command (scale (float x) (float y)))
(define-command (scissor (float x) (float y) (float w) (float h)))
(define-command (shape-anti-alias (int enabled)))
(define-command (shutdown))
(define-command (skew-x (float angle)))
(define-command (skew-y (float angle)))
(define-command (stroke))
(define-command (stroke-color (byte r) (byte g) (byte b) (byte a)))
(define-command (stroke-paint (int paint-id)))
(define-command (stroke-width (float width)))
(define-command (swap-buffers))
(define-command (text (float x) (float y) (string s))
  (returns (float result)))
(define-command (text-align (int align)))
(define-command (text-bounds (float x) (float y) (string s))
  (returns (float result) ((float-vector 4) bounds)))
(define-command (text-box (float x)
			  (float y)
			  (float break-row-width)
			  (string s)))
(define-command (text-box-bounds (float x)
				 (float y)
				 (float break-row-width)
				 (string s))
  (returns ((float-vector 4) bounds)))
(define-command (text-input-events (boolean on)))
(define-command (text-letter-spacing (float spacing)))
(define-command (text-line-height (float line-height)))
(define-command (text-metrics)
  (returns (float ascender) (float descender) (float lineh)))
(define-command (transform (float a)
			   (float b)
			   (float c)
			   (float d)
			   (float e)
			   (float f)))
(define-command (translate (float x)
			   (float y)))
(define-command (unregister (int id)))
(define-command (viewport (int x) (int y) (int w) (int h)))
(define-command (window-size)
  (returns (int window-width) (int win-height)))
(define-command (window-should-close?)
  (returns (boolean should-close?)))
(define-command (window-size-change-events (boolean on)))

;;; Events

(define-event (key-input (int key) (int code) (int mods)))
(define-event (mouse-button (int button) (int action) (int mods)))
(define-event (mouse-position (float xpos) (float ypos)))
(define-event (text-input (int code-point)))
(define-event (window-size-changed (int width) (int height)))

;;; Cleanup

(define (delete-nanovg-fifo)
  (delete-file (nvgw/fifo (current-nanovg-window))))

(define (nanovg-cleanup thunk)
  (let ((cleaned-up? #f))
    (dynamic-wind
	(lambda () (when cleaned-up? (error "Can't resume.")))
	thunk
	(lambda ()
	  (flush-commands)
	  (delete-nanovg-fifo)
	  (set! cleaned-up? #t)))))

;;; Subprocess

(cond-expand
 (chibi
  (define (make-fifo)
    (let ((pathname (call-with-temp-file "events"
					 (lambda (path out preserve) path))))
      (assert
       (system? "mkfifo" "-m" "600" pathname)
       "Failed to make FIFO (named pipe).")
      pathname)))
 (mit
  (define (make-fifo)
    (let ((pathname (call-with-temporary-file-pathname (lambda (x) x))))
      (assert
       (zero?
	(run-synchronous-subprocess "mkfifo"
				    `("-m" "600" ,(enough-namestring pathname))))
       "Failed to make FIFO (named pipe).")
      pathname))))

(cond-expand
 (chibi
  (define (make-nanovg-window repl-pathname width height title)
    (let ((fifo (make-fifo)))
      (call-with-process-io
       (list repl-pathname
	     (number->string width)
	     (number->string height)
	     title
	     fifo)
       (lambda (pid stdout stdin stderr)
	 (%make-nanovg-window (open-input-file fifo)
			      fifo
			      pid
			      stderr
			      stdin
			      stdout))))))
 (mit
  (define (make-nanovg-window repl-pathname width height title)
    ;; <> Fix: MIT Scheme's `start-pipe-subprocess' sends both the
    ;; subprocesses's `stdout' and its `stderr' to the same place.
    (let* ((fifo (make-fifo))
	   (process (start-pipe-subprocess
		     repl-pathname
		     (vector "repl"
			     (number->string width)
			     (number->string height)
			     title
			     (enough-namestring fifo))
		     scheme-subprocess-environment)))
      (%make-nanovg-window (open-input-file fifo)
			   fifo
			   (subprocess-id process)
			   (subprocess-output-port process)
			   (subprocess-input-port process)
			   (subprocess-output-port process))))))