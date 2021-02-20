;;;; NanoVG-REPL from Scheme: library declaration

;;;; Copyright MMXXI Arthur A. Gleckler.

;;;; See MIT license in "LICENSE" file.

(define-library (nanovg-repl)
  (cond-expand
   (chibi
    (import (chibi process))
    (import (chibi temp-file)))
   (mit
    (import (only (mit legacy runtime)
                  assert
                  call-with-temporary-file-pathname
                  enough-namestring
                  identifier?
                  load-option
                  run-synchronous-subprocess
                  scheme-subprocess-environment
                  start-pipe-subprocess
                  subprocess-id
                  subprocess-input-port
                  subprocess-output-port
                  unspecific))))
  (import (scheme base))
  (import (scheme file))
  (import (scheme read))
  (import (scheme write))
  (import (srfi 125))
  (import (srfi 128))
  (export delete-nanovg-fifo
	  dispatch-event
	  make-nanovg-window
	  nanovg-cleanup
	  read-nanovg-event
	  current-nanovg-window

	  ;; commands
	  add-fallback-font
	  add-fallback-font-id
	  arc
	  arc-to
	  begin-frame
	  begin-path
	  bezier-to
	  box-gradient
	  circle
	  clear
	  clear-color
	  close-path
	  close-window
	  create-font
	  create-font-at-index
	  create-image
	  current-transform
	  delete-image
	  ellipse
	  end-frame
	  fill
	  fill-color
	  fill-paint
	  find-font
	  font-blur
	  font-face
	  font-face-id
	  font-size
	  frame-buffer-size
	  global-alpha
	  image-pattern
	  image-size
	  intersect-scissor
	  key-input-events
	  linear-gradient
	  line-cap
	  line-join
	  line-to
	  miter-limit
	  mouse-button-events
	  mouse-position-events
	  move-to
	  path-winding
	  ping
	  poll-events
	  quad-to
	  radial-gradient
	  rect
	  reset
	  reset-fallback-fonts
	  reset-fallback-fonts-id
	  reset-scissor
	  reset-transform
	  restore
	  rotate
	  rounded-rect
	  rounded-rect-varying
	  save
	  scale
	  scissor
	  shape-anti-alias
	  shutdown
	  skew-x
	  skew-y
	  stroke
	  stroke-color
	  stroke-paint
	  stroke-width
	  swap-buffers
	  text
	  text-align
	  text-bounds
	  text-box
	  text-box-bounds
	  text-input-events
	  text-letter-spacing
	  text-line-height
	  text-metrics
	  transform
	  translate
	  unregister
	  viewport
	  window-size
	  window-should-close?
	  window-size-change-events)
  (include "nanovg-repl.scm"))