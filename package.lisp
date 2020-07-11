;;;; package.lisp

(defpackage #:jamais-vu
  (:use #:common-lisp
	#:sc)
  (:export #:server-start
	   
	   #:name
	   #:dur
	   #:number-of-sub-bufs
	   #:recording-p
	   #:playing-p
	   #:looping-p
	   #:play-repetitions
	   #:inter-onset-timings
	   #:absolute-onset-timings
	   #:loop-start
	   #:*loopers*
	   #:find-looper-by-name
	   #:default-looper
	   #:buffer-contents

	   #:osc-responder

	   #:start-recording
	   #:stop-recording

	   #:start-playing
	   #:start-playing-random-start
	   #:start-looping
	   #:stop-looping
	   #:stop-playing

	   #:buf-random-sign-delta
	   #:insert-sines

	   #:t-grains
	   #:stop-t-grains
	   #:toggle-t-grains

	   #:start-poeira
	   #:stop-poeira
	   #:toggle-poeira))

(defpackage #:jamais-vu.gui
  (:use #:jamais-vu
	#:cl+qt)
  (:export #:main
	   #:*window*))
