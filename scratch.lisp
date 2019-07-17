(in-package :jamais-vu)

(start-recording :loop 0)
(start-recording :loop 1)

(stop-recording)

(start-playing *buf* (now) (buffer-dur *buf*) 1 0)

(stop-playing)

(halve-buffer)

(proxy :grains
       (let* ((t-rate (mouse-y.kr 8 120 :exp))
	      (dur (/ 12 t-rate))
	      (clk (impulse.kr t-rate))
	      (pos (+ (mouse-x.kr 0 (buf-dur.kr *buf*))
		      (t-rand.kr 0 0.01 clk)))
	      (pan (white-noise.kr 0.6)))
	 (tgrains.ar 2 clk *buf* 1 pos dur pan 0.1)))

(free :grains)

(stop)


(populate-sub-buffers)
(play-sub-buffers :order (loop :for i :from 0 :upto 60 :collect (list 2 :rate (* 8 (sin i)) :dur (random 1.0) :amp 0.15)) :offset -0.5)

;;;;;

(in-package :sc-user)

(proxy :sine-synth
       (with-controls ((freq 440) (amp 0.7) (attack 0.01) (release 0.8))
	 (* (sin-osc.ar (list freq (* freq 1.02)) 0 amp)
	    (env-gen.ar (perc attack release) :act :free)))
       :fade 2)

(in-package :cl-patterns)

(pb :sine
  :instrument :sine-synth
  :freq 320
  :pfindur 1
  :quant 1
  :dur 4/3
  :release 0.5)

(play :sine)
(stop :sine)

(pb :sine2
  :instrument :sine-synth
  :freq (pseq '(200 300 400 550))
  :pfindur 4
  :quant 1
  :dur 6/7
  :release 0.3
  :amp (prand '(0.8 0.3 0.4)))

(play :sine2)
(stop :sine2)


(clock-clear-tasks)

;;;;

(in-package :sc-user)

(defparameter *b* (buffer-read (merge-pathnames #p"sounds/a11wlk01.wav" *sc-synth-program*)))

(defsynth concat1 (bufnum (match-length 0.01) (freeze 0))
  (let ((control (sound-in.ar))
	(input (play-buf.ar 1 bufnum (buf-rate-scale.kr bufnum) :loop 1))
	(concat (concat.ar )))))
