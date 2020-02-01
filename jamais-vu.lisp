;;;; jamais-vu.lisp

(in-package #:jamais-vu)

;;; Constants for the message id sent by the audio server on trigger events
(defconstant +rec-done+ 0)
(defconstant +onset+ 1)
(defconstant +play-done+ 2)
(defconstant +peak+ 3)

(defparameter *server-sample-rate* 44100
  "The sample rate of the audio server.")
(defparameter *default-buffer-duration* 8.0
  "Default buffer duration in seconds.")
(defparameter *default-number-of-sub-bufs* 8
  "Default number of sub-buffers.")
(defparameter *default-input* 1
  "Default recording input in the audio interface.")

;; Uncomment and eval to control synths with the mouse.
;; Otherwise use CTRL or the GUI
;(pushnew :mouse *features*)

(defvar *external-osc-port* 8000
  "OSC port of the external controller device.")
(defvar *osc-thread*
  "To be bind to the listener thread for the external OSC controller device.")
(defvar *osc-socket*
  "To be bind to the socket for the external OSC controller device.")
(defvar *osc-on* t
  "Setting to nil signals that the listener thread should end and the socket for the external OSC controller device should be closed.")

(defun server-start ()
  "Defines a SuperCollider server and boots it."
  (setf *s* (make-external-server "localhost"
				  :server-options (make-server-options :block-size 16
								       :hardware-samplerate 44100
								       :device "ASIO : ASIO Fireface")
				  :port 4444))
  (server-boot *s*))

;;; Boot the server if it's not running already
(unless (and *s* (sc::boot-p *s*)) (server-start))

(defparameter *looper-last-id* 0
  "Last id number that was attributed to a looper.")

(defclass looper ()
  ((id
    :accessor id
    :initarg :id
    :initform (incf *looper-last-id*)
    :documentation "Each looper gets an exclusive numerical identifier.")
   (name
    :accessor name
    :initarg :name
    :initform "Unnamed looper"
    :documentation "The looper's name.")
   (dur
    :accessor dur
    :initarg :dur
    :initform *default-buffer-duration*
    :documentation "Duration in seconds.")
   (number-of-sub-bufs
    :accessor number-of-sub-bufs
    :initarg :number-of-sub-bufs
    :initform *default-number-of-sub-bufs*
    :documentation "Number of sub-buffers to divide the main buffer.")
   (recording-p
    :accessor recording-p
    :initform nil
    :documentation "Is the looper currently recording?")
   (playing-p
    :accessor playing-p
    :initform nil
    :documentation "Is the looper currently playing?")
   (looping-p
    :accessor looping-p
    :initform nil
    :documentation "Is the looper currently looping?")
   (play-repetitions
    :accessor play-repetitions
    :initform 1
    :documentation "How many times will playback repeat.")
   (inter-onset-timings
    :accessor inter-onset-timings
    :initform '()
    :documentation "A list of the time in seconds between each onset, detected while recording.")
   (absolute-onset-timings
    :accessor absolute-onset-timings
    :initform '()
    :documentation "A list of the time in seconds for each onset.")
   (loop-start
    :accessor loop-start
    :initform 0
    :documentation "A time point in seconds where the loop starts.")
   (recorder-node
    :accessor recorder-node
    :documentation "The node of the active recorder synth.")
   (player-nodes
    :accessor player-nodes
    :initform nil
    :documentation "A list of the nodes for the active player synths.")
   (buffer
    :accessor buffer
    :documentation "A buffer object, representing audio in the server.")
   (sub-bufs
    :accessor sub-bufs
    :documentation "A list of buffers, resulting from the partitioning of the main buffer.")
   (peak
    :accessor peak
    :initform 0
    :documentation "Peak amplitude of the recorded signal."))
  (:documentation "A self-contained abstraction for the recording and playing of an audio sample, stored in the server. Despite the name, it may actually loop or not."))

(defmethod initialize-instance :after ((new-looper looper) &key)
  "Allocate the necessary audio buffers in the server."
  (with-accessors ((buffer-dur dur) (sub-bufs-n number-of-sub-bufs))
      new-looper
    (setf (buffer new-looper) (buffer-alloc (* *server-sample-rate*
					       buffer-dur)
					    :chanls 1)
	  (sub-bufs new-looper) (loop
				  :repeat sub-bufs-n
				  :collect (buffer-alloc (* *server-sample-rate*
							    (/ buffer-dur
							       sub-bufs-n))
							 :chanls 1)))))

(defun init-loopers ()
  "Return a list which contains only the default looper."
  (list (make-instance 'looper :name "Default")))

(defparameter *loopers* (init-loopers)
  "Global list of active loopers.")

(defun find-looper-by-name (name &optional (loopers *loopers*))
  "Return the looper called NAME."
  (find name loopers :key #'name :test 'string-equal))

(defun default-looper ()
  "Return the default looper."
  (find-looper-by-name "Default"))

(defun buffer-contents (&optional (looper (default-looper)))
  "Return a list with the audio frames on the buffer of LOOPER."
  (buffer-load-to-list (buffer looper)))

(defun load-sound-file (path &optional (looper (default-looper)))
  "Load file in PATH into LOOPER."
  (setf (buffer looper)
	(buffer-read-channel path :channels 0
				  :bufnum (bufnum (buffer looper)))))


;;; Synth definitions

(defsynth record-buf ((in 0) bufn (run 1) (stop 0) (loop 0))
  (let* ((sound-in (sound-in.ar in))
	 (rec (record-buf.ar sound-in bufn :run run :loop loop :act :free))
	 (peak (peak.ar sound-in 1))
	 (timer-stop (timer.kr stop))
	 (timer-done (timer.kr (done.kr rec)))
	 (onset-detected (coyote.kr sound-in))
	 (onset-timing (timer.kr onset-detected)))
    (send-trig.kr onset-detected 1 onset-timing)
    (send-trig.kr stop 0 timer-stop)
    (send-trig.kr (done.kr rec) 0 timer-done)
    (send-trig.kr stop 3 peak)
    (send-trig.kr (done.kr rec) 3 peak)
    (free-self.kr stop)))

(defsynth play-buf ((out 0) bufn dur (window 0.1) (rate 1.0) (start-pos 0.0)
		    (amp 1.0) (gate 1))
  (let ((sound (play-buf.ar 1 bufn rate :start-pos start-pos :loop 1))
	(envelope (env-gen.kr (env '(0 1 1 0)
				   (list (/ window 2) (- dur window) (/ window 2))
				   '(:lin :hold :lin)) :act :free))
	(cutoff (env-gen.kr (cutoff 0.1) :gate gate :act :free)))
    (send-trig.kr (done.kr envelope) +play-done+)
    (send-trig.kr (done.kr cutoff) +play-done+)
    (out.ar out (pan2.ar (* sound envelope cutoff amp)))))

(defsynth poeira ((out 0)
		  bufn
		  (density 5)
		  (attack 0.01)
		  (release 0.01)
		  (amp 1.0))
  (let* ((frames (/ (buf-frames.kr bufn) 3))
	 (trig (trig-1.ar (lf-noise0.ar density) 0.01))
	 (sound (play-buf.ar 1 bufn 1.0
			     :trig trig
			     :start-pos (ti-rand.kr 0 frames trig)))
	 (envelope (env-gen.kr (perc attack release) :gate trig)))
    (out.ar out (* sound envelope amp))))

;;; https://en.wikibooks.org/wiki/Designing_Sound_in_SuperCollider/Schroeder_reverb
(defsynth schroeder-reverb ()
  (let* ((input (in.ar)
	   ;; (dust2.ar 2 0.1)
	   )
	 (delrd (local-in.ar 4))
	 (output (+ input (subseq delrd 0 2)))
	 (sig (list (+ (first output) (second output))
		    (- (first output) (second output))
		    (+ (third delrd) (fourth delrd))
		    (- (third delrd) (fourth delrd))))
	 (sig (list (+ (first sig) (third sig))
		    (+ (second sig) (fourth sig))
		    (- (first sig) (third sig))
		    (- (second sig) (fourth sig))))
	 (sig (* sig '(0.4 0.37 0.333 0.3)))
	 (deltimes (- (mapcar (lambda (x) (* x 0.001))
			      '(101 143 165 177))
		      (control-dur.ir))))
    (local-out.ar (delay-c.ar sig deltimes deltimes))
    (out.ar '(0 1) output)))


;;; Global volume

;; TODO This only controls the mix for the live signal
(proxy :volume
       (with-controls ((amp 0))
	 (pan2.ar (in.ar 2) 0.0 amp))
       :pos :tail)

;; To hear the live signal:
;; (ctrl :volume :amp 1)


;;; OSC message responder

(defun inter-onset->absolute (iot)
  (loop :for v :in iot
	:sum v :into s
	:collect s))

(defun find-recording-looper-by-node-id (node-id)
  "Given the id value of a recording node on the server, return the looper object that is using that node."
  (flet ((rec-node-id (looper)
	   (sc::id (recorder-node looper))))
    (find node-id *loopers* :key #'rec-node-id)))

(defun find-playing-looper-by-node-id (node-id)
  "Given the id value of a playing node on the server, return the looper object that is using that node."
  (flet ((play-node-id (looper)
	   (mapcar #'sc::id (player-nodes looper))))
    (find node-id *loopers* :key #'play-node-id :test #'find)))

(defun osc-responder (node id value &optional (qobject nil))
  (case id
    ;; Recording stopped
    (#.+rec-done+
     (when qobject (cl+qt:signal! qobject (jamais-vu.gui::rec-stop jamais-vu.gui::float) value))
     (format t "~&Rec stop. Dur: ~a~%" value)
     (let ((looper (find-recording-looper-by-node-id node)))
       (with-accessors ((buffer buffer) (dur dur) (loop-start loop-start)
			(absolute-onset-timings absolute-onset-timings)
			(inter-onset-timings inter-onset-timings)
			(recording-p recording-p))
	   looper
	 (setf (frames buffer) (- (truncate (* (max value dur)
					       *server-sample-rate*))
				  32 ; TODO check why; block size?
				  )
	       loop-start (* (mod value dur) *server-sample-rate*)
	       absolute-onset-timings (inter-onset->absolute
				       (reverse inter-onset-timings))
	       inter-onset-timings '()
	       dur value
	       recording-p nil))))

    ;; Onset detected
    (#.+onset+
     (let ((looper (find-recording-looper-by-node-id node)))
       (push value (inter-onset-timings looper))
       (format t "~&Onset: ~a~%" value)))

    ;; Playing stopped
    (#.+play-done+
     (let* ((looper (find-playing-looper-by-node-id node))
	    (nodes (player-nodes looper)))
       (setf (player-nodes looper) (remove (find node nodes :key #'sc::id)
					   nodes))))

    ;; Peak
    (#.+peak+
     (let ((looper (find-recording-looper-by-node-id node)))
       (setf (peak looper) value)
       (format t "~&Peak: ~a~%" value))
     ;; (setf (peak (find-playing-looper-by-node-id node)) value)
     )

    (otherwise
     (format t "~&Unable to handle OSC message with node ~a, id ~a, and value ~a.~%"
	     node id value))))

(add-reply-responder "/tr" #'osc-responder)


;;; Recording

(defun start-recording (&key (looper (default-looper)) (in *default-input*) (loop 0))
  (unless (recording-p looper)
    (setf (absolute-onset-timings looper) nil
	  (recorder-node looper) (synth 'record-buf
					:in in
					:bufn (bufnum (buffer looper))
					:run 1
					:loop loop)
	  (recording-p looper) t)))

(defun stop-recording (&key (looper (default-looper)))
  (when (recording-p looper)
    (let ((node (recorder-node looper)))
      (ctrl node :run 0)
      (ctrl node :stop 1))))


;;; Playing

;;; TODO Is this needed?
(defmethod cue-repetition ((looper looper))
  (incf (play-repetitions looper)))

(defun start-playing (&key
			(looper (default-looper))
			(time (now)) (dur (dur looper))
			(n 1) (offset 0)
			(start-pos 0;(loop-start looper)
			 ))
  (with-accessors ((buffer buffer) (looper-dur dur))
      looper
    (if (or (plusp n) (looping-p looper))
	(let ((dur (min dur (buffer-dur buffer))))
	  (setf (playing-p looper) t)
	  (at time (push (synth 'play-buf :bufn (bufnum buffer)
					  :dur dur
					  :start-pos start-pos)
			 (player-nodes looper)))
	  (let ((next-time (- (+ time dur) offset)))
	    (callback next-time
		      #'start-playing
		      :looper looper
		      :time next-time
		      :dur dur
		      :n (- n 1)
		      :offset offset
		      :start-pos start-pos)))
	;; no more repetitions
	(progn
	  (setf (playing-p looper) nil)
	  (when jamais-vu.gui:*window*
	    (cl+qt:signal! jamais-vu.gui:*window* (play-finished string) "PLAY"))))))

(defun start-playing-random-start (&key
				     (looper (default-looper))
				     (dur 0.5))
  "Starts playback on LOOPER for DUR seconds, from a random starting point."
  (with-accessors ((absolute-onset-timings absolute-onset-timings)
		   (buffer buffer))
      looper
    (start-playing
     :start-pos (if absolute-onset-timings
		    (* (alexandria:random-elt absolute-onset-timings)
		       *server-sample-rate*)
		    (random (frames buffer)))
     :dur dur)))

;;; TODO
(defun start-looping (&key (looper (default-looper)))
  (setf (looping-p looper) t)
  (start-playing :looper looper))

(defun stop-looping (&key (looper (default-looper)))
  (setf (looping-p looper) nil))

(defun stop-playing (&key (looper (default-looper)))
  (with-accessors ((playing-p playing-p) (player-nodes player-nodes))
      looper
    (when playing-p
      (mapc (lambda (n) (ctrl n :gate 0)) player-nodes))
    (setf (playing-p looper) nil)))

;;; Clear buffer

(defun clear-buffer (&key (looper (default-looper)))
  (buffer-zero (buffer looper)))

;;; Transform buffer

(defun halve-buffer (&key (looper (default-looper)))
  (with-accessors ((buffer buffer)) looper
    (setf (frames buffer) (/ (frames buffer) 2))))


;;; Random sign delta

(defun random-sign (x prob)
  (if (> prob (random 1.0))
      (- x)
      x))

(defun random-sign-delta (seq prob)
  (typecase seq
    (list 
     (loop :for (a b) :on seq
	   :while b
	   :collect (random-sign (- b a) prob)))
    (vector
     (loop :with results := (make-array (1- (length seq)))
	   :for i :below (1- (length seq))
	   :do (setf (aref results i) (random-sign (- (aref seq (1+ i))
						      (aref seq i))
						   prob))
	   :finally (return results)))))


(defun make-wave-file (path &key frames (channels 1) (sample-rate 44100))
  (sf:with-open-sndfile (file path :direction :output :chanls channels :sr sample-rate)
    (sf:write-frames-float frames file)))

(defun buffer-load-from-list (buffer frames)
  "Send FRAMES to BUFFER using a temporary file."
  (uiop:with-temporary-file (:stream file
			     :pathname path
			     :type "wav"
			     :element-type '(unsigned-byte 32))
    (declare (ignore file))
    (make-wave-file path :frames frames)
    (buffer-read path :bufnum (bufnum buffer))))

(defun buf-random-sign-delta (&key (looper (default-looper)) (prob 0.2))
  "Destructively transforms the audio buffer in LOOPER, so that each pair of frames is replaced by the value of their difference. Furthermore, this number can be randomly inverted, with probability PROB (between 0 and 1)."
  (let* ((old-buffer (buffer looper))	 (deltas (random-sign-delta (buffer-load-to-list old-buffer)
				    (/ prob 2)))) ; when prob==1 the signal is just phase inverted;
					          ; 0.5 corresponds to the  maximum effect
    (buffer-load-from-list (buffer looper)
			   (loop :for d :in deltas
				 :for v := (buffer-get old-buffer 0)
				   :then (let ((sum (+ v d)))
					   (if (< -1.0 sum 1.0)
					       sum
					       (- v d)))
				 :collect v :into results
				 :finally (return (push 0.0 results))))))


;;; T-Grains

(defparameter *t-grains-on* nil)

(defun t-grains (&key (looper (default-looper)))
  (with-accessors ((buffer buffer)) looper
    (proxy :t-grains
	   (with-controls ((rate 32) (pos 50) (amp 1.0))
	     (let* ((t-rate #+mouse (mouse-y.kr 8 120 :exp)
			    #-mouse rate)
		    (dur (/ 12 t-rate))
		    (clk (impulse.kr t-rate))
		    (position (+ (* (buf-dur.kr buffer)
				    #+mouse (mouse-x.kr 0 100)
				    #-mouse pos
				    0.01)
				 (t-rand.kr 0 0.01 clk)))
		    (pan (white-noise.kr 0.6)))
	       (* (tgrains.ar 2 clk buffer 1 position dur pan 0.5)
		  amp)))
	   :fade 3.0))
  (setf *t-grains-on* t))

(defun stop-t-grains ()
  (ctrl :t-grains :gate 0)
  (setf *t-grains-on* nil))

(defun toggle-t-grains (&key (looper (default-looper)))
  (if *t-grains-on*
      (stop-t-grains)
      (t-grains :looper looper)))

(defun ctrl-t-grains (param val)
  (ctrl :t-grains param val))


;;; Poeira

(defparameter *poeira-node* nil)

(defun start-poeira (&key (looper (default-looper)))
  (setf *poeira-node* (synth 'poeira
			     :bufn (bufnum (buffer looper))
			     :density 10
			     :attack 0.04
			     :release 0.02
			     :amp 1.0)))

(defun stop-poeira (&optional (node *poeira-node*))
  (free node)
  (setf *poeira-node* nil))

(defun toggle-poeira (&key (looper (default-looper)))
  (if *poeira-node*
      (stop-poeira)
      (start-poeira :looper looper)))

;;; Insert sines

(defun insert-sines (&key (looper (default-looper)))
  (with-accessors ((buffer buffer))
      looper
    (let* ((sine-buffer (buffer-alloc (* (alexandria:random-elt '(1 2 3 4))
					 5512.5))))
      (wavetable sine-buffer :sine2 (let ((root (midicps (+ 24 (random 24)))))
				      (loop :for i :from 1 :upto 11
					    :append (list (* i root) (/ (peak looper) 11))))
			     :normalize nil
			     :as-wavetable nil)
      (buffer-copy (bufnum sine-buffer) (bufnum buffer) (random (- (frames buffer)
								   (frames sine-buffer)))))))

;;; Sub-buffers

;;; TODO refactor with the looper classs
(defun populate-sub-buffers (&key (looper (default-looper)))
  (mapc (lambda (sub-buf)
	  (let* ((sub-samples (/ (frames (buffer looper))
				 (number-of-sub-bufs looper)))
		 (start (* (position sub-buf (sub-bufs looper))
			   sub-samples))
		 (end (+ start sub-samples)))
	    (setf (frames sub-buf) sub-samples)
	    (buffer-load-from-list sub-buf (coerce (subseq (buffer-contents looper)
							   start end)
						   'list))))
	(sub-bufs looper)))

(defun play-sub-buffers (&key
			   (looper (default-looper))
			   (order '(0 1 2 3 4 5 6 7))
			   (time (now))
			   (offset 0))
  (when order
    (destructuring-bind (id &key
			      ((:dur bdur) (buffer-dur (nth id (sub-bufs looper))))
			      ((:rate rate) 1.0)
			      ((:start-pos start-pos) 0.0)
			      ((:amp amp) 1.0))
	(let ((this-time (first order)))
	  (if (and (integerp this-time)
		   (< this-time 0))
	      (list (abs this-time) :amp 0.0)
	      (alexandria:ensure-list this-time)))
      (when (>= id 0)
        (at time (synth 'play-buf :bufn (bufnum (nth id (sub-bufs looper)))
                                  :dur bdur
                                  :rate rate
                                  :start-pos start-pos
				  :amp amp)))
      (let ((next-time (+ time bdur offset)))
        (callback next-time #'play-sub-buffers
		  :order (rest order) :time next-time :offset offset)))))


(defun sub-buffer-preset (preset)
  (funcall (intern (concatenate 'string "PRESET-" (write-to-string preset)))))

(defun preset-1 ()
  (play-sub-buffers :order (loop :for i :from 0 :upto 60 :collect (list 2 :rate (* 8 (sin i))
									  :dur (random 1.0) :amp 0.15))))


;;; OSC interface

(defparameter *osc-controller-presets*
  #((start-recording stop-recording) ; 1
    (start-playing stop-playing) ; 2
    (start-playing-random-start stop-playing) ; 3
    (toggle-t-grains values) ; 4
    (toggle-poeira values) ; 5
    (start-looping values) ; 6
    (stop-looping values) ; 7
    (buf-random-sign-delta values)) ; 8  
  "A vector of function pairs for note on and note off OSC events.")

(defun external-osc-handler (buffer)
  (declare (type (simple-array (unsigned-byte 8) *) buffer))
  (ignore-errors 
   (destructuring-bind (msg val) (osc:decode-bundle buffer)
     (print msg)
     (if (equal msg "/volume")
	 (ctrl :volume (/ val 127))
	 (let ((preset (aref *osc-controller-presets* (1- val))))
	   (funcall (cond
		      ((equal msg "/on") (first preset))
		      ((equal msg "/off") (second preset))))))
     buffer)))

;;; TODO: condition handler para evitar que erros mandem a thread abaixo

(multiple-value-bind (thread osc-socket)
    (usocket:socket-server "127.0.0.1" *external-osc-port* #'external-osc-handler ()
		 :in-new-thread t
		 :protocol :datagram)
  (setf *osc-thread* thread
	*osc-socket* osc-socket))

;; (defun external-osc-listener (port)
;;   "Listen for OSC messages on PORT and execute the respective commands."
;;   (let ((s (usocket:socket-connect nil nil
;; 				   :local-port port
;; 				   :local-host #(127 0 0 1)
;; 				   :protocol :datagram
;; 				   :element-type '(unsigned-byte 8)))
;; 	(buffer (make-sequence '(vector (unsigned-byte 8)) 1024)))
;;     (unwind-protect
;; 	 (loop :do (usocket:socket-receive s buffer (length buffer))
;; 		   (destructuring-bind (msg val) (osc:decode-bundle buffer)
;; 		     (let ((preset (aref *osc-controller-presets* (1- val))))
;; 		       (funcall (cond
;; 				  ((equal msg "/on") (first preset))
;; 				  ((equal msg "/off") (second preset))))))
;; 	       :unless *osc-on* :do (return))
;;       (when s (usocket:socket-close s)))))

;; (defun start-external-osc-listener (&optional (port *external-osc-port*))
;;   (setf *osc-on* t)
;;   ;; (bt:make-thread
;;   ;;  (lambda () (external-osc-listener port))
;;   ;;  :name (format nil "External OSC listener on port ~a" port))
;;   (external-osc-listener port))

;; (unless *osc-on* (start-external-osc-listener))

;; (defun stop-external-osc-listener ()
;;   (setf *osc-on* nil))
