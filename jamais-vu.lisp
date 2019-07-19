;;;; jamais-vu.lisp

(in-package #:jamais-vu)
(in-readtable :qtools)

(defparameter *server-sample-rate* 44100)
(defparameter *default-buffer-duration* 8.0)
(defparameter *default-number-of-sub-bufs* 8)
(defparameter *default-input* 0)

(defvar *external-osc-port* 8000)
(defvar *osc-thread*)
(defvar *osc-socket*)
(defvar *osc-on* t)

(defconstant +rec-done+ 0)
(defconstant +onset+ 1)
(defconstant +play-done+ 2)

(defun server-start ()
  (setf *s* (make-external-server "localhost" :port 4445))
  (server-boot *s*))

(unless (and *s* (sc::boot-p *s*)) (server-start))

(defparameter *looper-last-id* 0)

(defclass looper ()
  ((id
    :initarg :id
    :accessor id
    :initform (incf *looper-last-id*))
   (name
    :initarg :name
    :accessor name
    :initform "Unnamed looper")
   (dur
    :initarg :dur
    :accessor dur
    :initform *default-buffer-duration*)
   (number-of-sub-bufs
    :initarg :number-of-sub-bufs
    :accessor number-of-sub-bufs
    :initform *default-number-of-sub-bufs*)
   (recording-p
    :accessor recording-p
    :initform nil)
   (playing-p
    :accessor playing-p
    :initform nil)
   (play-repetitions
    :accessor play-repetitions
    :initform 1)
   (looping-p
    :accessor looping-p
    :initform nil)
   (inter-onset-timings
    :accessor inter-onset-timings
    :initform '())
   (absolute-onset-timings
    :accessor absolute-onset-timings
    :initform '())
   (loop-start
    :accessor loop-start
    :initform 0)
   (recorder-node
    :accessor recorder-node)
   (player-nodes
    :accessor player-nodes
    :initform nil)
   (buffer
    :accessor buffer)
   (sub-bufs
    :accessor sub-bufs)))

(defmethod initialize-instance :after ((new-looper looper) &key)
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
  (list (make-instance 'looper :name "Default")))

(defparameter *loopers* (init-loopers)
  "Global list of active loopers.")

(defun default-looper ()
    (find-looper-by-name "Default"))

(defun find-looper-by-name (name &optional (loopers *loopers*))
  (find name loopers :key #'name :test 'string-equal))


;;; Synth definitions

(defsynth record-buf ((in 0) bufn (run 1) (stop 0) (loop 0))
  (let* ((sound-in (sound-in.ar in))
	 (rec (record-buf.ar sound-in bufn :run run :loop loop :act :free))
	 (timer-stop (timer.kr stop))
	 (timer-done (timer.kr (done.kr rec)))
	 (onset-detected (coyote.kr sound-in))
	 (onset-timing (timer.kr onset-detected)))
    (send-trig.kr onset-detected 1 onset-timing)
    (send-trig.kr stop 0 timer-stop)
    (send-trig.kr (done.kr rec) 0 timer-done)
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
		  (release 0.01))
  (let* ((frames (buf-frames.kr bufn))
	 (trig (dust.kr density))
	 (sound (play-buf.ar 1 bufn 1.0
			     :trig trig
			     :start-pos (ti-rand.kr 0 frames trig)))
	 (envelope (env-gen.kr (perc attack release) :gate trig)))
    (out.ar out (* sound envelope))))

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
     (when qobject (signal! qobject (rec-stop float) value))
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

    (otherwise (format t "~&Unable to handle OSC message with node ~a, id ~a, and value ~a.~%" node id value))))

(add-reply-responder "/tr" #'osc-responder)


;;; Recording

(defun start-recording (&key (looper (default-looper)) (in *default-input*) (loop 0))
  (setf (recording-p looper) t
	(absolute-onset-timings looper) nil
	(recorder-node looper) (synth 'record-buf
				      :in in
				      :bufn (bufnum (buffer looper))
				      :run 1
				      :loop loop)))

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
	  (setf (playing-p looper) nil
		(looping-p looper) nil)
	  (when *window*
	    (signal! *window* (play-finished string) "PLAY"))))))

(defun start-playing-random-start (&key
				     (looper (default-looper))
				     (dur 0.5))
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

(defun stop-playing (&key (looper (default-looper)))
  (with-accessors ((playing-p playing-p) (player-nodes player-nodes))
      looper
    (when playing-p
      (mapc (lambda (n) (ctrl n :gate 0)) player-nodes))
    (setf (playing-p looper) nil
	  (looping-p looper) nil)))

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
    (if (zerop (1- (random 2)))
	(abs x)
	(- (abs x)))
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
  (uiop:with-temporary-file (:stream file
			     :pathname path
			     :type "wav"
			     :element-type '(unsigned-byte 32))
    (declare (ignore file))
    (make-wave-file path :frames frames)
    (buffer-read path :bufnum (bufnum buffer))))

(defun buf-random-sign-delta (&key (looper (default-looper)) (prob 7))
  (let* ((old-buffer (buffer looper))
	 (deltas (random-sign-delta (buffer-load-to-list old-buffer)
				    prob)))
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
	   (let* ((t-rate (mouse-y.kr 8 120 :exp))
		  (dur (/ 12 t-rate))
		  (clk (impulse.kr t-rate))
		  (pos (+ (mouse-x.kr 0 (buf-dur.kr buffer))
			  (t-rand.kr 0 0.01 clk)))
		  (pan (white-noise.kr 0.6)))
	     (tgrains.ar 2 clk buffer 1 pos dur pan 0.1))))
  (setf *t-grains-on* t))

(defun stop-t-grains ()
  (free :t-grains)
  (setf *t-grains-on* nil))

(defun toggle-t-grains (&key (looper (default-looper)))
  (if *t-grains-on*
      (stop-t-grains)
      (t-grains :looper looper)))

;;; Poeira

(defparameter *poeira-node* nil)

(defun start-poeira (&key (looper (default-looper)))
  (setf *poeira-node* (synth 'poeira
			     :bufn (bufnum (buffer looper))
			     :density 10
			     :attack 0.2
			     :release 0.4)))

(defun stop-poeira (&optional (node *poeira-node*))
  (free node)
  (setf *poeira-node* nil))

(defun toggle-poeira (&key (looper (default-looper)))
  (if *poeira-node*
      (stop-poeira)
      (start-poeira :looper looper)))

;;; Sub-buffers

;; (defun populate-sub-buffers ()
;;   (let* ((data (buffer-load-to-list *buf*))
;; 	 (data-len (length data))
;; 	 (samples (make-array data-len
;; 			      :initial-contents (buffer-load-to-list *buf*))))
;;     (mapc (lambda (sub-buf)
;;             (let* ((sub-samples (/ data-len *number-of-sub-bufs*))
;;                    (start (* (position sub-buf *sub-bufs*)
;;                              sub-samples))
;;                    (end (+ start sub-samples)))
;;               (setf (frames sub-buf) sub-samples)
;;               (buffer-setn sub-buf (coerce (subseq samples start end) 'list))))
;;           *sub-bufs*)))

;; (defun play-sub-buffers (&key (order '(0 1 2 3 4 5 6 7))
;;                               (time (now))
;;                               (offset 0))
;;   (when order
;;     (destructuring-bind (id &key
;; 			      ((:dur bdur) (buffer-dur (nth id *sub-bufs*)))
;; 			      ((:rate rate) 1.0)
;; 			      ((:start-pos start-pos) 0.0)
;; 			      ((:amp amp) 1.0))
;; 	(let ((this-time (first order)))
;; 	  (if (and (integerp this-time)
;; 		   (< this-time 0))
;; 	      (list (abs this-time) :amp 0.0)
;; 	      (alexandria:ensure-list this-time)))
;;       (when (>= id 0)
;;         (at time (synth 'play-buf :bufn (bufnum (nth id *sub-bufs*))
;;                                   :dur bdur
;;                                   :rate rate
;;                                   :start-pos start-pos
;; 				  :amp amp)))
;;       (let ((next-time (+ time bdur offset)))
;;         (callback next-time #'play-sub-buffers
;; 		  :order (rest order) :time next-time :offset offset)))))

;;; GUI

(defvar *window* nil)

(define-widget main-window (QWidget)
	       ())

(define-widget plot (QWidget)
	       ((max-y :initarg :max-y :accessor max-y)
		(min-y :initarg :min-y :accessor min-y)
		(step-x :initarg :step-x :accessor step-x)
		(grid :initarg :grid :accessor grid)
		(data :initarg :data :accessor data)
		(adjust :initarg :adjust :accessor adjust))
	       (:default-initargs
		:max-y 100
		:min-y -100
		:step-x NIL
		:grid NIL
		:adjust T
		:data NIL))

(defmethod (setf data) :after (value (plot plot))
  (q+:repaint plot))

(define-override (plot paint-event) (ev)
  (declare (ignore ev))
  (with-finalizing ((painter (q+:make-qpainter plot))
		    (line (q+:make-qlinef)))
    (loop :for segment :across data
	  :do (destructuring-bind (x1 y1 x2 y2) segment
		(setf (q+:line line) (values x1 y1 x2 y2))
		(q+:draw-line painter line)))))

(defun points (data width height)
  "From a vector of samples DATA return a vector of line segments in the form (x1 y1 x2 y2), for plotting the samples in a graph with WIDTH x HEIGHT pixels."
  (let* ((data-len (length data))
	 (step (truncate (/ data-len width)))
	 (zero-y (/ height 2)))
    (loop :with result := (make-array (truncate (/ data-len step))
				      :fill-pointer 0)
	  :for read-pointer :upto (- data-len step) :by step
	  :for data-segment := (subseq data read-pointer (+ read-pointer step))
	  :for i :from 0
	  :do (vector-push 
	       (loop :for d :across data-segment
		     :maximize d :into max
		     :minimize d :into min
		     :finally (return (let ((x1 i)
					    (y1 (- zero-y (* max zero-y)))
					    (x2 i)
					    (y2 (+ zero-y (* (abs min) zero-y))))
					(list x1 y1 x2 y2))))
	       result)
	  :finally (return result))))

(define-subwidget (main-window plot) (make-instance 'plot)
  (setf (q+:minimum-size plot) (values 600 350))
  (let ((data (buffer-load-to-list (buffer (default-looper)))))
    (setf (data plot) (points (make-array (length data)
					  :initial-contents data)
			      600 350))))

(define-subwidget (main-window rec-button) (q+:make-qpushbutton "REC" main-window))

(define-subwidget (main-window stop-button) (q+:make-qpushbutton "STOP" main-window))

(define-subwidget (main-window play-button) (q+:make-qpushbutton "PLAY" main-window))

(define-subwidget (main-window play-rnd-button) (q+:make-qpushbutton "PLAYRND" main-window))

(define-subwidget (main-window layout) (q+:make-qvboxlayout main-window)
  (setf (q+:window-title main-window) "Buffer")
  (q+:add-widget layout plot)
  (let ((inner (q+:make-qhboxlayout)))
    (q+:add-widget inner rec-button)
    (q+:add-widget inner stop-button)
    (q+:add-widget inner play-button)
    (q+:add-widget inner play-rnd-button)
    (q+:add-layout layout inner)))

(defun replot (plot)
  (let ((width (q+:width plot))
	(height (q+:height plot))
	(data (buffer-load-to-list (buffer (default-looper)))))
    (setf (data plot) (points (make-array (length data)
					  :initial-contents data)
			      width height))
    (q+:repaint plot)))

(define-override (main-window resize-event) (ev)
  (declare (ignore ev))
  (replot plot))

(define-signal (main-window rec-stop) (float))
(define-signal (main-window play-finished) (string))

(define-slot (main-window record) ()
  (declare (connected rec-button (pressed)))
  (setf (q+:text rec-button) "Recording")
  (start-recording))

(define-slot (main-window stop) ()
  (declare (connected stop-button (pressed)))
  (signal! main-window (rec-stop float) 0.0))

(define-slot (main-window play) ()
  (declare (connected play-button (pressed)))
  (setf (q+:text play-button) "Playing")
  (start-playing))

(define-slot (main-window play-rnd) ()
  (declare (connected play-rnd-button (pressed)))
  (setf (q+:text play-rnd-button) "Playing")
  (start-playing-random-start :dur (+ 0.1 (random 0.25))))

(define-slot (main-window rec-stop) ((dur float))
  (declare (connected main-window (rec-stop float)))
  (setf (q+:text rec-button) "REC")
  (replot plot))

(define-slot (main-window play-finished) ((new-label string))
  (declare (connected main-window (play-finished string)))
  (setf (q+:text play-button) new-label))

(defun main ()
  (with-main-window (window 'main-window)
    (setf *window* window)
    (remove-reply-responder "/tr")
    (add-reply-responder "/tr" (lambda (node id value)
				 (osc-responder node id value window))))
  (add-reply-responder "/tr" #'osc-responder)
  (setf *window* nil))

;;; OSC interface

(defparameter *osc-controller-presets*
  #((start-recording stop-recording) ; 1
    (start-playing stop-playing) ; 2
    (start-playing-random-start stop-playing) ; 3
    (toggle-t-grains values) ; 4
    (toggle-poeira values)) ; 5
  "A vector of function pairs for note on and note off OSC events.")

(defun external-osc-handler (buffer)
  (declare (type (simple-array (unsigned-byte 8) *) buffer))
  (destructuring-bind (msg val) (osc:decode-bundle buffer)
    (let ((preset (aref *osc-controller-presets* (1- val))))
      (funcall (cond
		 ((equal msg "/on") (first preset))
		 ((equal msg "/off") (second preset))))
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
