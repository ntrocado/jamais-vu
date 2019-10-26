;;;; gui.lisp

(in-package #:jamais-vu.gui)
(in-readtable :qtools)

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
		    (pen (q+:make-qpen))
		    (line (q+:make-qlinef)))
    (setf (q+:width pen) 2)
    (setf (q+ color pen) (apply #'q+:make-qcolor (loop :repeat 4
						       :collect (random 255))))
    (setf (q+:pen painter) pen)
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
  (let ((data (jamais-vu:buffer-contents (jamais-vu:default-looper))))
    (setf (data plot) (points (make-array (length data)
					  :initial-contents data)
			      600 350))))

(define-subwidget (main-window loop-checkbox) (q+:make-qcheckbox "&Loop" main-window))

(define-subwidget (main-window rec-button) (q+:make-qpushbutton "REC" main-window)
  (setf (q+:checkable rec-button) t))

(define-subwidget (main-window play-button) (q+:make-qpushbutton "PLAY" main-window)
  (setf (q+:checkable play-button) t))

(define-subwidget (main-window play-rnd-button) (q+:make-qpushbutton "HIT" main-window))

(define-subwidget (main-window random-sign-delta-button) (q+:make-qpushbutton "DESTROY" main-window))

(define-subwidget (main-window insert-sines-button) (q+:make-qpushbutton "SINES" main-window))

;;; FX

;;; Grains

(define-subwidget (main-window grains-pos-slider)
    (make-instance 'qtools-elements:slider
		   :maximum 100.0
		   :minimum 1.0
		   :stepping 0.01
		   :default 50
		   :caption "Position"
		   :curve :lin))

(define-subwidget (main-window grains-rate-slider)
    (make-instance 'qtools-elements:slider
		   :maximum 120
		   :minimum 8
		   :stepping 0.01
		   :default 32
		   :caption "Rate"
		   :curve :exp))

(define-subwidget (main-window grains-amp-slider)
    (make-instance 'qtools-elements:slider
		   :maximum 1.5
		   :minimum 0.0
		   :stepping 0.01
		   :default 1.0
		   :caption "Volume"
		   :curve :lin))

(define-subwidget (main-window grains-group) (q+:make-qgroupbox "TGrains")
  (setf (q+:checkable grains-group) t
	(q+:checked grains-group) nil)
  (let ((grains-layout (q+:make-qvboxlayout)))
    (q+:add-widget grains-layout grains-pos-slider)
    (q+:add-widget grains-layout grains-rate-slider)
    (q+:add-widget grains-layout grains-amp-slider)
    (setf (q+:layout grains-group) grains-layout)))

(define-slot (main-window grains-pos-slider) ((value double))
  (declare (connected grains-pos-slider (value-changed double)))
  (jamais-vu::ctrl-t-grains :pos value))

(define-slot (main-window grains-rate-slider) ((value double))
  (declare (connected grains-rate-slider (value-changed double)))
  (jamais-vu::ctrl-t-grains :rate value))

(define-slot (main-window grains-amp-slider) ((value double))
  (declare (connected grains-amp-slider (value-changed double)))
  (jamais-vu::ctrl-t-grains :amp value))

(define-slot (main-window grains) ()
  (declare (connected grains-group (toggled boolean)))
  (if (q+:is-checked grains-group) 
      (jamais-vu:t-grains)
      (jamais-vu:stop-t-grains)))

;;; Poeira

(define-subwidget (main-window poeira-density-slider)
    (make-instance 'qtools-elements:slider
		   :maximum 40
		   :minimum 2
		   :stepping 0.5
		   :default 10
		   :caption "Density"
		   :curve :lin))

(define-subwidget (main-window poeira-fade-slider)
    (make-instance 'qtools-elements:slider
		   :maximum 0.8
		   :minimum 0.01
		   :stepping 0.01
		   :default 0.04
		   :caption "Attack"
		   :curve :exp))

(define-subwidget (main-window poeira-amp-slider)
    (make-instance 'qtools-elements:slider
		   :maximum 1.5
		   :minimum 0.0
		   :stepping 0.01
		   :default 1.0
		   :caption "Volume"
		   :curve :lin))

(define-subwidget (main-window poeira-group) (q+:make-qgroupbox "Poeira")
  (setf (q+:checkable poeira-group) t
	(q+:checked poeira-group) nil)
  (let ((poeira-layout (q+:make-qvboxlayout)))
    (q+:add-widget poeira-layout poeira-density-slider)
    (q+:add-widget poeira-layout poeira-fade-slider)
    (q+:add-widget poeira-layout poeira-amp-slider)
    (setf (q+:layout poeira-group) poeira-layout)))

(define-slot (main-window poeira-density-slider) ((value double))
      (declare (connected poeira-density-slider (value-changed double)))
  (sc:ctrl jamais-vu::*poeira-node* :density value))

(define-slot (main-window poeira-fade-slider) ((value double))
      (declare (connected poeira-fade-slider (value-changed double)))
  (sc:ctrl jamais-vu::*poeira-node* :attack value))

(define-slot (main-window poeira-amp-slider) ((value double))
      (declare (connected poeira-amp-slider (value-changed double)))
  (sc:ctrl jamais-vu::*poeira-node* :amp value))

(define-slot (main-window poeira) ()
  (declare (connected poeira-group (toggled boolean)))
  (if (q+:is-checked poeira-group) 
      (jamais-vu:start-poeira)
      (jamais-vu:stop-poeira)))

;;; Layout

(define-subwidget (main-window layout) (q+:make-qvboxlayout main-window)
  (setf (q+:window-title main-window) "Buffer")
  (q+:add-widget layout plot)
  (let ((inner (q+:make-qhboxlayout)))
    (q+:add-widget inner loop-checkbox)
    (q+:add-widget inner rec-button)
    (q+:add-widget inner play-button)
    (q+:add-widget inner play-rnd-button)
    (q+:add-widget inner random-sign-delta-button)
    (q+:add-widget inner insert-sines-button)
    (q+:add-layout layout inner))
  (q+:add-widget layout grains-group)
  (q+:add-widget layout poeira-group))

(defun replot (plot)
  (let ((width (q+:width plot))
	(height (q+:height plot))
	(data (jamais-vu:buffer-contents (jamais-vu:default-looper))))
    (setf (data plot) (points (make-array (length data)
					  :initial-contents data)
			      width height))
    (q+:repaint plot)))

(define-override (main-window resize-event) (ev)
  (declare (ignore ev))
  (replot plot))

(define-signal (main-window rec-stop) (float))
(define-signal (main-window play-finished) (string))

(define-slot (main-window loop) ()
  (declare (connected loop-checkbox (state-changed int)))
  (setf (jamais-vu:looping-p (jamais-vu:default-looper)) (q+:is-checked loop-checkbox)))

(define-slot (main-window record) ()
  (declare (connected rec-button (toggled boolean)))
  (if (q+:is-checked rec-button)
      (progn (jamais-vu:start-recording) (setf (q+:text rec-button) "Recording"))
      (progn (jamais-vu:stop-recording) (setf (q+:text rec-button) "REC"))))

(define-slot (main-window play) ()
  (declare (connected play-button (toggled boolean)))
  (if (q+:is-checked play-button)
      (progn (jamais-vu:start-playing)
	     (setf (q+:text play-button) "Playing"
		   (q+:enabled play-rnd-button) nil))
      (progn (jamais-vu:stop-playing)
	     (setf (q+:text play-button) "PLAY"
		   (q+:enabled play-rnd-button) t))))

(define-slot (main-window play-rnd) ()
  (declare (connected play-rnd-button (pressed)))
  (setf (q+:enabled play-button) nil)
  (jamais-vu:start-playing-random-start :dur (+ 0.1 (random 0.25))))

(define-slot (main-window random-sign-delta-button) ()
  (declare (connected random-sign-delta-button (pressed)))
  (jamais-vu:buf-random-sign-delta)
  (replot plot))

(define-slot (main-window insert-sines-button) ()
  (declare (connected insert-sines-button (pressed)))
  (jamais-vu:insert-sines)
  (replot plot))

(define-slot (main-window rec-stop) ((dur float))
  (declare (connected main-window (rec-stop float)))
  (setf (q+:checked rec-button) nil)
  (replot plot))

(define-slot (main-window play-finished) ((new-label string))
  (declare (connected main-window (play-finished string)))
  (setf (q+:text play-button) new-label
	(q+:enabled play-button) t
	(q+:checked play-button) nil))

(defun main ()
  (with-main-window (window 'main-window)
    (setf *window* window)
    (sc:remove-reply-responder "/tr")
    (sc:add-reply-responder "/tr" (lambda (node id value)
				    (jamais-vu:osc-responder node id value window))))
  (sc:add-reply-responder "/tr" #'jamais-vu:osc-responder)
  (setf *window* nil))
