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
  (let ((data (jamais-vu:buffer-contents (jamais-vu:default-looper))))
    (setf (data plot) (points (make-array (length data)
					  :initial-contents data)
			      600 350))))

(define-subwidget (main-window rec-button) (q+:make-qpushbutton "REC" main-window)
  (setf (q+:checkable rec-button) t))

(define-subwidget (main-window play-button) (q+:make-qpushbutton "PLAY" main-window)
  (setf (q+:checkable play-button) t))

(define-subwidget (main-window play-rnd-button) (q+:make-qpushbutton "HIT" main-window))

(define-subwidget (main-window layout) (q+:make-qvboxlayout main-window)
  (setf (q+:window-title main-window) "Buffer")
  (q+:add-widget layout plot)
  (let ((inner (q+:make-qhboxlayout)))
    (q+:add-widget inner rec-button)
    (q+:add-widget inner play-button)
    (q+:add-widget inner play-rnd-button)
    (q+:add-layout layout inner)))

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

(define-slot (main-window rec-stop) ((dur float))
  (declare (connected main-window (rec-stop float)))
  (setf (q+:checked rec-button) nil)
  (replot plot))

(define-slot (main-window play-finished) ((new-label string))
  (declare (connected main-window (play-finished string)))
  (setf (q+:text play-button) new-label
	(q+:enabled play-button) t))

(defun main ()
  (with-main-window (window 'main-window)
    (setf *window* window)
    (sc:remove-reply-responder "/tr")
    (sc:add-reply-responder "/tr" (lambda (node id value)
				    (jamais-vu:osc-responder node id value window))))
  (sc:add-reply-responder "/tr" #'jamais-vu:osc-responder)
  (setf *window* nil))
