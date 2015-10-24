;;;; defender.lisp

(in-package #:defender)

;;; "defender" goes here. Hacks and glory await!

;;;;;;;;;;;;;;;;;;;;;;;; CONFIG/PRESETS ;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *data-root* (asdf:system-source-directory 'defender)) ; Change to project name
(defparameter *font-root* (merge-pathnames "fonts/" *data-root*))
(defparameter *audio-root* (merge-pathnames "audio/" *data-root*))
(defparameter *gfx-root* (merge-pathnames "gfx/" *data-root*))

;;;; Game Params
(defparameter *game-width* 800)
(defparameter *game-height* 600)
(defparameter *game-state* 0) ; 0:menu/intro, 1:in game, 2:game over

(defparameter *pause* nil)

(defparameter *key-left* 0)
(defparameter *key-right* 0)
(defparameter *key-up* 0)
(defparameter *key-down* 0)

(defparameter *player* nil)
(defparameter *thrust* nil)

(defparameter *mountain-front* nil)
(defparameter *mountain-behind* nil)

(defparameter *acceleration* 0.15)
(defparameter *horizontal-max-speed* 10)
(defparameter *vertical-max-speed* 5)
(defparameter *horizontal-slowdown* 0.98)
(defparameter *vertical-slowdown* 0.96)

(defparameter *camera* nil)

(defparameter *ss-player* nil)
(defparameter *ss-enemy* nil)
(defparameter *cells* nil)

;;;; Sound Params
(defparameter *mixer-opened* nil)
(defparameter *music* nil)
(defparameter *soundfx* nil)

;;;; GFX Params
(defparameter *gfx-ss-player* (merge-pathnames "player-spritesheet.png" *gfx-root*))
(defparameter *gfx-mountain-1* (merge-pathnames "mountain-1.png" *gfx-root*))
(defparameter *gfx-mountain-2* (merge-pathnames "mountain-2.png" *gfx-root*))
(defparameter *gfx-mountain-mini* (merge-pathnames "mountain-mini-1.png" *gfx-root*))

;;;; Font Params
(defparameter *terminus-ttf-12* 
  (make-instance 'SDL:ttf-font-definition
		 :size 12
		 :filename (merge-pathnames "TerminusTTF.ttf" *font-root*)))
(defparameter *terminus-ttf-18* 
  (make-instance 'SDL:ttf-font-definition
		 :size 18
		 :filename (merge-pathnames "TerminusTTF.ttf" *font-root*)))
(defparameter *terminus-ttf-24* 
  (make-instance 'SDL:ttf-font-definition
		 :size 24
		 :filename (merge-pathnames "TerminusTTF.ttf" *font-root*)))
(defparameter *terminus-ttf-32* 
  (make-instance 'SDL:ttf-font-definition
		 :size 32
		 :filename (merge-pathnames "TerminusTTF.ttf" *font-root*)))

(defparameter *ttf-font-small* nil)
(defparameter *ttf-font-normal* nil)
(defparameter *ttf-font-large* nil)
(defparameter *ttf-font-huge* nil)

;;;;;;;;;;;;;;;;;;;;;;;; TEMPLATES ;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *tmpl-mountain* '((0 450) (100 470) (125 450) (250 500) (310 490) (325 520) 
				(400 480) (430 450) (500 480) (615 440) (625 480) 
				(750 460) (1000 490) (1050 480) 
				(1200 450) (1200 550) (0 550)))

;;;;;;;;;;;;;;;;;;;;;;;; STRUCTS ;;;;;;;;;;;;;;;;;;;;;;;;


(defstruct camera
  (x 0)
  (vx 0))

(defstruct player
  (x 0)
  (y 0)
  (direction 1)
  (vx 0)
  (vy 0))

(defstruct mountain
  (x 0)
  (y 0)
  (data nil))


;;;;;;;;;;;;;;;;;;;;;;;; SLIME ;;;;;;;;;;;;;;;;;;;;;;;;


;;;; CONTINUABLE macro

(defmacro continuable (&body body)
  `(restart-case
       (progn ,@body)
     (continue () :report "Continue")))


;;;; UPDATE-SWANK function

(defun update-swank ()
  (continuable
   (let ((connection (or swank::*emacs-connection*
			 (swank::default-connection))))
     (when connection
       (swank::handle-requests connection t)))))


;;;;;;;;;;;;;;;;;;;;;;;; MATHS ;;;;;;;;;;;;;;;;;;;;;;;;


;;;; SQUARE function

(defun square (x)
  (* x x))


;;;;;;;;;;;;;;;;;;;;;;;; PHYSICS ;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;; PRIMITIVES ;;;;;;;;;;;;;;;;;;;;;;;;


;;;; DRAW-TEXT function

(defun draw-text (string x y r g b &optional (font *ttf-font-normal*))
  (sdl:draw-string-solid-* string
			   x y
			   :color (sdl:color :r r :g g :b b)
			   :font font))


;;;; DRAW-BOX function

(defun draw-box (x y w h r g b)
  (sdl:draw-box (sdl:rectangle-from-midpoint-* x y w h)
		:color (sdl:color :r r :g g :b b)))


;;;; DRAW-LINE function

(defun draw-line (x0 y0 x1 y1 r g b)
  (sdl:draw-line-* x0 y0 x1 y1
		   :color (sdl:color :r r :g g :b b)))


;;;; DRAW-CIRCLE function

(defun draw-circle (x y rad r g b)
  (sdl:draw-circle-* x y rad
		     :color (sdl:color :r r :g g :b b)))


;;;; DRAW-CIRCLE-FILLED function

(defun draw-circle-filled (x y rad r g b)
  (sdl:draw-filled-circle-* x y rad
		     :color (sdl:color :r r :g g :b b)))


;;;; DRAW-ELLIPSE-FILLED function

(defun draw-ellipse-filled (x y rx ry r g b)
  (sdl:draw-filled-ellipse-* x y rx ry
		     :color (sdl:color :r r :g g :b b)))


;;;; DRAW-POLYGON function

(defun draw-polygon (vertices r g b)
  (sdl:draw-polygon vertices :color (sdl:color :r r :g g :b b)))


;;;; PLAY-SOUND function

(defun play-sound (s)
  (sdl-mixer:play-sample (aref *soundfx* s)))

;;;;;;;;;;;;;;;;;;;;;;;; SCENE ;;;;;;;;;;;;;;;;;;;;;;;;

(defun update-scene (p)
  p)

;;;;;;;;;;;;;;;;;;;;;;;; PLAYER ;;;;;;;;;;;;;;;;;;;;;;;;


;;;; CREATE-PLAYER function

(defun create-player ()
  (setf *player* (make-player :x (ash *game-width* -1) :y 250 :direction 1 :vx 0)))


;;;; DRAW-PLAYER function

(defun draw-player (p)
  (if (= (player-direction p) 1)
      (sdl:draw-surface-at-* *ss-player* (round (- (player-x p) 32))
			     (round (player-y p)) :cell 1)
      (sdl:draw-surface-at-* *ss-player* (round (- (player-x p) 32)) 
			     (round (player-y p)) :cell 0)))


;;;; UPDATE-PLAYER function

(defun update-player (p)
  (cond ((= *key-up* 1) 
	 (setf (player-vy p) (- (player-vy p) (* *acceleration* 2))))

	((= *key-down* 1) 
	 (setf (player-vy p) (+ (player-vy p) (* *acceleration* 2)))))

  (cond ((= *key-left* 1)
	 (progn (setf (player-vx p) (- (player-vx p) *acceleration*))
		(setf (player-direction p) -1)))

	((= *key-right* 1)
	 (progn (setf (player-vx p) (+ (player-vx p) *acceleration*))
		(setf (player-direction p) 1))))

  (check-max-speed p)

  (unless (or (= *key-left* 1) (= *key-right* 1))
    (if (< (abs (player-vx p)) 0.5)
	(setf (player-vx p) 0)
	(setf (player-vx p) (* (player-vx p) *horizontal-slowdown*))))

  (unless (or (= *key-up* 1) (= *key-down* 1))
    (if (< (abs (player-vy p)) 0.5)
	(setf (player-vy p) 0)
	(setf (player-vy p) (* (player-vy p) *vertical-slowdown*))))

  (set-player-position p))

;  (setf (camera-x *camera*) (round (/ (+ (- (ash *game-width* -1) (player-x p)) 
;					 (* (camera-x *camera*) 19)) 20))))


;;;; SET-PLAYER-POSITION function

(defun set-player-position (p)

  (when (< (player-vx p) -0.1)
      (setf (player-x p) (- 400 (* (player-vx p) 30))))

  (when (> (player-vx p) 0.1)
      (setf (player-x p) (- 400 (* (player-vx p) 30))))

  (setf (player-y p) (+ (player-y p) (player-vy p)))

  (if (< (player-y p) 120)
      (setf (player-y p) 120))

  (if (> (+ (player-y p) 24) *game-height*)
      (setf (player-y p) (- *game-height* 24))))



;;;; CHECK-MAX-SPEED function

(defun check-max-speed (p)
  (if (< (player-vx p) (- *horizontal-max-speed*))
      (setf (player-vx p) (- *horizontal-max-speed*)))

  (if (> (player-vx p) *horizontal-max-speed*)
      (setf (player-vx p) *horizontal-max-speed*))

  (if (< (player-vy p) (- *vertical-max-speed*))
      (setf (player-vy p) (- *vertical-max-speed*)))

  (if (> (player-vy p) *vertical-max-speed*)
      (setf (player-vy p) *vertical-max-speed*)))

;;;; FIRE-LASER function

(defun fire-laser (p)
  p)

;;;;;;;;;;;;;;;;;;;;;;;; MOUNTAINS ;;;;;;;;;;;;;;;;;;;;;;;;


;;;; CREATE-MOUNTAIN function

(defun create-mountain ()
  (setf *mountain-front* nil)
  (push (make-mountain :x -50 :y 450 :data *tmpl-mountain*) *mountain-front*)
  (push (make-mountain :x 1150 :y 450 :data *tmpl-mountain*) *mountain-front*)

  (setf *mountain-behind* nil)
  (push (make-mountain :x -50 :y 400) *mountain-behind*)
  (push (make-mountain :x 1150 :y 400) *mountain-behind*))


;;;; DRAW-MOUNTAIN function

(defun draw-mountain-old ()
  (let ((mountain nil))
    (dolist (mt *mountain*)
      (dolist (m (mountain-data mt))
	(push (list (+ (first m) (mountain-x mt)) (second m))
	      mountain))
      (draw-polygon mountain 255 255 255)
      (setf mountain nil))))


;;;; DRAW-MOUNTAIN function

(defun draw-mountain ()
;  (dolist (mt *mountain-behind*)
;    (sdl:draw-surface-at-* (sdl:load-image *gfx-mountain-2*) 
;			   (mountain-x mt) (mountain-y mt)))

  (dolist (mt *mountain-front*)
    (sdl:draw-surface-at-* (sdl:load-image *gfx-mountain-1* :alpha 255) 
			   (mountain-x mt) (mountain-y mt))))

;  (sdl:draw-surface-at-* (sdl:load-image *gfx-mountain-mini* :alpha 255)
;			 200 60))


;;;; UPDATE-MOUNTAIN-POSITION function

(defun update-mountain-position (p)
  (dolist (mt *mountain-front*)
    (setf (mountain-x mt) (round (- (mountain-x mt) (player-vx p)))))

  (dolist (mt *mountain-behind*)
    (setf (mountain-x mt) (round (- (mountain-x mt) (/ (player-vx p) 3)))))

  (determine-furthest-mountain))


;;;; DETERMINE-FURTHEST-MOUNTAIN function

(defun determine-furthest-mountain ()
  (let ((mountain-1 (mountain-x (first *mountain-front*)))
	(mountain-2 (mountain-x (second *mountain-front*)))
	(mountain-3 (mountain-x (first *mountain-behind*)))
	(mountain-4 (mountain-x (second *mountain-behind*))))

    ; front mountain
    (when (and (> mountain-1 1200) (< (player-vx *player*) 0))
      (setf (mountain-x (first *mountain-front*)) (- mountain-2 1200)))
    (when (and (> mountain-2 1200) (< (player-vx *player*) 0))
      (setf (mountain-x (second *mountain-front*)) (- mountain-1 1200)))

    (when (and (< mountain-1 -1200) (> (player-vx *player*) 0))
      (setf (mountain-x (first *mountain-front*)) (+ mountain-2 1200)))
    (when (and (< mountain-2 -1200) (> (player-vx *player*) 0))
      (setf (mountain-x (second *mountain-front*)) (+ mountain-1 1200)))

    ; rear mountain
    (when (and (> mountain-3 1200) (< (player-vx *player*) 0))
      (setf (mountain-x (first *mountain-behind*)) (- mountain-4 1200)))
    (when (and (> mountain-4 1200) (< (player-vx *player*) 0))
      (setf (mountain-x (second *mountain-behind*)) (- mountain-3 1200)))

    (when (and (< mountain-3 -1200) (> (player-vx *player*) 0))
      (setf (mountain-x (first *mountain-behind*)) (+ mountain-4 1200)))
    (when (and (< mountain-4 -1200) (> (player-vx *player*) 0))
      (setf (mountain-x (second *mountain-behind*)) (+ mountain-3 1200)))))


;;;;;;;;;;;;;;;;;;;;;;;; CAMERA ;;;;;;;;;;;;;;;;;;;;;;;;

(defun create-camera (p)
  (setf *camera* (make-camera :x (- (/ *game-width* 2) (player-x p)) 
			      :vx 0)))

;;;;;;;;;;;;;;;;;;;;;;;; LEVEL ;;;;;;;;;;;;;;;;;;;;;;;;


;;;; DISPLAY-LEVEL function

(defun display-level ()
)


;;;; DRAW-GAME-UI function

(defun draw-game-ui ()
  (draw-line 0 120 *game-width* 120 255 255 255)
  (draw-line 200 0 200 120 255 255 255)
  (draw-line (- *game-width* 200) 0 (- *game-width* 200) 120 255 255 255)

  (draw-text (format nil "Thrust: ~a" (player-vx *player*))
	     20 20 255 255 255 *ttf-font-small*)

  (draw-text (format nil "Camera: ~a" (camera-x *camera*))
	     20 40 255 255 255 *ttf-font-small*)

  (draw-text (format nil "Player: ~ax~a" 
		     (round (player-x *player*)) (round (player-y *player*)))
	     20 60 255 255 255 *ttf-font-small*)

  (draw-text (format nil "Mountain: ~a : ~a" 
		     (mountain-x (first *mountain-front*))
		     (mountain-x (second *mountain-front*)))
		     20 80 255 255 255 *ttf-font-small*)

  
  (draw-text (format nil "FPS: ~a"
		     1)
		     620 20 255 255 255 *ttf-font-small*)

  (if (eql *pause* t)
      (draw-text "Paused" 
	     380 280 255 255 255 *ttf-font-large*)))


;;;;;;;;;;;;;;;;;;;;;;;; SCREENS ;;;;;;;;;;;;;;;;;;;;;;;;


;;;; DISPLAY-GAME-OVER function

(defun display-game-over ()
  (draw-text "Game Over" 20 100 255 255 255)

  (draw-text "Press SPACE to Continue..." 290 520 255 255 255))


;;;; DISPLAY-LEVEL-COMPLETE function

(defun display-level-complete ()
  (draw-text "Wave End" 20 100 255 255 255)

  (draw-text "Press SPACE to Continue..." 290 520 255 255 255))


;;;; DISPLAY-MENU function

(defun display-menu ()
  (draw-text "Intro Screen" 20 100 255 255 255)

  (draw-text "Press SPACE to Continue..." 290 520 255 255 255))


;;;;;;;;;;;;;;;;;;;;;;;; GAME STATE ;;;;;;;;;;;;;;;;;;;;;;;;


;;;; PAUSE-GAME function

(defun pause-game ()
  (if (eql *pause* nil)
      (setf *pause* t)
      (setf *pause* nil)))


;;;; STATE-IN-PLAY function

(defun state-in-play ()
  
  (unless (eql *pause* t)
    (update-player *player*)
    (update-mountain-position *player*)
  )

  (display-level)
  (draw-mountain)
  (draw-player *player*)
  (draw-game-ui))


;;;; CONTINUE-OPTION function

(defun continue-option ()
  (cond ((zerop *game-state*) (change-game-state))
	((= *game-state* 2) (change-game-state))
	((= *game-state* 3) (change-game-state))
	(t ())))


;;;; CHANGE-GAME-STATE function
;;;; 0:menu, 1:in game, 2:level complete, 3:game over, 4:hi score
(defun change-game-state ()
  (cond ((zerop *game-state*) 
	 (progn (reset-game)
		(setf *game-state* 1)))

	((= *game-state* 1) (setf *game-state* 2))
	
	((= *game-state* 2) (setf *game-state* 1))

	((= *game-state* 3) (setf *game-state* 0))
	
	(t ())))


;;;;;;;;;;;;;;;;;;;;;;;; THE GAME ;;;;;;;;;;;;;;;;;;;;;;;;


;;;; RENDER function

(defun render ()
  (update-swank)
  (sdl:clear-display sdl:*black*)

  (cond ((= *game-state* 1) (state-in-play))

	((= *game-state* 2) (display-level-complete))

	((= *game-state* 3) (display-game-over))

	(t (display-menu)))

  (sdl:update-display))


;;;; RESET-GAME function

(defun reset-game ()
  (setf *pause* nil)
  (create-player)
  (create-mountain)
  (create-camera *player*))


;;;; INITIALIZE-GAME function

(defun initialize-game ()
  (setf *game-state* 0))


;;;; LOAD-SPRITE-SHEET function

(defun load-sprite-sheet ()
  ; player sprite sheet
  (setf *ss-player* (sdl:load-image *gfx-ss-player* :alpha 255))
  
  (setf *cells* '((0 0 64 24) (0 24 64 24)))

  (setf (sdl:cells *ss-player*) *cells*))



;;;; SETUP-AUDIO function

(defun setup-audio ()
  (setf *soundfx* (make-array 1))
  (sdl-mixer:init-mixer :mp3)
  (setf *mixer-opened* (sdl-mixer:OPEN-AUDIO :chunksize 1024 :enable-callbacks nil))
  (when *mixer-opened*
    ;(setf (aref *soundfx* 0) (sdl-mixer:load-sample (sdl:create-path "beep.ogg" *audio-root*)))
    (sample-finished-action)
    (sdl-mixer:allocate-channels 16)))


;;; SAMPLE-FINISHED-ACTION function

(defun sample-finished-action ()
  (sdl-mixer:register-sample-finished
   (lambda (channel)
     (declare (ignore channel))
     nil)))


;;;; CLEAN-UP function

(defun clean-up ()
  (when *music*
    (when (sdl-mixer:music-playing-p)
      (sdl-mixer:Pause-Music)
      (sdl-mixer:Halt-Music))
    (sdl:Free *music*)
    (setf *music* nil))

  (when (sdl-mixer:sample-playing-p nil)
    (sdl-mixer:pause-sample t)
    (sdl-mixer:Halt-sample :channel t))

  (loop for s below (length *soundfx*)
     do (if (equal (aref *soundfx* s) 0)
	    t
	    (progn (sdl:free (aref *soundfx* s))
		   (setf (aref *soundfx* s) 0))))
  
  (when *mixer-opened*
    (sdl-mixer:Close-Audio t)
    (setf *mixer-opened* nil))
  (sdl-mixer:quit-mixer))


;;;; START function

(defun start ()
  (initialize-game)
  (reset-game)
  (sdl:with-init (sdl:sdl-init-video sdl:sdl-init-audio)
    (sdl:window *game-width* *game-height* 
		:title-caption "Defender"
		:fps (make-instance 'sdl:fps-timestep))
    (setf (sdl:frame-rate) 60)

    (setup-audio)

    (load-sprite-sheet)

    ;(sdl-mixer:play-music *music-intro* :loop t)

    (unless (sdl:initialise-default-font *terminus-ttf-18*)
      (error "FONT-EXAMPLE: Cannot initialize the default font."))

    (setf *ttf-font-small* (sdl:initialise-font *terminus-ttf-12*))
    (setf *ttf-font-normal* (sdl:initialise-font *terminus-ttf-18*))
    (setf *ttf-font-large* (sdl:initialise-font *terminus-ttf-24*))
    (setf *ttf-font-huge* (sdl:initialise-font *terminus-ttf-32*))
    
    (sdl:with-events ()
      (:quit-event ()
		   (clean-up)
		   t)
      (:key-down-event (:key key)
		       (case key
			 (:sdl-key-left (setf *key-left* 1))
			 (:sdl-key-right (setf *key-right* 1))
			 (:sdl-key-up (setf *key-up* 1))
			 (:sdl-key-down (setf *key-down* 1))

			 (:sdl-key-p (if (= *game-state* 1)
					 (pause-game)))
			 (:sdl-key-q (if (= *game-state* 1)
					 (setf *game-state* 3)))
			 (:sdl-key-space (if (= *game-state* 1)
					     (fire-laser *player*)
					     (continue-option)))
			 (:sdl-key-escape (sdl:push-quit-event))))
      (:key-up-event (:key key)
		     (case key
		       (:sdl-key-left (setf *key-left* 0))
		       (:sdl-key-right (setf *key-right* 0))
		       (:sdl-key-up (setf *key-up* 0))
		       (:sdl-key-down (setf *key-down* 0))))
      (:idle ()
	     (render)))))
