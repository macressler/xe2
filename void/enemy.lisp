(in-package :void)


;;; The Canaz ship

(defcell canaz 
  (tile :initform "canaz")
  (hit-points :initform (make-stat :base 4 :max 4 :min 0))
  (speed :initform (make-stat :base 2))
  (strength :initform (make-stat :base 10))
  (defense :initform (make-stat :base 10))
  (energy :initform (make-stat :base 100 :min 0 :max 100))
  (stepping :initform t)
  (movement-cost :initform (make-stat :base 6))
  (equipment-slots :initform '(:center-bay))
  (firing-with :initform :center-bay)
  (max-items :initform (make-stat :base 2))
  (direction :initform (random-direction))
  (categories :initform '(:actor :obstacle :enemy :target))
  (description :initform 
"The Canaz is a lightweight silicate-body fighter equipped with a muon cannon.
Not the typical choice of the best pilots."))

(define-method run canaz ()
  (clon:with-field-values (row column) self
    (let* ((world *world*)
	   (direction [direction-to-player *world* row column])
	   (distance [distance-to-player *world* row column]))
      (if (< distance 8)
	  (progn 
	    (setf <direction> (if (< distance 4)
				  (random-direction)
				  (if (> 3 (random 10))
				      (random-direction)
				      direction)))
	    [>>fire self direction])
	  ;; bounce around 
	  (progn 
	    (when [obstacle-in-direction-p *world* <row> <column> <direction>]
	      (setf <direction> (random-direction)))
	    [>>move self <direction>])))))

(define-method die canaz ()
  [play-sample self "death-alien"]
  [drop self (clone =energy=)]
  [parent>>die self])

(define-method loadout canaz ()
  [make-inventory self]
  [make-equipment self]
  [equip self [add-item self (clone =muon-cannon=)]])

;;; The Berserker is a relatively simple AI enemy.

;; Run in a straight line until hitting an obstacle.
;; Then choose a random direction and try again.
;; If the player gets close, try and attack him.

(defcell berserker 
  (name :initform "Berserker")
  (categories :initform '(:actor :target :obstacle :opaque :enemy :equipper))
  (equipment-slots :initform '(:robotic-arm))
  (speed :initform (make-stat :base 7 :min 7))
  (max-items :initform (make-stat :base 3))
  (movement-cost :initform (make-stat :base 3))
  (tile :initform "humanoid")
  (stepping :initform t)
  (attacking-with :initform :robotic-arm)
  (max-weight :initform (make-stat :base 25))
  (direction :initform (xe2:random-direction))
  (strength :initform (make-stat :base 4 :min 0 :max 30))
  (dexterity :initform (make-stat :base 5 :min 0 :max 30))
  (intelligence :initform (make-stat :base 11 :min 0 :max 30))
  (hit-points :initform (make-stat :base 10 :min 0 :max 10))
  (description :initform 
"A protocol droid gone mad from radiation damage to the neural circuits.
Berserkers attack with a shock probe."))

(define-method initialize berserker ()
  [make-inventory self]
  [make-equipment self])

(define-method run berserker ()
  (clon:with-field-values (row column) self
    (let ((world *world*))
      (if (< [distance-to-player world row column] 5)
	  (let ((player-dir [direction-to-player world row column]))
	    (if [adjacent-to-player world row column]
		[>>attack self player-dir]
		[>>move self player-dir]))
	  (progn (when [obstacle-in-direction-p world row column <direction>]
		   (setf <direction> (xe2:random-direction)))
		 [>>move self <direction>])))))

(define-method die berserker ()
  (when (> 5 (random 10))
    [drop self (clone (random-powerup))])
  [play-sample self "blaagh"]
  [parent>>die self])

(define-method loadout berserker ()
  (let ((probe (clone =shock-probe=)))
    [equip self [add-item self probe]]))

(define-method attack berserker (target)
  [play-sample self "drill-little"]
  [parent>>attack self target])


;;; Spike beam 

; (defcell spike-beam 

;;; Guardians protect a given cell.

(defcell guardian 
  (categories :initform '(:actor :target :obstacle :opaque :enemy))
  (equipment-slots :initform '(:robotic-arm :shoulder-mount))
  (attacking-with :initform :robotic-arm)
  (firing-with :initform :robotic-arm)
  (dexterity :initform (make-stat :base 11))
  (max-items :initform (make-stat :base 1))
  (speed :initform (make-stat :base 10))
  (stepping :initform t)
  (behavior :initform :homing)
  (clock :initform 8)
  (clock-reset-value :initform 8)
  (scouting-direction :initform :north)
  (attack-distance :initform 10)
  (strength :initform (make-stat :base 10))
  (movement-cost :initform (make-stat :base 7))
  (tile :initform "guardian")
  (defended-cell :initform nil)
  (hit-points :initform (make-stat :base 20 :min 0 :max 60))
  (description :initform 
"The Guardian cell patrols the area around a given protected cell,
and attacks anyone who comes near."))

(define-method defend guardian (defended-cell)
  (setf <defended-cell> defended-cell))

(define-method run guardian ()
  (ecase <behavior>
    (:homing [home self])
    (:scouting [scout self])))

(define-method home guardian ()
  (decf <clock>)
  (clon:with-field-values (row column) self
    (if (< [distance-to-player self] <attack-distance>)
	(let ((direction [direction-to-player self])
	      (world *world*))
	  (if [adjacent-to-player self]
	      (progn (format t "FOO")
		     [attack self direction])
	      (progn
		(when [obstacle-in-direction-p world row column direction]
		  (setf <direction> (random-direction)))
		[move self direction])))
	;; otherwise, move toward the defended cell until clock runs out
	(let* ((cell <defended-cell>)
	       (r0 (field-value :row cell))
	       (c0 (field-value :column cell)))
	  (if [adjacent-to-player self]
	      [attack self direction])
	  (when [obstacle-in-direction-p *world* row column (direction-to row column r0 c0)]
	    (setf <direction> (random-direction))
	    [move self <direction>])
	  [move self (direction-to row column r0 c0)]
	  (when (<= <clock> 0)
	    (setf <scouting-direction> (random-direction))
	    (setf <clock> <clock-reset-value>)
	    (setf <behavior> :scouting))))))

(define-method scout guardian ()
  (decf <clock>)
  ;; check for player 
  (if (< [distance-to-player self] <attack-distance>)
      (setf <behavior> :homing)
      ;; are we done scouting? then begin homing. 
      (if (<= <clock> 0)
	  (setf <clock> <clock-reset-value>
		<behavior> :homing)
	  ;; otherwise continue scouting
	  [move self <scouting-direction>])))

(define-method loadout guardian ()
  [make-inventory self]
  [make-equipment self]
  [equip self [add-item self (clone =shock-probe=)]])

;;; The GOND is a multi-warhead-launching superguardian

(define-prototype gond (:parent =guardian=)
  (tile :initform "gond")
  (attack-distance :initform 14)
  (strength :initform (make-stat :base 14))
  (hit-points :initform (make-stat :base 20 :min 0 :max 60)))

(define-method loadout gond ()
  [make-inventory self]
  [make-equipment self]
  [equip self [add-item self (clone =multi-missile-launcher=)]])

(define-method home gond ()
  (decf <clock>)
  (clon:with-field-values (row column) self
    (if (< [distance-to-player self] <attack-distance>)
	[fire self [direction-to-player self]]
	;; otherwise, move toward the defended cell until clock runs out
	(let* ((cell <defended-cell>)
	       (r0 (field-value :row cell))
	       (c0 (field-value :column cell)))
	  (if [obstacle-in-direction-p *world* row column 
				       (direction-to row column r0 c0)]
	      (progn (setf <direction> (random-direction))
		     [move self <direction>])
	      [move self (direction-to row column r0 c0)])
	  (when (<= <clock> 0)
	    (setf <scouting-direction> (random-direction))
	    (setf <clock> <clock-reset-value>)
	    (setf <behavior> :scouting))))))

(define-method die gond ()
  [play-sample self "blaagh3"]
  [parent>>die self])

;;; The speed-sucking Lymphocytes

(defcell lymphocyte
 (tile :initform "lymphocyte")
 (hit-points :initform (make-stat :base 12 :max 12 :min 0))
 (speed :initform (make-stat :base 4))
 (strength :initform (make-stat :base 10))
 (defense :initform (make-stat :base 10))
 (stepping :initform t)
 (movement-cost :initform (make-stat :base 10))
 (max-items :initform (make-stat :base 2))
 (direction :initform (random-direction))
 (categories :initform '(:actor :obstacle :enemy :target))
 (description :initform
"STAY AWAY! These can cause permanent speed drains, paralysis, and death."))

(define-method speedsuck lymphocyte (victim)
  [play-sample self "lymph"]
  [>>say :narrator "The speed-sucking Lymphocyte drains your speed by 2 points!"]
  [stat-effect victim :speed -2])

(define-method run lymphocyte ()
  (clon:with-field-values (row column) self
    (let ((world *world*))
      (if (< [distance-to-player world row column] 8)
	  (let ((player-dir [direction-to-player world row column]))
	    (if [adjacent-to-player world row column]
		[>>speedsuck self [resolve self player-dir]]
		[>>move self player-dir]))
	  (progn (when [obstacle-in-direction-p world row column <direction>]
		   (setf <direction> (xe2:random-direction)))
		 [>>move self <direction>])))))
