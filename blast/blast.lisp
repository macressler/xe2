;; blast.lisp --- a micro shmup in common lisp

;; Copyright (C) 2009  David O'Toole

;; Author: David O'Toole <dto@gnu.org>
;; Keywords: games

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary 

;; Blast Tactics is a micro shoot-em-up in Common Lisp. 

;; You can shoot asteroids or destroy them with your trail.  Powerups
;; extend the trail, enabling higher scores!

;;; Packaging

(defpackage :blast
  (:documentation "Blast Tactics: A sci-fi roguelike for Common Lisp.")
  (:use :rlx :common-lisp)
  (:export blast))

(in-package :blast)

;;; Empty space.

(defcell space 
  (tile :initform "space"))

;;; An explosion.

(define-prototype explosion (:parent rlx:=cell=)
  (name :initform "Explosion")
  (categories :initform '(:actor))
  (tile :initform "explosion")
  (speed :initform (make-stat :base 10))
  (damage-per-turn :initform 1)
  (clock :initform 3))

(define-method run explosion ()
  (if (zerop <clock>)
      [die self]
      (progn
	(decf <clock>)
	[expend-action-points self 10]
	(let* ((cells [cells-at *active-world* <row> <column>])
	       (x (1- (fill-pointer cells))))
	  (loop while (not (minusp x))
	       do (progn 
		    [>>damage (aref cells x) <damage-per-turn>]
		    (decf x)))))))

;;; Your explosive vapor trail. 

(defcell trail 
  (categories :initform '(:actor))
  (clock :initform 4))
  
(define-method initialize trail (&key direction clock)
  (setf <clock> clock)
  (setf <tile> (ecase direction
		 (:north "trail-north")
		 (:south "trail-south")
		 (:east "trail-east")
		 (:west "trail-west")
		 (:northeast "trail-northeast")
		 (:northwest "trail-northwest")
		 (:southeast "trail-southeast")
		 (:southwest "trail-southwest"))))

(define-method run trail ()
  [expend-action-points self 10]
  (decf <clock>)
  (when (zerop <clock>)
    [die self]))

(define-method step trail (stepper)
  [drop self (clone =explosion=)]	       
  [damage stepper 1])

;;; Death icon.

(define-prototype skull (:parent rlx:=cell=)
  (tile :initform "skull")
  (categories :initform '(:dead :player :actor))
  (action-points :initform 0))

(define-method forward skull (&rest args)
  (declare (ignore args))
  [queue>>narrateln :narrator "You are dead. You can't do anything!"])
  
(define-method move skull (&rest args)
  (declare (ignore args))
 [queue>>narrateln :narrator "You are dead. You can't do anything!"])

(define-method attack skull (&rest args)
  (declare (ignore args))
 [queue>>narrateln :narrator "You are dead. You can't do anything!"])

(define-method fire skull (&rest args)
  (declare (ignore args))
 [queue>>narrateln :narrator "You are dead. You can't do anything!"])

(define-method quit skull ()
  (rlx:quit :shutdown))

;;; Your ship.

(defcell ship 
  (tile :initform "player-ship-north-shield")
  (name :initform "Olvac 2")
  (speed :initform (make-stat :base 10))
  (strength :initform (make-stat :base 10))
  (defense :initform (make-stat :base 10))
  (hit-points :initform (make-stat :base 3 :min 0))
  (movement-cost :initform (make-stat :base 10))
  (max-items :initform (make-stat :base 2))
  (trail-length :initform 12)
  (stepping :initform t)
  (lives :initform 3)
  (score :initform 0)
  (categories :initform '(:actor :player :target :container :light-source))
  (equipment-slots :initform '(:gun :trail))
  (boost-clock :initform 0))

(define-method quit ship ()
  (rlx:quit :shutdown))

(define-method run ship ()
  [update *status*])	       

(define-method wait ship ()
  [stat-effect self :oxygen -1]
  [expend-action-points self <action-points>])

(define-method move ship (direction)
  [drop self (clone =trail= 
		    :direction direction 
		    :clock <trail-length>)]
  [parent>>move self direction])

(define-method update-tile ship ()
  (setf <tile> 
	(ecase [stat-value self :hit-points]
	  (3 "player-ship-north-shield")
	  (2 "player-ship-north")
	  (1 "player-ship-north-dying")
	  (0 "skull"))))
		 
(define-method damage ship (points)
  [parent>>damage self points]
  [update-tile self])

(define-method loadout ship ()
  [make-inventory self]
  [make-equipment self])

(define-method die ship ()
  (let ((skull (clone =skull=)))
    [drop-cell *active-world* skull <row> <column> :loadout t :no-collisions nil]
    (setf <action-points> 0)
    [add-category self :dead]
    [>>delete-from-world self]
    [>>narrateln :narrator "You die."]
    [set-player *active-world* skull]))

;;; An asteroid.

(defcell asteroid
  (categories :initform '(:actor :sticky))
  (hit-points :initform (make-stat :base 1 :min 0))
  (movement-cost :initform (make-stat :base 10))
  (stuck-to :initform nil)
  (direction :initform :north)
  (stepping :initform t))

(define-method is-stuck asteroid ()
  <stuck-to>)

(define-method die asteroid ()
  [>>say :narrator "You destroyed an asteroid!"]
  (incf (field-value :score [get-player *active-world*]) 80)
  (when <stuck-to>
    [unstick <stuck-to> self])
  [parent>>die self])

(define-method initialize asteroid (&key speed direction color)
  (setf <speed> (make-stat :base speed))
  (setf <direction> direction)
  (setf <tile>
	(ecase color
	  (:red "asteroid-red")
	  (:blue "asteroid-blue")
	  (:brown "asteroid-brown"))))

(define-method run asteroid ()
  (if (<= [stat-value self :hit-points] 0)
      [die self]
      ;; if free, float
      (if (and (not <stuck-to>)
	       (not [obstacle-in-direction-p *active-world* <row> <column> <direction>]))
	  [move self <direction>]
	  ;; otherwise bounce (when free)
	  (unless <stuck-to>
	    (setf <direction> (rlx:random-direction))))))

(define-method step asteroid (stepper)
  (when [in-category stepper :player]
    [damage stepper 1]
    [>>say :narrator "You took a hit!"])) 

;;; Polaris collects asteroids

(defcell polaris
  (tile :initform "polaris")
  (asteroids :initform '())
  (categories :initform '(:actor :obstacle))
  (direction :initform (rlx:random-direction)))

(define-method scan-neighborhood polaris ()
  (dolist (dir *compass-directions*)
    (multiple-value-bind (r c) (rlx:step-in-direction <row> <column> dir)
      (do-cells (cell [cells-at *active-world* r c])
	(when (and cell [in-category cell :sticky])
	  [stick self cell])))))

(define-method change-direction polaris (direction)
  (dolist (asteroid <asteroids>)
    (assert (clon:object-p asteroid))
    (setf (field-value :direction asteroid) direction))
  (setf <direction> direction))

(define-method move-as-group polaris (direction)
  ;; move self first so that nobody steps on us
  [move self direction]
  ;; now move the stuck asteroids
  (dolist (a <asteroids>)
    [move a direction]))

(define-method unstick polaris (asteroid)
  (setf <asteroids> (delete asteroid <asteroids>)))

(define-method run polaris ()
  [scan-neighborhood self]	       
  (let ((direction <direction>))	       
    (labels ((obstructed (asteroid)
	       [obstacle-in-direction-p *active-world*
					(field-value :row asteroid)
					(field-value :column asteroid)
					direction]))
      (let ((timeout 8)) 
	(loop while (and (plusp timeout)
			 (or (some #'obstructed <asteroids>)
			     (obstructed self)))
	   do [change-direction self (rlx:random-direction)]
	     (decf timeout))
	(unless (zerop timeout)
	  ;; it's safe. move as a group. 
	  [move-as-group self <direction>])))))
	      
(define-method stick polaris (asteroid)
  (when (and [in-category asteroid :sticky]
	     (not [is-stuck asteroid]))
    (setf (field-value :stuck-to asteroid) self)
    (setf (field-value :direction asteroid) <direction>)
    ;; put it back where it was
    [move asteroid (rlx:opposite-direction (field-value :direction asteroid))]
    (pushnew asteroid <asteroids>)))

;;; The endless void.

(define-prototype void-world (:parent rlx:=world=)
  (width :initform 80)
  (height :initform 46)
  (asteroid-count :initform 70)
  (polaris-count :initform 20)
  (ambient-light :initform :total))

(define-method generate void-world (&optional parameters)
  (declare (ignore parameters))
  (clon:with-field-values (height width) self
    (dotimes (i height)
      (dotimes (j width)
	[drop-cell self (clone =space=) i j]))
    (dotimes (i <asteroid-count>)
      [drop-cell self (clone =asteroid= 
			     :speed (+ 3 (random 7))
			     :direction (rlx:random-direction)
			     :color (nth (random 3)
					 '(:red :blue :brown)))
		 (random height) (random width)])
    (dotimes (i <polaris-count>)
      [drop-cell self (clone =polaris=)
		 (random height) (random width)])))

;;; Controlling the game.

(define-prototype blast-prompt (:parent rlx:=prompt=))

(defparameter *qwerty-keybindings*
  '(("Y" nil "move :northwest .")
    ("K" nil "move :north .")
    ("U" nil "move :northeast .")
    ("H" nil "move :west .")
    ("L" nil "move :east .")
    ("B" nil "move :southwest .")
    ("J" nil "move :south .")
    ("N" nil "move :southeast .")
    ;;
    ("Y" (:alt) "attack :northwest .")
    ("K" (:alt) "attack :north .")
    ("U" (:alt) "attack :northeast .")
    ("H" (:alt) "attack :west .")
    ("L" (:alt) "attack :east .")
    ("B" (:alt) "attack :southwest .")
    ("J" (:alt) "attack :south .")
    ("N" (:alt) "attack :southeast .")
    ;;
    ("Y" (:meta) "attack :northwest .")
    ("K" (:meta) "attack :north .")
    ("U" (:meta) "attack :northeast .")
    ("H" (:meta) "attack :west .")
    ("L" (:meta) "attack :east .")
    ("B" (:meta) "attack :southwest .")
    ("J" (:meta) "attack :south .")
    ("N" (:meta) "attack :southeast .")
    ;;
    ("Y" (:control) "fire :northwest .")
    ("K" (:control) "fire :north .")
    ("U" (:control) "fire :northeast .")
    ("H" (:control) "fire :west .")
    ("L" (:control) "fire :east .")
    ("B" (:control) "fire :southwest .")
    ("J" (:control) "fire :south .")
    ("N" (:control) "fire :southeast .")
    ;;
    ("W" nil "wait .")
    ("0" nil "equip 0 .")
    ("1" nil "equip 1 .")
    ("0" (:control) "drop-item 0 .")
    ("1" (:control) "drop-item 1 .")
    ("2" nil "activate-equipment :belt .")
    ("Q" (:control) "quit .")))

(defparameter *alternate-qwerty-keybindings*
  '(("Q" nil "move :northwest .")
    ("W" nil "move :north .")
    ("E" nil "move :northeast .")
    ("A" nil "move :west .")
    ("D" nil "move :east .")
    ("Z" nil "move :southwest .")
    ("X" nil "move :south .")
    ("C" nil "move :southeast .")
    ;;
    ("Q" (:alt) "attack :northwest .")
    ("W" (:alt) "attack :north .")
    ("E" (:alt) "attack :northeast .")
    ("A" (:alt) "attack :west .")
    ("D" (:alt) "attack :east .")
    ("Z" (:alt) "attack :southwest .")
    ("X" (:alt) "attack :south .")
    ("C" (:alt) "attack :southeast .")
    ;;
    ("Q" (:meta) "attack :northwest .")
    ("W" (:meta) "attack :north .")
    ("E" (:meta) "attack :northeast .")
    ("A" (:meta) "attack :west .")
    ("D" (:meta) "attack :east .")
    ("Z" (:meta) "attack :southwest .")
    ("X" (:meta) "attack :south .")
    ("C" (:meta) "attack :southeast .")
    ;;
    ("Q" (:control) "fire :northwest .")
    ("W" (:control) "fire :north .")
    ("E" (:control) "fire :northeast .")
    ("A" (:control) "fire :west .")
    ("D" (:control) "fire :east .")
    ("Z" (:control) "fire :southwest .")
    ("X" (:control) "fire :south .")
    ("C" (:control) "fire :southeast .")
    ;;
    ("S" nil "wait .")
    ("0" nil "equip 0 .")
    ("1" nil "equip 1 .")
    ("0" (:control) "drop-item 0 .")
    ("1" (:control) "drop-item 1 .")
    ("2" nil "activate-equipment :belt .")
    ("Q" (:control) "quit .")))

;; g c r
;;  \|/
;; h-.-n
;;  /|\ 
;; m w v

(defparameter *dvorak-keybindings*
  '(("G" nil "move :northwest .")
    ("C" nil "move :north .")
    ("R" nil "move :northeast .")
    ("H" nil "move :west .")
    ("N" nil "move :east .")
    ("M" nil "move :southwest .")
    ("W" nil "move :south .")
    ("V" nil "move :southeast .")
    ;;
    ("G" (:alt) "attack :northwest .")
    ("C" (:alt) "attack :north .")
    ("R" (:alt) "attack :northeast .")
    ("H" (:alt) "attack :west .")
    ("N" (:alt) "attack :east .")
    ("M" (:alt) "attack :southwest .")
    ("W" (:alt) "attack :south .")
    ("V" (:alt) "attack :southeast .")
    ;;
    ("G" (:meta) "attack :northwest .")
    ("C" (:meta) "attack :north .")
    ("R" (:meta) "attack :northeast .")
    ("H" (:meta) "attack :west .")
    ("N" (:meta) "attack :east .")
    ("M" (:meta) "attack :southwest .")
    ("W" (:meta) "attack :south .")
    ("V" (:meta) "attack :southeast .")
    ;;
    ("G" (:control) "fire :northwest .")
    ("C" (:control) "fire :north .")
    ("R" (:control) "fire :northeast .")
    ("H" (:control) "fire :west .")
    ("N" (:control) "fire :east .")
    ("M" (:control) "fire :southwest .")
    ("W" (:control) "fire :south .")
    ("V" (:control) "fire :southeast .")
    ;;
    ("S" nil "wait .")
    ("0" nil "equip 0 .")
    ("1" nil "equip 1 .")
    ("0" (:control) "drop-item 0 .")
    ("1" (:control) "drop-item 1 .")
    ("2" nil "activate-equipment :belt .")
    ("Q" (:control) "quit .")))

(define-method install-keybindings blast-prompt ()
  (let ((keys (ecase rlx:*user-keyboard-layout* 
		(:qwerty *qwerty-keybindings*)
		(:alternate-qwerty *alternate-qwerty-keybindings*)
		(:dvorak *dvorak-keybindings*))))
    (dolist (k keys)
      (apply #'bind-key-to-prompt-insertion self k)))
  [define-key self nil '(:timer) (lambda ()
				   [run-cpu-phase *active-world* :timer])])

;;; A shield status and score widget.

(define-prototype status (:parent rlx:=formatter=)
  (character :documentation "The character cell whose status is shown."))

(define-method set-character status (char)
  (setf <character> char))

(define-method update status ()
  [delete-all-lines self]
  (let* ((char <character>)
	 (hits [stat-value char :hit-points]))
    [print self " HITS: "]
    (dotimes (i 3)
      [print self "[]" 
	     :foreground ".yellow"
	     :background (if (< i hits)
			     ".red"
			     ".gray20")]
      [space self])
    [print self " --- SCORE: "]
    [print self (format nil "~D" (field-value :score char))]))

(defvar *status*)

;;; Main program. 

(defun blast ()
  (setf clon:*send-parent-depth* 2)
  (rlx:set-screen-height 600)
  (rlx:set-screen-width 800)
;;  (rlx:set-frame-rate 30)
  (rlx:set-timer-interval 10)
  (rlx:enable-timer)
  (rlx:enable-held-keys 0 15)
  (let* ((prompt (clone =blast-prompt=))
	 (world (clone =void-world=))
	 (narrator (clone =narrator=))
	 (status (clone =status=))
	 (player (clone =ship=))
	 (viewport (clone =viewport=)))
    (setf *active-world* world)
    ;;
    [resize prompt :height 20 :width 100]
    [move prompt :x 0 :y 0]
    [hide prompt]
    [install-keybindings prompt]
    [set-receiver prompt world]
    ;;
    [create-default-grid world]
    [generate world]
    [set-player world player]
    [drop-cell world player 25 25 :loadout t]
    ;;
    [resize narrator :height 80 :width 800]
    [move narrator :x 0 :y 520]
    [set-narrator world narrator]
    [set-verbosity narrator 0]
    ;;
    [resize status :height 30 :width 700]
    [move status :x 10 :y 10]
    [set-character status player]
    (setf *status* status)
    [update status]
   ;;
    (setf (clon:field-value :tile-size viewport) 10)
    [set-world viewport world]
    [resize viewport :height 460 :width 800]
    [move viewport :x 0 :y 60]
    [set-origin viewport :x 0 :y 0 :height 46 :width 80]
    [adjust viewport]
    ;;
    [start world]
    ;;
	 ;;    (play-music "void" :loop t)
    
    (install-widgets prompt status viewport narrator)))

(blast)
