;;; example.lisp --- simple xe2 example game

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

;; See also http://dto.github.com/notebook/developers-guide.html

;;; Packaging

(defpackage :superxong
  (:documentation "Space game.")
  (:use :xe2 :common-lisp)
  (:export superxong))

(in-package :superxong)

;;; Walls

(defcell wall 
  (tile :initform "wall")
  (categories :initform '(:obstacle :opaque :exclusive :wall))
  (hit-points :initform (make-stat :base 1 :min 0))) 
  
(define-method hit wall ()
  (setf <tile> "debris")
  [delete-category self :obstacle]
  [delete-category self :wall])

;;; The bouncing ball

(defcell ball 
  (tile :initform "ball")
  (categories :initform '(:actor))
  (direction :initform (xe2:random-direction))
  (hit-points :initform (make-stat :base 5 :min 0)))

(define-method serve ball (direction)
  [play-sample self "serve"]
  (setf <direction> direction))

(define-method run ball ()
  (when (eq <direction> :here) (setf <direction> (random-direction)))
  (clon:with-fields (direction row column) self
    (multiple-value-bind (r c) (xe2:step-in-direction row column direction)
      (if [obstacle-at-p *world* r c]
	  (progn 
	    ;; is it a wall or character? then hit it
	    (let ((object [category-at-p *world* r c '(:wall :person)]))
	      (when object
		[hit object]
		[damage self 1]))
	    ;; bounce
	    [play-sample self "bip"]
	    (setf direction (xe2:random-direction)))
	  ;; move along
	  [move self direction]))))

;;; The player

(defcell player 
  (tile :initform "player")
  (name :initform "Player")
  (speed :initform (make-stat :base 10 :min 0 :max 10))
  (movement-cost :initform (make-stat :base 10))
  (stepping :initform t)
  (categories :initform '(:actor :player :obstacle)))

(define-method quit player ()
  (xe2:quit :shutdown))

(define-method run player ()
  ;; if you are in category :actor, this is called every turn
  nil)

(define-prototype room-prompt (:parent xe2:=prompt=))

(defparameter *numpad-keybindings* 
  '(("KP7" nil "move :northwest .")
    ("KP8" nil "move :north .")
    ("KP9" nil "move :northeast .")
    ("KP4" nil "move :west .")
    ("KP6" nil "move :east .")
    ("KP1" nil "move :southwest .")
    ("KP2" nil "move :south .")
    ("KP3" nil "move :southeast .")
    ;;
    ("KP7" (:control) "serve-ball :northwest .")
    ("KP8" (:control) "serve-ball :north .")
    ("KP9" (:control) "serve-ball :northeast .")
    ("KP4" (:control) "serve-ball :west .")
    ("KP6" (:control) "serve-ball :east .")
    ("KP1" (:control) "serve-ball :southwest .")
    ("KP2" (:control) "serve-ball :south .")
    ("KP3" (:control) "serve-ball :southeast .")))

(defparameter *qwerty-keybindings*
  (append *numpad-keybindings*
	  '(("Y" nil "move :northwest .")
	    ("K" nil "move :north .")
	    ("U" nil "move :northeast .")
	    ("H" nil "move :west .")
	    ("L" nil "move :east .")
	    ("B" nil "move :southwest .")
	    ("J" nil "move :south .")
	    ("N" nil "move :southeast .")
	    ;;
	    ("Y" (:control) "serve-ball :northwest .")
	    ("K" (:control) "serve-ball :north .")
	    ("U" (:control) "serve-ball :northeast .")
	    ("H" (:control) "serve-ball :west .")
	    ("L" (:control) "serve-ball :east .")
	    ("B" (:control) "serve-ball :southwest .")
	    ("J" (:control) "serve-ball :south .")
	    ("N" (:control) "serve-ball :southeast .")
	    ;;
	    ("Q" (:control) "quit ."))))

(define-method install-keybindings room-prompt ()
  (dolist (k (append *numpad-keybindings* *qwerty-keybindings*))
    (apply #'bind-key-to-prompt-insertion self k))
  ;; we also want to respond to timer events. this is how. 
  [define-key self nil '(:timer) (lambda ()
				   [run-cpu-phase *world* :timer])])

;;; Main program. 

(defparameter *room-window-width* 800)
(defparameter *room-window-height* 600)

(defun superxong ()
  (xe2:message "Initializing SUPER XONG...")
  (clon:initialize)
  (xe2:set-screen-height *room-window-height*)
  (xe2:set-screen-width *room-window-width*)
  (let* ((prompt (clone =room-prompt=))
	 (universe (clone =universe=))
	 (narrator (clone =narrator=))
	 (player (clone =player=))
	 (viewport (clone =viewport=)))
    ;;
    [resize prompt :height 20 :width 100]
    [move prompt :x 0 :y 0]
    [hide prompt]
    [install-keybindings prompt]
    ;;
    [resize narrator :height 80 :width *room-window-width*]
    [move narrator :x 0 :y (- *room-window-height* 80)]
    [set-verbosity narrator 0]
    ;;
    [play universe
	  :address '(=room=)
	  :player player
	  :narrator narrator
	  :prompt prompt
	  :viewport viewport]
    [set-tile-size viewport 32]
    [resize viewport :height 470 :width *room-window-width*]
    [move viewport :x 0 :y 0]
    [set-origin viewport :x 0 :y 0 
		:height (truncate (/ (- *room-window-height* 130) 32))
		:width (truncate (/ *room-window-width* 32))]
    [adjust viewport] 
    [narrateln narrator "You are the green guy."]
    [narrateln narrator "Use the numpad or nethack keys to move; Control-direction to fire."]
   ;;
    (xe2:install-widgets prompt viewport narrator)))

(superxong)
