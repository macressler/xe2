;;; xiobeat --- freestyle video dance engine

;;       _       _                _   
;; __  _(_) ___ | |__   ___  __ _| |_ 
;; \ \/ / |/ _ \| '_ \ / _ \/ _` | __|
;;  >  <| | (_) | |_) |  __/ (_| | |_ 
;; /_/\_\_|\___/|_.__/ \___|\__,_|\__|
;;                                 

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

;;; Overview

;; XIOBEAT is a "video dance engine", at once both a rhythm game and a
;; music creation tool, and suitable for solo freestyling and group
;; performance. Between one and four players may use the numeric
;; keypad, USB dance pads, or virtually any other standard USB
;; joystick for controlling and performing sounds, and multiple
;; simultaneous input devices are supported. XIOBEAT is fully
;; extensible in Common Lisp.

;;; Modes of play 

;; Stomp your feet to create sound in Freestyle mode using any Ogg
;; Vorbis or XM (Extended Module) format song as a backing track, and
;; load any WAV sample into each of the 8 dance pad buttons to be
;; triggered by foot stomps. Using XM songs as a backing track allows
;; you to alter the music by choosing different patterns to loop from
;; the XM. For more about the Extended Module song standard, see 
;; http://en.wikipedia.org/wiki/XM_(file_format)

;; With the more sophisticated Track mode you can create, improvise,
;; or follow dance routines using XIOBEAT's dance gesture input
;; system.

;; Puzzle mode is as yet undescribed, but details will follow
;; soon---try to imagine falling blocks with dance phrases on them.

;;; Status

;; Stomp mode works, there is a video available here:
;; http://www.youtube.com/watch?v=q2b9dKlMaw4
;; Track mode is being implemented now.

;; The first beta release of XIOBEAT is slated for late November 2010
;; and will include support for USB dance pads and USB
;; cameras. (Playstation 2 compatible dance pads can be used with a
;; compatible USB adapter.) XIOBEAT is written in Common Lisp and will
;; be free software. See http://dtogameblog.blogspot.com for updates.

;; Future plans include USB camera support (this should be available
;; in the beta), USB microphone support, and live video effects.
;; Remix and share your creations, record your performances, etc.

;;; Packaging

(defpackage :xiobeat
  (:documentation "XIOBEAT is a freestyle video dance engine written in Common Lisp.")
  (:use :xe2 :common-lisp)
  (:export xiobeat))

(in-package :xiobeat)

;;; Dance pad layout

;;    The diagram below gives the intended dance pad layout for
;;    XIOBEAT. Many generic USB and/or game console compatible dance
;;    pads are marked this way. (Some pads are printed with "back"
;;    instead of "select").
;;
;;    select  start
;;    -------------
;;    |B  |^  |A  |
;;    |___|___|___|
;;    |<  |   |>  |
;;    |___|___|___|
;;    |Y  |v  |X  |
;;    |   |   |	  |
;;    -------------
;;
;;    A 10-button dance pad is required (i.e. all four corners must be
;;    buttons, as well as the orthogonal arrows and select/start.)
;;    Konami's soft home pads lack the lower corner buttons, so they
;;    won't be usable even with a USB adapter. Most generic dance pads
;;    will work just fine. (Pads for Pump It Up might not work.)

;;; Dance gesture input

;; A dance phrase is a sequence of dance pad button presses. Each
;; phrase consists of an optional command prefix (one of the corner
;; buttons, with each one controlling a separate system function)
;; followed by zero or more of the dance arrows (up, down, left, or
;; right.) A phrase is terminated by a "period" (represented by a
;; circle) which can be entered using any of the four corner
;; buttons. The period immediately executes the command (if any). You
;; can enter each of the four system menus by double stomping a corner
;; arrow, i.e. a command phrase with no arrows in it.

(defparameter *dance-arrows* '(:left :right :up :down))

(defparameter *corner-buttons* '(:a :b :x :y))

(defparameter *function-buttons* '(:select :start))

(defparameter *dance-phrase-symbols*
  (append *dance-arrows* *corner-buttons* *function-buttons*))

;; Track-specific phrases have no command prefix. These trigger sounds
;; or other events.

(defun is-command-phrase (phrase)
  (member (first phrase) *corner-buttons*))

;; Configure the engine so that it translates dance pad button presses
;; into standard XE2 joystick events.

(setf xe2:*joystick-button-symbols* *dance-phrase-symbols*)

;;; Input device configuration

;; The dance pad layout shown above is also available on the numeric
;; keypad.

(defparameter *basic-keybindings* 
  '(("KP8" nil "up .")
    ("KP4" nil "left .")
    ("KP6" nil "right .")
    ("KP2" nil "down .")
    ("KP1" nil "button-y .")
    ("KP3" nil "button-x .")
    ("KP7" nil "button-b .")
    ("KP9" nil "button-a .")
    ("KP-ENTER" nil "start .")
    ("KP0" nil "select .")
    ("JOYSTICK" (:up :button-down) "up .")
    ("JOYSTICK" (:left :button-down) "left .")
    ("JOYSTICK" (:right :button-down) "right .")
    ("JOYSTICK" (:down :button-down) "down .")
    ("JOYSTICK" (:y :button-down) "button-y .")
    ("JOYSTICK" (:x :button-down) "button-x .")
    ("JOYSTICK" (:b :button-down) "button-b .")
    ("JOYSTICK" (:a :button-down) "button-a .")
    ("JOYSTICK" (:start :button-down) "start .")
    ("JOYSTICK" (:select :button-down) "select .")))

;; Other commands must be configured per layout, currently there are
;; none.

(defparameter *qwerty-keybindings*
  (append *basic-keybindings* nil))

;; Including configurations for common dance pads is a good idea.
;; Eventually we need a real configuration menu (for the beta.)

(defparameter *energy-dance-pad-mapping*
  '((12 . :up)
    (15 . :left)
    (13 . :right)
    (14 . :down)
    (0 . :y)
    (3 . :x)
    (2 . :b)
    (1 . :a)
    (8 . :select)
    (9 . :start)))

(setf xe2:*joystick-mapping* *energy-dance-pad-mapping*)

(defun get-button-index (arrow)
  (first (find arrow *joystick-mapping* :key #'cdr)))

;;; Displaying arrows as icons

(defparameter *icon-height* 64)
(defparameter *icon-width* 64)

(defparameter *icon-images* 
  '(:up "up" :left "left" :right "right" :down "down"
    :a "a" :x "x" :y "y" :b "b" :period "period"))

(defun arrow-icon-image (arrow)
  (getf *icon-images* arrow))

;;; Modes

(defparameter *modes* '(:stomp :track :puzzle))

(defvar *mode* :stomp)

;;; Initial window size

(defparameter *xiobeat-window-width* 800)
(defparameter *xiobeat-window-height* 600)

;;; System status display widget

(defparameter *margin-size* 16)

(define-prototype status (:parent xe2:=widget=)
  (height :initform *xiobeat-window-height*)
  (width :initform (+  (* 3 *icon-width*)
			(* 2 *margin-size*))))

(define-method render status ()
  (with-fields (image width height) self
    (draw-box 0 0 width height :stroke-color ".gray50" :color ".gray50")
    (let ((x 0) 
	  (y 0))
      (dolist (row '((:b :up :a) 
		     (:left nil :right) 
		     (:y :down :x)))
	(setf x 0)
	(dolist (button row)
	  (when button
	    (let ((icon (arrow-icon-image button))
		  (index (get-button-index button)))
	      ;; draw a rectangle if the button is pressed
	      (when (plusp (poll-joystick-button index))
		(draw-box x y *icon-height* *icon-height*
			  :stroke-color ".dark orange" :color ".dark orange" :destination image))
	      ;; draw the icon above the rectangle, if any
	      (draw-resource-image icon x y :destination image)))
	  (incf x *icon-width*))
	(incf y *icon-height*)))))

;;; Stomp mode is the basic functionality the other modes build on.

(define-prototype stomper (:parent xe2:=prompt=))
  
(define-method install-keybindings stomper ()
  (dolist (k *basic-keybindings*)
      (apply #'bind-key-to-prompt-insertion self k)))

(define-method select stomper ()
  (play-music "xiophant" :loop t))

(define-method start stomper ()
  (play-music "electron2" :loop t))

(define-method button-y stomper ()
  (play-sample "hit2"))

(define-method button-x stomper ()
  (play-sample "scratch"))

(define-method button-b stomper ()
  (play-sample "snare2"))

(define-method button-a stomper ()
  (play-sample "ting"))

(define-method up stomper ()
  (play-sample "bip"))

(define-method down stomper ())

(define-method left stomper ()
  (play-sample "snare2"))

(define-method right stomper ()
  (play-sample "snare3"))

(setf xe2:*dt* 20)

(defun xiobeat ()
  (xe2:message "Initializing Xiobeat...")
  (setf xe2:*window-title* "Xiobeat")
  (setf xe2:*output-chunksize* 128)
  (xe2:set-screen-height *xiobeat-window-height*)
  (xe2:set-screen-width *xiobeat-window-width*)
  (let* ((prompt (clone =stomper=))
	 (status (clone =status=)))
    [resize status :height 600 :width 200]
    [move status :x 0 :y 0]
    [resize prompt :height 20 :width 100]
    [move prompt :x 200 :y 0]
    [show prompt]
    [set-receiver prompt prompt]
    [install-keybindings prompt]
    (xe2:reset-joystick)
    (set-music-volume 255)
    (xe2:install-widgets status prompt)
    (xe2:enable-classic-key-repeat 100 100)))

(xiobeat)
