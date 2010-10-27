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

;; Stomp your feet to create sound in Stomp mode using any Ogg
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

(setf xe2:*resizable* t)
(setf xe2:*dt* 20)

(defparameter *window-width* 1280)
(defparameter *window-height* 720)
(defparameter *prompt-height* 20)
(defparameter *quickhelp-height* 140)
(defparameter *quickhelp-width* 390)
(defparameter *quickhelp-spacer* 0)
(defparameter *terminal-height* 100)
(defparameter *pager-height* 20)
(defparameter *sidebar-width* 200)
(defparameter *status-height* (* 32 3))
(defparameter *modes* '(:stomp :track :puzzle))
(defvar *mode* :stomp)
(defvar *form*)
(defvar *terminal*)
(defvar *prompt*)
(defvar *pager*)
(defvar *frame*)
(defvar *track*)
(defvar *engine*)

(defun handle-xiobeat-command (command)
  (assert (stringp command))
  [insert *prompt* command]
  [execute *prompt*])

(setf xe2:*form-command-handler-function* #'handle-xiobeat-command)

(define-prototype xiobeat-frame (:parent xe2:=split=))

(defparameter *qwerty-keybindings*
  '(;; arrow key cursor movement
    ("UP" nil :move-cursor-up)
    ("DOWN" nil :move-cursor-down)
    ("LEFT" nil :move-cursor-left)
    ("RIGHT" nil :move-cursor-right)
    ;; emacs-style cursor movement
    ("A" (:control) :move-beginning-of-line)
    ("E" (:control) :move-end-of-line)
    ("F" (:control) :move-cursor-right)
    ("B" (:control) :move-cursor-left)
    ;; editing keys
    ("HOME" nil :move-beginning-of-line)
    ("END" nil :move-end-of-line)
    ("PAGEUP" nil :move-beginning-of-column)
    ("PAGEDOWN" nil :move-end-of-column)
    ;; switching windows
    ("TAB" nil :switch-pages)
    ("TAB" (:control) :switch-pages)
    ("LEFT" (:control) :left-page)
    ("RIGHT" (:control) :right-page)
    ;; dropping commonly-used cells
    ("1" (:control) :drop-data-cell)
    ("2" (:control) :drop-command-cell)
    ;; performing operations like clone, erase
    ("UP" (:control) :apply-right)
    ("DOWN" (:control) :apply-left)
    ("LEFTBRACKET" nil :apply-left)
    ("RIGHTBRACKET" nil :apply-right)
    ;; marking and stuff
    ("SPACE" (:control) :set-mark)
    ("SPACE" (:alt) :clear-mark)
    ("SPACE" (:meta) :clear-mark)
    ;; numeric keypad
    ("KP8" nil :move-cursor-up)
    ("KP2" nil :move-cursor-down)
    ("KP4" nil :move-cursor-left)
    ("KP6" nil :move-cursor-right)
    ("KP8" (:control) :apply-right)
    ("KP2" (:control) :apply-left)
    ("KP4" (:control) :left-page)
    ("KP6" (:control) :right-page)
    ;; entering data and confirm/cancel
    ("RETURN" nil :enter)
    ("RETURN" (:control) :exit) ;; see also handle-key
    ("ESCAPE" nil :cancel)
    ("G" (:control) :cancel)
    ;; view mode
    ("F9" nil :tile-view)
    ("F10" nil :label-view)
    ;; other
    ("X" (:control) :goto-prompt)
    ("X" (:alt) :goto-prompt)
    ("X" (:meta) :goto-prompt)
    ("T" (:control) :next-tool)))

(define-method install-keybindings xiobeat-frame ()
  (dolist (binding (case *user-keyboard-layout*
		     (:qwerty *qwerty-keybindings*)
		     (otherwise *qwerty-keybindings*)))
    (/generic-keybind self binding)))

(define-method left-form xiobeat-frame ()
  (nth 0 <children>))

(define-method right-form xiobeat-frame ()
  (nth 1 <children>))

(define-method other-form xiobeat-frame ()
  (ecase <focus>
    (0 (/right-form self))
    (1 (/left-form self))))

(define-method left-world xiobeat-frame ()
  (field-value :world (/left-form self)))

(define-method right-world xiobeat-frame ()
  (field-value :world (/right-form self)))

(define-method selected-form xiobeat-frame ()
  (nth <focus> <children>))

(define-method left-selected-data xiobeat-frame ()
  (/get-selected-cell-data (/left-form self)))

(define-method right-selected-data xiobeat-frame ()
  (/get-selected-cell-data (/right-form self)))

(define-method focus-left xiobeat-frame ()
  (/focus (/left-form self))
  (/unfocus (/right-form self)))

(define-method focus-right xiobeat-frame ()
  (/focus (/right-form self))
  (/unfocus (/left-form self)))

(define-method refocus xiobeat-frame ()
  (ecase <focus>
    (0 (/focus-left self))
    (1 (/focus-right self))))

(define-method left-page xiobeat-frame ()
  "Select the left spreadsheet page."
  (/say self "Selecting left page.")
  (/focus-left self)
  (setf <focus> 0))

(define-method right-page xiobeat-frame ()
  "Select the right spreadsheet page."
  (/say self "Selecting right page.")
  (/focus-right self)
  (setf <focus> 1))

(define-method switch-pages xiobeat-frame ()
  (let ((newpos (mod (1+ <focus>) (length <children>))))
    (setf <focus> newpos)
    (ecase newpos
      (0 (/left-page self))
      (1 (/right-page self)))))

(define-method apply-left xiobeat-frame ()
  "Move data LEFTWARD from right page to left page, applying current
left side tool to the right side data."
  (let* ((form (/left-form self))
	 (tool (field-value :tool form))
	 (data (/right-selected-data self)))
    (/say self (format nil "Applying LEFT tool ~S to data ~S in LEFT form." tool data))
    (/apply-tool form data)))

(define-method apply-right xiobeat-frame ()
  "Move data RIGHTWARD from left page to right page, applying current
right side tool to the left side data."
  (let* ((form (/right-form self))
	 (tool (field-value :tool form))
	 (data (/left-selected-data self)))
    (/say self (format nil "Applying RIGHT tool ~S to data ~S in RIGHT form." tool data))
    (/apply-tool form data)))

(define-method paste xiobeat-frame (&optional page)
  (let ((source (if page 
		    (find-page page)
		    (field-value :world (/other-form self))))
	(destination (field-value :world (/selected-form self))))
    (multiple-value-bind (top left bottom right) (/mark-region (/selected-form self))
      (multiple-value-bind (top0 left0 bottom0 right0) (/mark-region (/other-form self))
	(let ((source-height (field-value :height source))
	      (source-width (field-value :width source)))
	  (with-fields (cursor-row cursor-column) (/selected-form self)
	    (let* ((height (or (when top (- bottom top))
			       (when top0 (- bottom0 top0))
			       source-height))
		   (width (or (when left (- right left))
			      (when left0 (- right0 left0))
			      source-width))
		   (r0 (or top cursor-row))
		   (c0 (or left cursor-column))
		   (r1 (or bottom (- height 1)))
		   (c1 (or right (- width 1)))
		   (sr (or top0 0))
		   (sc (or left0 0)))
	      (/paste-region destination source r0 c0 sr sc height width))))))))
  
(define-method commands xiobeat-frame ()
  "Syntax: command-name arg1 arg2 ...
Available commands: HELP EVAL SWITCH-PAGES LEFT-PAGE RIGHT-PAGE
NEXT-TOOL SET-TOOL APPLY-LEFT APPLY-RIGHT VISIT SELECT SAVE-ALL
SAVE-MODULE LOAD-MODULE TILE-VIEW LABEL-VIEW QUIT VISIT APPLY-TOOL
CLONE ERASE CREATE-WORLD PASTE QUIT ENTER EXIT"
 nil)

;;; Main command prompt

(define-prototype xiobeat-prompt (:parent xe2:=prompt=))

(define-method say xiobeat-prompt (&rest args)
  (apply #'send nil :say *terminal* args))

(define-method goto xiobeat-prompt ()
  (/unfocus (/left-form *frame*))
  (/unfocus (/right-form *frame*))
  (setf <mode> :direct))

(define-method do-after-execute xiobeat-prompt ()
  (/clear-line self)  
  (setf <mode> :forward))

(define-method exit xiobeat-prompt ()
  (/parent>>exit self)
  (/refocus *frame*))

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
  '(("Q" (:control) "quit .")
    ("KP8" nil "up .")
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

;; (defparameter *qwerty-keybindings*
;;   (append *basic-keybindings* nil))

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

(defparameter *icon-height* 32)
(defparameter *icon-width* 32)

;; (defparameter *icon-images* 
;;   '(:up "up" :left "left" :right "right" :down "down"
;;     :a "a" :x "x" :y "y" :b "b" :period "period" :prompt "prompt"))

(defparameter *icon-images* 
  '(:up "up-medium" :left "left-medium" :right "right-medium" :down "down-medium"
    :a "a-medium" :x "x-medium" :y "y-medium" :b "b-medium" 
    :period "period-medium" :prompt "prompt-medium"))

(defun arrow-icon-image (arrow)
  (getf *icon-images* arrow))

(defun arrow-icon-formatted-string (arrow)
  (list nil :image (arrow-icon-image arrow)))

;;; System status display widget

(defparameter *margin-size* 16)

(define-prototype status (:parent xe2:=widget=)
  (height :initform *window-height*)
  (width :initform (+  (* 3 *icon-width*)
			(* 2 *margin-size*))))

(define-method render status ()
  (with-fields (image width height) self
    (draw-box 0 0 width height :stroke-color ".black" :color ".black" 
	      :destination image)
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
	      ;; TODO nice shaded glowing panels
	      (when (plusp (poll-joystick-button index))
		(draw-box x y *icon-height* *icon-height*
			  :stroke-color ".dark orange" :color ".dark orange" :destination image))
	      ;; draw the icon above the rectangle, if any
	      (draw-resource-image icon x y :destination image)))
	  (incf x *icon-width*))
	(incf y *icon-height*)))))

;;; Arrow phrase command display/processor

(defvar *commander* nil)

(define-prototype commander (:parent xe2:=formatter=)
  (display-current-line :initform t))

(define-method insert commander (arrow)
  (/print-formatted-string self (arrow-icon-formatted-string arrow)))

;;; Stomp mode is the basic functionality the other modes build on.

(define-prototype stomper (:parent xe2:=prompt=))
  
(define-method install-keybindings stomper ()
  (dolist (k *basic-keybindings*)
      (apply #'bind-key-to-prompt-insertion self k)))

(define-method quit stomper ()
  (xe2:quit))

(define-method select stomper ()
  (play-music "electron" :loop t))

(define-method start stomper ()
  (/insert *commander* :period)
  (/newline *commander*)
  (/insert *commander* :prompt))

(define-method button-y stomper ()
  (/insert *commander* :y))

(define-method button-x stomper ()
;;  (/insert *commander* :x)
;  (play-sample "scratch")
  (/start self))

(define-method button-b stomper ()
;;  (/insert *commander* :b)
  (/start self))

(define-method button-a stomper ()
  (/insert *commander* :a)
  (play-sample "bass"))

(define-method up stomper ()
  (/insert *commander* :up)
  (play-sample "ting"))

(define-method down stomper ()
  (play-sample "scratch")
  (/insert *commander* :down))

(define-method left stomper ()
  (/insert *commander* :left)
  (play-sample "snare3"))

(define-method right stomper ()
  (/insert *commander* :right)
  (play-sample "cymb"))

;; (defun xiobeat ()
;;   (xe2:message "Initializing Xiobeat...")
;;   (setf xe2:*window-title* "Xiobeat")
;;   (setf xe2:*output-chunksize* 128)
;;   (xe2:set-screen-height *xiobeat-window-height*)
;;   (xe2:set-screen-width *xiobeat-window-width*)
;;   (let* ((engine (clone =stomper=))
;; 	 (status (clone =status=))
;; 	 (commander (clone =commander=)))
;;     [resize status :height 600 :width 200]
;;     [move status :x 0 :y 0]
;;     [resize engine :height 20 :width 100]
;;     [move engine :x 200 :y 0]
;;     [show engine]
;;     [resize commander :height 550 :width 580]
;;     [move commander :x 200 :y 25]
;;     [show commander]
;;     (setf *commander* commander)
;;     [set-receiver engine engine]
;;     [install-keybindings engine]
;;     (xe2:reset-joystick)
;;     (set-music-volume 255)
;;     (xe2:install-widgets status engine commander)
;;     (xe2:enable-classic-key-repeat 100 100)))

(define-prototype help-prompt (:parent =prompt=)
  (default-keybindings :initform '(("N" nil "page-down .")
				   ("P" nil "page-up ."))))

(define-prototype help-textbox (:parent =textbox=))

(define-method render help-textbox ()
  (/parent>>render self)
  (/message *pager* 
	   (list (format nil " --- Showing lines ~A-~A of ~A. Use PAGE UP and PAGE DOWN to scroll the text." 
			 <point-row> (+ <point-row> <max-displayed-rows>) (length <buffer>)))))

(add-hook '*after-load-module-hook* (lambda ()
				      (/message *pager* (list (format nil "  CURRENT MODULE: ~S." *module*)))
				      (when (string= *module* "xiobeat")
					(/visit *form* "FrontPage"))))

(defun xiobeat ()
  (xe2:message "Initializing XIOBEAT...")
  (setf xe2:*window-title* "XIOBEAT")
  (clon:initialize)
  (xe2:set-screen-height *window-height*)
  (xe2:set-screen-width *window-width*)
  (let* ((prompt (clone =xiobeat-prompt=))
	 (help (clone =help-textbox=))
	 (help-prompt (clone =help-prompt=))
	 (quickhelp (clone =formatter=))
	 (form (clone =form=))
	 (form2 (clone =form= "*scratch*"))
	 (engine (clone =stomper=))
 	 (status (clone =status=))
 	 (commander (clone =commander=))
	 (terminal (clone =narrator=))
	 (frame (clone =xiobeat-frame=))
	 (stack (clone =stack=)))
    ;;
    (setf *form* form)
    (setf *engine* engine)
    (setf *prompt* prompt)
    (setf *terminal* terminal)
    (setf *frame* frame)
    (labels ((resize-widgets ()
	       (/say terminal "Resizing to ~S" (list :width *screen-width* :height *screen-height*))
	       (/resize prompt :height *prompt-height* :width *screen-width*)
	       (/resize form :height (- *screen-height* *terminal-height* 
					*status-height*
				       *prompt-height* *pager-height*) 
		       :width (- *screen-width* *sidebar-width* 2))
	       (/resize form2 :height (- *screen-height* 
					 *status-height* 
					 *terminal-height* 
					 *prompt-height* 
					 *pager-height*) 
			:width (- *sidebar-width* 2))
	       (/resize-to-scroll help :height (- *screen-height* *pager-height*) :width *screen-width*)
	       (/resize frame :width (- *screen-width* 1) :height (- *screen-height* *pager-height* *prompt-height* *status-height* *terminal-height*))
	       (/resize terminal :height *terminal-height* :width *screen-width*)
	       (/resize quickhelp :height *quickhelp-height* :width *quickhelp-width*)
	       ;;
	       (/resize status :height *status-height* :width *window-width*)
	       (/move status :x 0 :y 0)
	       (/show status)
	       (setf *status* status)
	       (/resize engine :height 20 :width 100)
	       (/move engine :x 200 :y 0)
	       (/hide engine)
	       (/resize commander :height 550 :width 580)
	       (/move commander :x 200 :y 25)
	       (/hide commander)
	       (setf *commander* commander)
	       ;;	           [set-receiver prompt engine]
	       (/resize stack :width *screen-width* :height (- *screen-height* *pager-height* *prompt-height*))
	       [install-keybindings engine]
	       (/move quickhelp :y (- *screen-height* *quickhelp-height* *pager-height*) :x (- *screen-width* *quickhelp-width* *quickhelp-spacer*))
	       (/auto-position *pager*)))
      (add-hook 'xe2:*resize-hook* #'resize-widgets))
    ;;
    (/resize prompt :height *prompt-height* :width *screen-width*)
    (/move prompt :x 0 :y 0)
    (/show prompt)
    (/install-keybindings prompt)
    (/install-keybindings frame)
    (/say prompt "Welcome to XIOBEAT. Press ALT-X to enter command mode, or F1 for help.")
    (/set-mode prompt :forward) ;; don't start with prompt on
    (/set-receiver prompt frame)
    ;; 
    (/resize form :height (- *screen-height* *terminal-height* 
			    *prompt-height* *pager-height*) 
	    :width (- *screen-width* *sidebar-width*))
    (/move form :x 0 :y 0)
    (/set-prompt form prompt)
    (/set-narrator form terminal)
    ;;
    (/resize-to-scroll help :height (- *screen-height* *pager-height*) :width *screen-width*)
    (/move help :x 0 :y 0)
    (setf (field-value :read-only help) t)
    (let ((text	(find-resource-object "xiobeat-help-message")))
      (/set-buffer help text))
    ;;
    (/resize help-prompt :width 10 :height 10)
    (/move help-prompt :x 0 :y 0)
    (/hide help-prompt)
    (/set-receiver help-prompt help)

    ;;
    (/resize form2 :height (- *screen-height* *terminal-height* *prompt-height* *pager-height*) :width *sidebar-width*)
    (/move form2 :x 0 :y 0)
    (setf (field-value :header-style form2) nil)
    (/set-prompt form2 prompt)
    (/set-narrator form2 terminal)
    ;;
    (xe2:halt-music 1000)
    ;;
    ;; (/resize help :height 540 :width 800) 
    ;; (/move help :x 0 :y 0)
    (/resize-to-scroll help :height 540 :width 800) 
    (/move help :x 0 :y 0)
    (let ((text	(find-resource-object "xiobeat-help-message")))
      (/set-buffer help text))
    ;;
    (/resize quickhelp :height *quickhelp-height* :width *quickhelp-width*)
    (/move quickhelp :y (- *screen-height* *quickhelp-height* *pager-height*) :x (- *screen-width* *quickhelp-width* *quickhelp-spacer*))
    (let ((text	(find-resource-object "xiobeat-quickhelp-message")))
      (dolist (line text)
    	(dolist (string line)
    	  (funcall #'send nil :print-formatted-string quickhelp string))
    	(/newline quickhelp)))
    ;;
    (/resize stack :width *screen-width* :height (- *screen-height* *pager-height*))
    (/move stack :x 0 :y 0)
    (/set-children stack (list frame status terminal prompt))
    ;;
    (/resize frame :width *screen-width* :height (- *screen-height* *pager-height* *terminal-height* *prompt-height*))
    (/move frame :x 0 :y 0)
    (/set-children frame (list form form2))
    ;;
    (/resize terminal :height *terminal-height* :width *screen-width*)
    (/move terminal :x 0 :y (- *screen-height* *terminal-height*))
    (/set-verbosity terminal 0)
    ;;
    ;;
    (setf *pager* (clone =pager=))
    (/auto-position *pager*)
    ;;
    (/add-page *pager* :edit (list prompt stack frame terminal status quickhelp))
    (/add-page *pager* :help (list help-prompt help))
    (/select *pager* :edit)
    (xe2:reset-joystick)
    (xe2:enable-classic-key-repeat 100 100)
    [focus-left *frame*]
    (run-hook 'xe2:*resize-hook*)
))

(xiobeat)
