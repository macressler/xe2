;;; xiodev.lisp --- XE2 dev module

;; Copyright (C) 2010  David O'Toole

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

(defpackage :xiodev
  (:use :xe2 :common-lisp)
  (:export xiodev))

(in-package :xiodev)

(setf xe2:*dt* 20)
(setf xe2:*resizable* t)

;;; Main program. 

(defparameter *window-width* 1200)
(defparameter *window-height* 720)
(defparameter *prompt-height* 20)
(defparameter *terminal-height* 100)
(defparameter *pager-height* 20)
(defparameter *sidebar-width* 400)

(defvar *form*)
(defvar *terminal*)
(defvar *prompt*)
(defvar *pager*)

(add-hook '*after-load-module-hook* (lambda ()
				      [message *pager* (list (format nil "  CURRENT MODULE: ~S." *module*))]))

(define-prototype xiodev-prompt (:parent xe2:=prompt=))

(define-method say xiodev-prompt (&rest args)
  (apply #'send nil :say *terminal* args))

(defun xiodev ()
  (setf xe2:*screen-width* *window-width*)
  (setf xe2:*screen-height* *window-height*)
  (xe2:message "Initializing XIODEV...")
  (setf xe2:*window-title* "XIODEV")
  (clon:initialize)
  (xe2:set-screen-height *window-height*)
  (xe2:set-screen-width *window-width*)
  (let* ((prompt (clone =xiodev-prompt=))
	 (help (clone =formatter=))
	 (quickhelp (clone =formatter=))
	 (form (clone =form=))
	 (form2 (clone =form= "*index*"))
	 (terminal (clone =narrator=))
	 (split (clone =split=))
	 (stack (clone =stack=)))
    ;;
    (setf *form* form)
    (setf *prompt* prompt)
    (setf *terminal* terminal)
    (labels ((resize-widgets ()
	       [say terminal "Resizing to ~S" (list :width *screen-width* :height *screen-height*)]
	       [resize prompt :height *prompt-height* :width *screen-width*]
	       [resize form :height (- *screen-height* *terminal-height* 
				       *prompt-height* *pager-height*) 
		       :width (- *screen-width* *sidebar-width* 2)]
	       [resize form2 :height (- *screen-height* *terminal-height* *prompt-height* *pager-height*) :width (- *sidebar-width* 2)]
	       [resize help :height 540 :width 800] 
	       [resize stack :width *screen-width* :height (- *screen-height* *pager-height*)]
	       [resize split :width (- *screen-width* 1) :height (- *screen-height* *pager-height* *terminal-height*)]
	       [resize terminal :height *terminal-height* :width *screen-width*]
	       [auto-position *pager*]))
      (add-hook 'xe2:*resize-hook* #'resize-widgets))
    ;;
    [resize prompt :height *prompt-height* :width *screen-width*]
    [move prompt :x 0 :y 0]
    [show prompt]
    [install-keybindings prompt]
    [say prompt "Welcome to XIODEV. Press CONTROL-X to enter command mode, or F1 for help."]
    [set-mode prompt :forward] ;; don't start with prompt on
    [set-receiver prompt split]
    ;; 
    [resize form :height (- *screen-height* *terminal-height* 
			    *prompt-height* *pager-height*) 
	    :width (- *screen-width* *sidebar-width*)]
    [move form :x 0 :y 0]
    [set-prompt form prompt]
    [set-narrator form terminal]
    ;;
    [resize form2 :height (- *screen-height* *terminal-height* *prompt-height* *pager-height*) :width *sidebar-width*]
    [move form2 :x 0 :y 0]
    (setf (field-value :header-style form2) nil)
    ;; [set-prompt form2 prompt]
    [set-narrator form2 terminal]
    ;;
    (xe2:halt-music 1000)
    ;;
    [resize help :height 540 :width 800] 
    [move help :x 0 :y 0]
    (let ((text	(find-resource-object "help-message")))
      (dolist (line text)
    	(dolist (string line)
    	  (funcall #'send nil :print-formatted-string help string))
    	[newline help]))
    ;; ;;
    ;; [resize quickhelp :height 72 :width 250] 
    ;; [move quickhelp :y (- *screen-height* 130) :x (- *screen-width* 250)]
    ;; (let ((text	(find-resource-object "quickhelp-message")))
    ;;   (dolist (line text)
    ;; 	(dolist (string line)
    ;; 	  (funcall #'send nil :print-formatted-string quickhelp string))
    ;; 	[newline quickhelp]))
    ;;
    [resize stack :width *screen-width* :height (- *screen-height* *pager-height* *prompt-height*)]
    [move stack :x 0 :y 0]
    [set-children stack (list split terminal prompt)]
    ;;
    [resize split :width *screen-width* :height (- *screen-height* *pager-height* *terminal-height* *prompt-height*)]
    [move split :x 0 :y 0]
    [set-children split (list form form2)]
    ;;
    [resize terminal :height *terminal-height* :width *screen-width*]
    [move terminal :x 0 :y (- *screen-height* *terminal-height*)]
    [set-verbosity terminal 0]
    ;;
    ;;
    (setf *pager* (clone =pager=))
    [auto-position *pager*]
    ;;
    [add-page *pager* :edit (list prompt stack split terminal)]
    [add-page *pager* :help (list help)]
    [select *pager* :edit]
    (xe2:enable-classic-key-repeat 100 100)
    (run-hook 'xe2:*resize-hook*)
))

(xiodev)


;;; xiodev.lisp ends here
