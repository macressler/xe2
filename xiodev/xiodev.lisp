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

;;; Main program. 

(defparameter *window-width* 1200)
(defparameter *window-height* 720)
(defparameter *prompt-height* 20)
(defparameter *terminal-height* 100)
(defparameter *pager-height* 20)

(defvar *form*)
(defvar *terminal*)
(defvar *prompt*)
(defvar *pager*)

(define-prototype xiodev-prompt (:parent xe2:=prompt=))

(define-method say xiodev-prompt (&rest args)
  [say *terminal* "~S" args])
  
(defun xiodev ()
  (xe2:message "Initializing XIODEV...")
  (setf xe2:*window-title* "XIODEV")
  (clon:initialize)
  (xe2:set-screen-height *window-height*)
  (xe2:set-screen-width *window-width*)
  (let* ((prompt (clone =xiodev-prompt=))
	 (help (clone =formatter=))
	 (quickhelp (clone =formatter=))
	 (form (clone =form=))
	 (terminal (clone =narrator=))
	 (stack (clone =stack=)))
    ;;
    (setf *form* form)
    (setf *prompt* prompt)
    (setf *terminal* terminal)
    ;;
    [resize prompt :height *prompt-height* :width *window-width*]
    [move prompt :x 0 :y 0]
    [show prompt]
    [install-keybindings prompt]
    [set-receiver prompt form]
    ;; 
    [resize form :height (- *window-height* *terminal-height* *prompt-height* *pager-height*) :width *window-width*]
    [move form :x 0 :y 0]
    [set-prompt form prompt]
    [set-narrator form terminal]
    ;;
    (xe2:halt-music 1000)
    ;; (setf xe2:*physics-function* #'(lambda (&rest ignore)
    ;; 				     (when *world* [run-cpu-phase *world* :timer])))
    [resize help :height 540 :width 800] 
    [move help :x 0 :y 0]
    (let ((text	(find-resource-object "help-message")))
      (dolist (line text)
    	(dolist (string line)
    	  (funcall #'send nil :print-formatted-string help string))
    	[newline help]))
    ;; ;;
    ;; [resize quickhelp :height 72 :width 250] 
    ;; [move quickhelp :y (- *window-height* 130) :x (- *window-width* 250)]
    ;; (let ((text	(find-resource-object "quickhelp-message")))
    ;;   (dolist (line text)
    ;; 	(dolist (string line)
    ;; 	  (funcall #'send nil :print-formatted-string quickhelp string))
    ;; 	[newline quickhelp]))
    ;;
    [resize stack :width *window-width* :height (- *window-height* *pager-height*)]
    [move stack :x 0 :y 0]
    [set-children stack (list form terminal prompt)]
    ;;
    [resize terminal :height *terminal-height* :width *window-width*]
    [move terminal :x 0 :y (- *window-height* *terminal-height*)]
    [set-verbosity terminal 0]
    ;;
    ;;
    (setf *pager* (clone =pager=))
    [auto-position *pager*]
    ;;
    [add-page *pager* :help (list help)]
    [add-page *pager* :edit (list stack prompt form terminal )]
    [select *pager* :edit]
;;    [add-page *pager* :configure nil]
    (xe2:enable-classic-key-repeat 100 100)
))


(xiodev)


;;; xiodev.lisp ends here
