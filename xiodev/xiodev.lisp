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
(defvar *forms*)

(define-prototype help-prompt (:parent =prompt=)
  (default-keybindings :initform '(("N" nil "page-down .")
				   ("P" nil "page-up ."))))

(define-prototype help-textbox (:parent =textbox=))

(define-method render help-textbox ()
  [parent>>render self]
  [message *pager* 
	   (list (format nil " --- Line ~A of ~A. Use N (NEXT) and P (PREVIOUS) to scroll the text." 
			 <point-row> (length <buffer>)))])

(add-hook '*after-load-module-hook* (lambda ()
				      [message *pager* (list (format nil "  CURRENT MODULE: ~S." *module*))]))

(define-prototype xiodev-prompt (:parent xe2:=prompt=))

(define-method say xiodev-prompt (&rest args)
  (apply #'send nil :say *terminal* args))

(define-prototype xiodev-split (:parent xe2:=split=))

(define-method install-keybindings xiodev-split ()
  ;; (bind-key-to-method self "UP" '(:control) :pickup)
  ;; (bind-key-to-method self "DOWN" '(:control) :clone)
  (bind-key-to-method self "LEFT" '(:control) :left-pane)
  (bind-key-to-method self "RIGHT" '(:control) :right-pane)
  (bind-key-to-method self "LEFTBRACKET" nil :apply-left)
  (bind-key-to-method self "RIGHTBRACKET" nil :apply-right)
  (bind-key-to-method self "TAB" nil :switch-panes))

(define-method left-form xiodev-split ()
  (nth 0 <children>))

(define-method right-form xiodev-split ()
  (nth 1 <children>))

(define-method left-selected-data xiodev-split ()
  [get-selected-cell-data [left-form self]])

(define-method right-selected-data xiodev-split ()
  [get-selected-cell-data [right-form self]])

(define-method left-pane xiodev-split ()
  [say self "Selecting left pane."]
  (setf <focus> 0))

(define-method right-pane xiodev-split ()
  [say self "Selecting right pane."]
  (setf <focus> 1))

(define-method apply-left xiodev-split ()
  (let* ((form [left-form self])
	 (tool (field-value :tool form))
	 (data [right-selected-data self]))
    [say self (format nil "Applying LEFT tool ~S to data ~S in LEFT form." tool data)]
    [apply-tool form data]))

(define-method apply-right xiodev-split ()
  (let* ((form [right-form self])
	 (tool (field-value :tool form))
	 (data [left-selected-data self]))
    [say self (format nil "Applying RIGHT tool ~S to data ~S in RIGHT form." tool data)]
    [apply-tool form data]))

(defun xiodev ()
  (setf xe2:*screen-width* *window-width*)
  (setf xe2:*screen-height* *window-height*)
  (xe2:message "Initializing XIODEV...")
  (setf xe2:*window-title* "XIODEV")
  (clon:initialize)
  (xe2:set-screen-height *window-height*)
  (xe2:set-screen-width *window-width*)
  (let* ((prompt (clone =xiodev-prompt=))
	 (help (clone =help-textbox=))
	 (help-prompt (clone =help-prompt=))
	 (quickhelp (clone =formatter=))
	 (form (clone =form=))
	 (form2 (clone =form= "*index*"))
	 (terminal (clone =narrator=))
	 (split (clone =xiodev-split=))
	 (stack (clone =stack=)))
    ;;
    (setf *form* form)
    (setf *prompt* prompt)
    (setf *terminal* terminal)
    (setf *forms* split)
    (labels ((resize-widgets ()
	       [say terminal "Resizing to ~S" (list :width *screen-width* :height *screen-height*)]
	       [resize prompt :height *prompt-height* :width *screen-width*]
	       [resize form :height (- *screen-height* *terminal-height* 
				       *prompt-height* *pager-height*) 
		       :width (- *screen-width* *sidebar-width* 2)]
	       [resize form2 :height (- *screen-height* *terminal-height* *prompt-height* *pager-height*) :width (- *sidebar-width* 2)]
	       [resize-to-scroll help :height (- *screen-height* *pager-height*) :width *screen-width*]
	       [resize stack :width *screen-width* :height (- *screen-height* *pager-height*)]
	       [resize split :width (- *screen-width* 1) :height (- *screen-height* *pager-height* *prompt-height* *terminal-height*)]
	       [resize terminal :height *terminal-height* :width *screen-width*]
	       [auto-position *pager*]))
      (add-hook 'xe2:*resize-hook* #'resize-widgets))
    ;;
    [resize prompt :height *prompt-height* :width *screen-width*]
    [move prompt :x 0 :y 0]
    [show prompt]
    [install-keybindings prompt]
    [install-keybindings split]
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
    [resize-to-scroll help :height 540 :width 800] 
    [move help :x 0 :y 0]
    (setf (field-value :read-only help) t)
    (let ((text	(find-resource-object "help-message")))
      [set-buffer help text])
    ;;
    [resize help-prompt :width 10 :height 10]
    [move help-prompt :x 0 :y 0]
    [hide help-prompt]
    [set-receiver help-prompt help]

    ;;
    [resize form2 :height (- *screen-height* *terminal-height* *prompt-height* *pager-height*) :width *sidebar-width*]
    [move form2 :x 0 :y 0]
    (setf (field-value :header-style form2) nil)
    [set-prompt form2 prompt]
    [set-narrator form2 terminal]
    ;;
    (xe2:halt-music 1000)
    ;;
    ;; [resize help :height 540 :width 800] 
    ;; [move help :x 0 :y 0]
    [resize-to-scroll help :height 540 :width 800] 
    [move help :x 0 :y 0]
    (let ((text	(find-resource-object "help-message")))
      [set-buffer help text])
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
    [add-page *pager* :help (list help-prompt help)]
    [select *pager* :edit]
    (xe2:enable-classic-key-repeat 100 100)
;;    (run-hook 'xe2:*resize-hook*)
))

(xiodev)


;;; xiodev.lisp ends here
