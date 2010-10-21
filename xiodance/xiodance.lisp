;;; xiodance --- freestyle video dance engine

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

(defpackage :xiodance
  (:documentation "XIODANCE is a freestyle video dance engine written in Common Lisp.")
  (:use :xe2 :common-lisp)
  (:export xiodance))

(in-package :xiodance)

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

(defparameter *qwerty-keybindings*
  (append *basic-keybindings* nil))

(define-prototype xiodance (:parent xe2:=prompt=))
  
(define-method install-keybindings xiodance ()
  (dolist (k *basic-keybindings*)
      (apply #'bind-key-to-prompt-insertion self k)))

(define-method select xiodance ()
  (play-music "electron2" :loop t))

(define-method start xiodance ()
  (play-music "electron" :loop t))

(define-method button-y xiodance ()
  (play-sample "pad1"))

(define-method button-x xiodance ()
  (play-sample "pad2"))

(define-method button-b xiodance ()
  (play-sample "bd"))

(define-method button-a xiodance ()
  (play-sample "ka"))

(define-method up xiodance ())

(define-method down xiodance ())

(define-method left xiodance ()
  (play-sample "snare"))

(define-method right xiodance ()
  (play-sample "snare"))

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

(defparameter *xiodance-window-width* 800)
(defparameter *xiodance-window-height* 600)

(defun xiodance ()
  (xe2:message "Initializing Xiodance...")
  (setf xe2:*window-title* "Xiodance")
  (setf xe2:*output-chunksize* 256)
  (xe2:set-screen-height *xiodance-window-height*)
  (xe2:set-screen-width *xiodance-window-width*)
  (let* ((prompt (clone =xiodance=)))
    [resize prompt :height 20 :width 100]
    [move prompt :x 0 :y 0]
    [show prompt]
    [set-receiver prompt prompt]
    [install-keybindings prompt]
    (xe2:reset-joystick)
    (set-music-volume 255)
    (setf xe2:*dt* 60)
    (setf xe2:*joystick-button-symbols*
	  '(:a :b :x :y :left :right :up :down :select :start))
    (setf *joystick-mapping* *energy-dance-pad-mapping*)
    (xe2:install-widgets prompt)
    (xe2:enable-classic-key-repeat 100 100)))

(xiodance)
