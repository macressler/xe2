;;; superxong.lisp --- hockey paintball snake pong

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

(in-package :xiodance)

(setf xe2:*dt* 60)

(setf xe2:*joystick-button-symbols*
      '(:a :b :x :y :left :right :up :down :select :start))

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
    ("KP-INSERT" nil "select .")
    ("JOYSTICK" (:up) "up .")
    ("JOYSTICK" (:left) "left .")
    ("JOYSTICK" (:right) "right .")
    ("JOYSTICK" (:down) "down .")
    ("JOYSTICK" (:y) "button-y .")
    ("JOYSTICK" (:x) "button-x .")
    ("JOYSTICK" (:b) "button-b .")
    ("JOYSTICK" (:a) "button-a .")
    ("JOYSTICK" (:start) "start .")
    ("JOYSTICK" (:select) "select .")))

(defparameter *qwerty-keybindings*
  (append *basic-keybindings* nil))

(define-prototype xiodance (:parent xe2:=prompt=))
  
(define-method install-keybindings xiodance ()
  (dolist (k *qwerty-keybindings*)
      (apply #'bind-key-to-prompt-insertion self k)))

(define-method select xiodance ()
  (halt-music 0))

(define-method start xiodance ()
  (play-music "electron" :loop t))

(define-method button-y xiodance ()
  (play-sample "pad1"))

(define-method button-x xiodance ()
  (play-sample "pad2")

(define-method button-b xiodance ()
  (play-sample "bd"))
(define-method button-a xiodance ()
(define-method up xiodance ()
(define-method down xiodance ()
(define-method left xiodance ()
(define-method right xiodance ()

(defparameter *energy-dance-pad-mapping*
  '((0 . :up)
    (1 . :left)
    (2 . :right)
    (3 . :down)
    (4 . :y)
    (5 . :x)
    (6 . :b)
    (7 . :a)
    (8 . :select)
    (9 . :start)))

(defparameter *xiodance-window-width* 800)
(defparameter *xiodance-window-height* 600)

(defun xiodance ()
  (xe2:message "Initializing Xiodance...")
  (setf xe2:*window-title* "Xiodance")
  (xe2:set-screen-height *xiodance-window-height*)
  (xe2:set-screen-width *xiodance-window-width*)
  (let* ((prompt (clone =xiodance=)))
    [resize prompt :height 20 :width 100]
    [move prompt :x 0 :y 0]
    [hide prompt]
    [install-keybindings prompt]
    (xe2:reset-joystick)
    (set-music-volume 255)
    (setf *joystick-mapping* *energy-dance-pad-mapping*)
    (xe2:install-widgets (list prompt))
    (xe2:enable-classic-key-repeat 100 100)))

