(defpackage :cons-game
  (:documentation "CONS is an alternate universe sci-fi shooter game.")
  (:use :xe2 :common-lisp)
  (:export cons-game))

(in-package :cons-game)

(setf xe2:*module-package-name* :cons-game)

(defvar *player*)
