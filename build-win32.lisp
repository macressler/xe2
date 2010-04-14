;;; Deliver the program using allegro common lisp on win32
(in-package :cl-user)

(require :asdf)

(defvar *dll-pathname* #p"c:/Users/Sandy/xe2/")
(defvar *game* "xiotank")
(defvar *executable* #p"c:/Users/Sandy/xe2/app.exe")
(defvar *base-pathname* (make-pathname :name nil :type nil :defaults *load-pathname*))

(pushnew (translate-pathname *base-pathname* "**/" "**/site/cffi_0.10.3/") asdf:*central-registry*)
(pushnew (translate-pathname *base-pathname* "**/" "**/site/babel_0.3.0/") asdf:*central-registry*)
(pushnew (translate-pathname *base-pathname* "**/" "**/site/alexandria/") asdf:*central-registry*)
(pushnew (translate-pathname *base-pathname* "**/" "**/site/trivial-features_0.4/") asdf:*central-registry*)
(pushnew (translate-pathname *base-pathname* "**/" "**/site/rt-20040621/") asdf:*central-registry*)

(pushnew (translate-pathname *base-pathname* "**/" "**/site/lispbuilder/lispbuilder-sdl/") asdf:*central-registry*)
(pushnew (translate-pathname *base-pathname* "**/" "**/site/lispbuilder/lispbuilder-sdl-image/") asdf:*central-registry*)
(pushnew (translate-pathname *base-pathname* "**/" "**/site/lispbuilder/lispbuilder-sdl-mixer/") asdf:*central-registry*)
(pushnew (translate-pathname *base-pathname* "**/" "**/site/lispbuilder/lispbuilder-sdl-gfx/") asdf:*central-registry*)

(pushnew (translate-pathname *base-pathname* "**/" "**/clon/") asdf:*central-registry*)
(pushnew (translate-pathname *base-pathname* "**/" "**/xe2/") asdf:*central-registry*)

(asdf:oos 'asdf:load-op :cffi)
(require 'sb-posix)
(sb-posix:chdir *dll-pathname*)
;;(setf *default-pathname-defaults* (make-pathname :directory '(:relative)))
(asdf:oos 'asdf:load-op :xe2)
(pop cffi:*foreign-library-directories*)

(defun main ()
  (setf xe2:*module-directories* (list (make-pathname :directory '(:relative))))
  (xe2:play *game*)
  0)

(sb-ext:save-lisp-and-die *executable* :toplevel #'main :executable t)
