;;; console.lisp --- core operations for XE2

;; Copyright (C) 2006, 2007, 2008, 2009, 2010  David O'Toole

;; Author: David O'Toole <dto@gnu.org> <dto1138@gmail.com>
;; Keywords: multimedia, games

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; The "console" is the library which provides all XE2 system
;; services. Primitive operations such as setting the resolution,
;; displaying bitmaps, drawing lines, playing sounds, file access, and
;; keyboard/mouse input are handled here. 

;; Currently it uses the cross-platform SDL library (via
;; LISPBUILDER-SDL) as its device driver, and wraps the library for
;; use by the rest of XE2.

;; http://lispbuilder.sourceforge.net/

(in-package :xe2) 

;;; Platforms

(defvar *windows* nil)
(defvar *linux* nil)
(defvar *osx* nil)

;;; Message logging

(defparameter *message-logging* nil)

(defun message (format-string &rest args)
  "Print a log message to the standard output. The FORMAT-STRING and
remaining arguments are passed to `format'.

When the variable `*message-logging*' is nil, this output is
disabled."
  (when *message-logging*
    (apply #'format t format-string args)
    (fresh-line)))

;;; Sequence numbers

(defvar *sequence-number* 0)

(defun genseq (&optional (x 0))
  "Generate an all-purpose sequence number."
  (+ x (incf *sequence-number*)))
   
;;; Hooks

(defun add-hook (hook func)
  "Hooks are special variables whose names are of the form
`*foo-hook*' and whose values are lists of functions taking no
arguments. The functions of a given hook are all invoked (in list
order) whenever the hook is run with `run-hook'.

This function arranges for FUNC to be invoked whenever HOOK is triggered with
`run-hook'. The function should have no arguments."
  (pushnew func (symbol-value hook)))

(defun remove-hook (hook func)
  "Stop calling FUNC whenever HOOK is triggered."
  (setf (symbol-value hook)
	(delete func (symbol-value hook))))

(defun run-hook (hook)
  "Call all the functions in HOOK, in list order."
  (dolist (func (symbol-value hook))
    (funcall func)))

;;; Vector utility macro 

(defmacro do-cells ((var expr) &body body)
  "Execute the forms in BODY with VAR bound successively to the
elements of the vector produced by evaluating EXPR."
  (let ((counter (gensym))
	(vector (gensym)))
    `(progn
       (let* ((,var nil)
	      (,vector (progn ,expr)))
	 (when (vectorp ,vector)
	   (let ((,counter (fill-pointer ,vector)))
	     (decf ,counter)
	     (loop while (>= ,counter 0) 
		   do (setf ,var (aref ,vector ,counter))
		   (progn (decf ,counter)
			  (when ,var ,@body)))))))))

;;; The active widgets list 

(defvar *active-widgets* nil "List of active widget objects. 
These widgets receive input events and are rendered to the screen by
the console. See also `send-event-to-widgets'.

Do not set this variable directly from a module; instead, call
`install-widgets'.")

(defun show-widgets ()
  "Draw the active widgets to the screen."
  (dolist (widget *active-widgets*)
    (with-field-values (image x y visible) widget
      (when (and image visible)
	[render widget]
	(sdl:draw-surface-at-* image x y)))))

(defvar *module-widgets* nil "List of widget objects in the current module.")

(defun install-widgets (&rest widgets)
  "User-level function for setting the active widget set. Note that
XE2 may override the current widget set at any time for system menus
and the like."
  (setf *module-widgets* widgets)
  (setf *active-widgets* widgets))
;; TODO why does this crash: 
;;  (show-widgets))

(defun install-widget (widget)
  (unless (find widget *module-widgets*)
    (setf *module-widgets* (append *module-widgets* (list widget))))
  (unless (find widget *active-widgets*)
    (setf *active-widgets* (append *active-widgets* (list widget))))
  (show-widgets))

(defun uninstall-widget (widget)
  (setf *module-widgets* (delete widget *module-widgets* :test #'eq))
  (setf *active-widgets* (delete widget *active-widgets* :test #'eq))
  (show-widgets))

;;; "Classic" key repeat

(defun enable-classic-key-repeat (delay interval)
  ;; (let ((delay-milliseconds (truncate (* delay (/ 1000.0 *frame-rate*))))
  ;; 	(interval-milliseconds (truncate (* interval (/ 1000.0 *frame-rate*)))))
    (sdl:enable-key-repeat delay interval))

(defun disable-classic-key-repeat ()
  (sdl:disable-key-repeat))

;;; "Held Keys" key repeat emulation

(defvar *key-table* (make-hash-table :test 'equal))

(defvar *held-keys* nil)

(defun enable-held-keys (&rest args)
  "Enable key repeat on every frame when held. Arguments are ignored
for backward-compatibility."
  (when args 
    (message "Warning. DELAY and INTERVAL arguments to XE2:ENABLE-HELD-KEYS are deprecated and ignored."))
  (setf *key-table* (make-hash-table :test 'equal))
  (setf *held-keys* t))

(defun disable-held-keys ()
  "Disable key repeat."
  (setf *held-keys* nil))
;;  (sdl:disable-key-repeat))

(defun hold-event (event)
  (when (null (gethash event *key-table*)) 
    (setf (gethash event *key-table*) 0)))

(defun release-held-event (event)
  (setf (gethash event *key-table*) -1))

(defun send-held-events ()
  (unless (null *key-table*)
    (maphash #'(lambda (event counter)
		 (dispatch-event event)
		 ;; A counter value of -1 means that the key release
		 ;; happened before the event had a chance to be sent.
		 ;; These must be removed immediately after sending once.
		 (if (minusp counter)
		     (remhash event *key-table*)
		     ;; Otherwise, keep counting how many frames the
		     ;; key is held for.
		     (when (numberp (gethash event *key-table*))
		       (incf (gethash event *key-table*)))))
	     *key-table*)))

(defun break-events (event)
  (labels ((break-it (event2 ignore)
	     (when (intersection event event2 :test 'equal)
	       (message "Breaking ~S due to match with ~S." event2 event)
	       (remhash event2 *key-table*))))
    (maphash #'break-it *key-table*)))

;;; Physics timestep callback

(defvar *dt* 10)

(defvar *physics-function* nil)

(defun do-physics (&rest args) 
  (when (functionp *physics-function*)
    (apply *physics-function* args)))

;;; Event handling and widgets

(defun send-event-to-widgets (event)
  "Keyboard, mouse, joystick, and timer events are represented as
event lists of the form:

:      (STRING . MODIFIERS)

Where MODIFIERS is a list of symbols like :shift, :control, :alt,
 :timer, :system, :mouse, and so on.

The default event handler attempts to deliver a keypress to one of
the widgets in `*active-widgets*'. See widgets.lisp and the docstrings
below for more information.

This function attempts to deliver EVENT to each of the *active-widgets*
one at a time (in list order) until one of them is found to have a
matching keybinding, in which case the keybinding's corresponding
function is triggered. If none of the widgets have a matching
keybinding, nothing happens, and this function returns nil."
  (some #'(lambda (widget)
	    [handle-key widget event])
	*active-widgets*))

(defvar *event-handler-function* #'send-event-to-widgets
  "Function to be called with keypress events. This function should
accept an event list of the form

  (STRING . MODIFIERS)

where STRING is a string representing the key, and MODIFIERS is a list
of key modifier symbols like :shift, :control, :alt, and so on.

The modifier list is sorted; thus, events can be compared for
equality with `equal' and used as hashtable keys.

The default event handler is `send-event-to-widgets', which see. An
XE2 game can use the widget framework to do its drawing and event
handling, or override `*event-handler-function*' and do something
else.")

(defun normalize-event (event)
  "Convert EVENT to a normal form suitable for `equal' comparisons."
  (setf (rest event)
	(sort (remove-duplicates (delete nil (rest event)))
	      #'string< :key #'symbol-name))
  event)

(defun dispatch-event (event)
  "Send EVENT to the handler function."
  (if *event-handler-function*
      (progn (message "TRANSLATED EVENT: ~A" event)
	     (funcall *event-handler-function* event))
      (error "No event handler registered.")))

(defun hit-widgets (x y &optional (widgets *active-widgets*))
  "Hit test the WIDGETS to find the clicked widget."
  (some #'(lambda (widget)
	    [hit widget x y])
	(reverse widgets)))

;;; Translating SDL key events into XE2 event lists

(defparameter *other-modifier-symbols* '(:button-down :button-up :axis))

(defun make-key-modifier-symbol (sdl-mod)
  "Translate from the SDL key modifier symbol SDL-MOD to our own
key event symbols."
  (if (or (member sdl-mod *joystick-button-symbols*)
	  (member sdl-mod *other-modifier-symbols*))
      sdl-mod
      (ecase sdl-mod
	(:SDL-KEY-MOD-NONE nil)
	(:SDL-KEY-MOD-LSHIFT :shift)
	(:SDL-KEY-MOD-RSHIFT :shift)
	(:SDL-KEY-MOD-LCTRL :control)
	(:SDL-KEY-MOD-RCTRL :control)
	(:SDL-KEY-MOD-LALT :alt)
	(:SDL-KEY-MOD-RALT :alt)
	(:SDL-KEY-MOD-LMETA :meta)
	(:SDL-KEY-MOD-RMETA :meta)
	;; for compatibility:
	(:SDL-KEY-NONE nil)
	(:SDL-KEY-LSHIFT :shift)
	(:SDL-KEY-RSHIFT :shift)
	(:SDL-KEY-LCTRL :control)
	(:SDL-KEY-RCTRL :control)
	(:SDL-KEY-LALT :alt)
	(:SDL-KEY-RALT :alt)
	(:SDL-KEY-LMETA :meta)
	(:SDL-KEY-RMETA :meta)
	;; fix for windows
	(:SDL-KEY-MOD-NUM nil)
	(:SDL-KEY-CAPS :caps-lock)
	(:SDL-KEY-MODE nil)
	(:SDL-KEY-MOD-MODE :mode)
	(:SDL-KEY-RESERVED nil)
	)))
  
(defun make-key-string (sdl-key)
  "Translate from :SDL-KEY-X to the string \"X\"."
  (let ((prefix "SDL-KEY-"))
    (subseq (symbol-name sdl-key)
            (length prefix))))

(defun make-event (sdl-key sdl-mods)
  "Create a normalized event out of the SDL data SDL-KEY and SDL-MODS.
The purpose of putting events in a normal form is to enable their use
as hash keys."
  (message "SDL KEY AND MODS: ~A" (list sdl-key sdl-mods))
  (normalize-event
   (cons (if (eq sdl-key :joystick) 
	     "JOYSTICK"
	     (if (eq sdl-key :axis) 
		 "AXIS"
		 (make-key-string sdl-key)))
	 (mapcar #'make-key-modifier-symbol
		 (cond ((keywordp sdl-mods)
			(list sdl-mods))
		       ((listp sdl-mods)
			sdl-mods)
		       ;; catch apparent lispbuilder-sdl bug?
		       ((eql 0 sdl-mods)
			nil))))))

;;; Joystick support (gamepad probably required)

(defparameter *joystick-button-symbols*
  '(:button-0 :button-1 :button-2 :button-3 :button-4 :button-5 :button-6 :button-7 :button-8 :button-9
    :button-10 :button-11 :button-12 :button-13 :button-14 :button-15 :button-16 :button-17 :button-18 :button-19
    :left :right :up :down :select :start))

(defparameter *generic-joystick-mapping*
  '((0 . :button-0)
    (1 . :button-1)
    (2 . :button-2)
    (3 . :button-3)
    (4 . :button-4)
    (5 . :button-5)
    (6 . :button-6)
    (7 . :button-7)
    (8 . :button-8)
    (9 . :button-9)
    (10 . :button-10)
    (11 . :button-11)
    (12 . :button-12)
    (13 . :button-13)
    (14 . :button-14)
    (15 . :button-15)
    (16 . :button-16)
    (17 . :button-17)
    (18 . :button-18)
    (19 . :button-19)
    (20 . :button-20)))

(defvar *joystick-dead-zone* 2000)

(defvar *joystick-axis-mapping* '((0 :left :right)
				  (1 :up :down)))

(defun axis-value-to-direction (axis value)
  (let ((entry (assoc axis *joystick-axis-mapping*)))
    (if entry 
	(if (plusp value)
	    (second (cdr entry))
	    (when (minusp value)
	      (first (cdr entry)))))))

(defvar *joystick-axis-values* (make-array 100 :initial-element 0))

(defun do-joystick-axis-event (axis value state)
  (dispatch-event (make-event :axis 
			      (list (axis-value-to-direction axis value)
				    state))))
	
(defun update-joystick-axis (axis value)
  (let ((state (if (< (abs value) *joystick-dead-zone*)
		   :button-up :button-down)))
    (setf (aref *joystick-axis-values* axis) value)))

(defun poll-joystick-axis (axis)
  (aref *joystick-axis-values* axis))

(defvar *joystick-mapping* *generic-joystick-mapping*)

(defun translate-joystick-button (button)
  (cdr (assoc button *joystick-mapping*)))

(defun symbol-to-button (sym)
  (let ((entry (some #'(lambda (entry)
			 (when (eq sym (cdr entry))
			   entry))
		     *joystick-mapping*)))
    (when entry 
      (car entry))))

(defvar *joystick-device* nil)

(defvar *joystick-buttons* nil
  "The nth element is non-nil when the nth button is pressed.")

(defvar *joystick-position* nil "Current position of the joystick, as a direction keyword.")

(defun reset-joystick ()
  "Re-open the joystick device and re-initialize the state."
  (setf *joystick-device* (sdl-cffi::sdl-joystick-open 0))
  (setf *joystick-buttons* (make-array 100 :initial-element nil))
  (setf *joystick-position* :here))

(defun update-joystick (button state)
  "Update the table in `*joystick-buttons*' to reflect the STATE of
the BUTTON. STATE should be either 1 (on) or 0 (off)."
  (setf (aref *joystick-buttons* button) (ecase state
					   (1 t)
					   (0 nil)))
  (let ((sym (translate-joystick-button button)))
    (labels ((pressed (sym) 
	       (let ((b (symbol-to-button sym)))
		 (when (integerp b)
		   (aref *joystick-buttons* b)))))
      (setf *joystick-position* 
	    (or (cond ((and (pressed :up) (pressed :right))
		       :northeast)
		      ((and (pressed :up) (pressed :left))
		       :northwest)
		      ((and (pressed :down) (pressed :right))
		       :southeast)
		      ((and (pressed :down) (pressed :left))
		       :southwest)
		      ((pressed :up)
		       :north)
		      ((pressed :down)
		       :south)
		      ((pressed :right)
		       :east)
		      ((pressed :left)
		       :west))
		:here)))
    (message "UPDATE-JOYSTICK: BUTTON(~S) STATE(~S)" button state)))

(defun poll-joystick-button (button)
  "Return 1 if the button numbered BUTTON is pressed, otherwise 0."
  (sdl-cffi::sdl-joystick-get-button *joystick-device* button))

(defun poll-all-buttons ()
  (dolist (entry *joystick-mapping*)
    (destructuring-bind (button . symbol) entry
      (update-joystick button (poll-joystick-button button)))))

(defun generate-button-events ()
  (let ((button 0) state sym)
    (loop while (< button (length *joystick-buttons*))
	  do (setf state (aref *joystick-buttons* button))
	     (setf sym (translate-joystick-button button))
	     (when (and state sym)
	       (dispatch-event (make-event :joystick sym)))
	     (incf button))))

;;; The active world

(defvar *world* nil 
"The current world object. Only one may be active at a time. See also
worlds.lisp. Cells are free to send messages to `*world*' at
any time, because it is always bound to the world containing the cell
at the time the cell method is run.")

(defun world ()
  "Return the current world."
  *world*)

;;; Auto-zooming images

(defvar *zoom-factor* 1 
"When set to some integer greater than 1, all image resources are
scaled by that factor unless marked with the property :nozoom t.")

(defun is-zoomed-resource (resource)
  "Return non-nil if the RESOURCE should be zoomed by `*zoom-factor*'."
  (not (getf (resource-properties resource)
	     :nozoom)))

(defun zoom-image (image &optional (factor *zoom-factor*))
  "Return a zoomed version of IMAGE, zoomed by FACTOR.
Allocates a new image."
  (assert (integerp factor))
  (lispbuilder-sdl-gfx:zoom-surface factor factor
				    :surface image
				    :smooth nil))

;;; Timing (OBSOLETE)

;; TODO remove this section, it's deprecated. 

(defvar *frame-rate* 30 
"The intended frame rate of the game. Recommended value is 30.
Don't set this variable directly; use `set-frame-rate' instead.")

(defun set-frame-rate (rate)
  "Set the frame rate for the game. The recommended default is 30.
You only need to set the frame rate if you are using the timer; see
`enable-timer'."
  (message "Setting frame rate to ~S" rate)
  (message "WARNING: SET-FRAME-RATE is deprecated and does nothing.")
  (setf *frame-rate* rate)
  (setf (sdl:frame-rate) rate))

(defvar *clock* 0 "Number of frames until next timer event.")

(defvar *timer-p* nil "Non-nil if timer events are actually being sent.")

(defun enable-timer ()
  "Enable timer events. The next scheduled event will be the first sent."
  (setf *timer-p* t))

(defun disable-timer ()
  "Disable timer events."
  (setf *timer-p* nil))

(defvar *timer-event* (list nil :timer) 
  "Since all timer events are identical, this is the only one we need.")

(defvar *timer-interval* 15 
"Number of frames to wait before sending each timer event.
Set this to 0 to get a timer event every frame.
Don't set this yourself; use `set-timer-interval'.")

(defun set-timer-interval (interval)
  "Set the number of frames to wait before sending each timer event.
Set it to 0 to get a timer event every frame."
  (setf *timer-interval* interval))

;;; Screen dimensions

(defvar *resizable* nil)

(defparameter *resize-hook* nil)

(defvar *screen-width* 640 "The width (in pixels) of the game
window. Set this in the game startup file.")

(defun set-screen-width (width)
  (setf *screen-width* width))

(defvar *screen-height* 480 "The height (in pixels) of the game
window. Set this in the game startup file.")

(defun set-screen-height (height)
  (setf *screen-height* height))

;;; The main loop of XE2

(defvar *next-module* "standard")

(defvar *quitting* nil)

(defvar *fullscreen* nil "When non-nil, attempt to use fullscreen mode.")

(defvar *window-title* "XE2")
(defvar *window-position* :center
  "Controls the position of the game window. Either a list of coordinates or the symbol :center.")

(defun run-main-loop ()
  "Initialize the console, open a window, and play.
We want to process all inputs, update the game state, then update the
display."
  (let ((fps (make-instance 'sdl:fps-unlocked :dt *dt* :ps-fn #'do-physics)))
    (cond (*fullscreen*
	   (sdl:window *screen-width* *screen-height*
		       :fps fps 
		       :title-caption *window-title*
		       :flags sdl:SDL-FULLSCREEN
		       :position *window-position*))
	  (*resizable*
	   	   (sdl:window *screen-width* *screen-height*
		       :fps fps 
		       :title-caption *window-title*
		       :flags sdl:SDL-RESIZABLE
		       :position *window-position*))
	  (t (sdl:window *screen-width* *screen-height*
			 :fps fps
			 :title-caption *window-title*
			 :position *window-position*)))
    (reset-joystick)
    (sdl:clear-display sdl:*black*)
    (show-widgets)
    (sdl:update-display)
    (run-hook '*initialization-hook*)
    (let ((events-this-frame 0))
      (sdl:with-events ()
	(:quit-event () (prog1 t))
	(:video-resize-event (:w w :h h)  
			     (setf *screen-width* w
				   *screen-height* h)
			     (run-hook '*resize-hook*)
			     (sdl:window w h :fps fps :title-caption *window-title*
					 :flags sdl:SDL-RESIZABLE
					 :position *window-position*))
	(:mouse-motion-event (:state state :x x :y y :x-rel x-rel :y-rel y-rel)
			     nil)
	(:mouse-button-down-event (:button button :state state :x x :y y)
				  (let ((object (hit-widgets x y *active-widgets*)))
				    (cond ((null object)
					   (message ""))
					  ((eq t object)
					   nil)
					  (t 
					   ;; deliver messages in a queued environment
					   (sdl:clear-display sdl:*black*)
					   (when *world*
					     (when (field-value :message-queue *world*)
					       (with-message-queue (field-value :message-queue *world*)
						 (case button
						   (1 (when (has-method :select object) 
							[select object]))
						   (3 (when (has-method :activate object) 
							[activate object]))))
					       [process-messages *world*]))))))
	(:mouse-button-up-event (:button button :state state :x x :y y)
				nil)
	(:joy-button-down-event (:which which :button button :state state)
				(when (assoc button *joystick-mapping*)
				  (update-joystick button state)
				  (dispatch-event (make-event :joystick
							      (list (translate-joystick-button button) 
								    :button-down)))))
	(:joy-button-up-event (:which which :button button :state state)  
			      (when (assoc button *joystick-mapping*)
				(update-joystick button state)
				(dispatch-event (make-event :joystick
							    (list (translate-joystick-button button) 
								  :button-up)))))
	(:joy-axis-motion-event (:which which :axis axis :value value)
				(update-joystick-axis axis value))
	(:video-expose-event () (sdl:update-display))
	(:key-down-event (:key key :mod-key mod)
			 (let ((event (make-event key mod)))
			   (if *held-keys*
			       (hold-event event)
			       (dispatch-event event))))
	(:key-up-event (:key key :mod-key mod)
		       (when *held-keys*
			 (let* ((event (make-event key mod))
				(entry (gethash event *key-table*)))
			   (if (numberp entry)
			       (if (plusp entry)
				   (progn (message "Removing entry ~A:~A" event entry)
					  (remhash event *key-table*))
				   (when (zerop entry)
				     ;; This event hasn't yet been sent,
				     ;; but the key release happened
				     ;; now. Mark this entry as pending
				     ;; deletion.
				     (release-held-event event)))
			       (break-events event)))))
	(:idle ()
	       (if *timer-p*
		   (if (zerop *clock*)
		       (progn 
			 (sdl:clear-display sdl:*black*)
			 ;; send held events
			 (when *held-keys*
			   (send-held-events))
			 ;; send timer event
			 (dispatch-event *timer-event*)
			 ;; send any joystick button events
			 ;; (poll-all-buttons)
			 ;;  (generate-button-events)
			 ;; update display
			 (show-widgets)
			 (sdl:update-display)
			 (setf *clock* *timer-interval*))
		       (decf *clock*))
		   ;; clean this up. these two cases aren't that different.
		   (progn 
		     (sdl:clear-display sdl:*black*)
		     (when *held-keys* (send-held-events)) ;; TODO move this to do-physics?
		     (show-widgets) 
		     (sdl:update-display))))))))
  
  
;;; The .xe2rc user init file

(defparameter *user-init-file-name* ".xe2rc")

(defvar *initialization-hook* nil 
"This hook is run after the XE2 console is initialized.
Set timer parameters and other settings here.")

(defun load-user-init-file ()
  (let ((file (merge-pathnames (make-pathname :name *user-init-file-name*)
			       (user-homedir-pathname))))
    (when (probe-file file)
      (load (merge-pathnames (make-pathname :name *user-init-file-name*)
			     (user-homedir-pathname))))))

(defparameter *user-keyboard-layout* :qwerty)

(defparameter *use-sound* t "Non-nil (the default) is to use sound. Nil disables sound.")

;;; PAK resource interchange files

(defparameter *pak-file-extension* ".pak"
"PAK is a simple Lisp data interchange file format readable and
writable by both Emacs Lisp and Common Lisp. A PAK file can contain
one or more data resources. A 'resource' is an image, sound, text,
font, lisp program, or other data whose interpretation is up to the
client.

A PAK resource can be either self-contained, or point to an
external file for its data.

A 'resource record' defines a resource. A resource record is a
structure with the following elements:

 :NAME    A string; the name of the resource.
          The colon character : is reserved and used to specify 
          resource transformations; see below.
 :TYPE    A keyword symbol identifying the data type.
          Corresponding handlers are the responsibility of the client.
          See also `*resource-handlers*' and `load-resource'.

          The special type :pak is used to load the pak file
          specified in :FILE, from (optionally) another module
          whose name is given in :DATA.

          The special type :alias is used to provide multiple names
          for a resource. The :DATA field contains the name of the
          target resource. This name can specify resource
          transformations, see below. 

 :PROPERTIES  Property list with extra data; for example :copyright,
              :license, :author. 
              The special property :AUTOLOAD, when non-nil causes
              the resource to be loaded automatically upon startup 
              (the default is to load resources on demand.)

 :FILE    Name of file to load data from, if any. 
          Relative to directory of PAK file.
 :DATA    Lisp data encoding the resource itself, if any.

In memory, these will be represented by resource structs (see below).
On disk, it's Lisp data printed as text. This text will compress very
well.

The string '()' is a valid .PAK file; it contains no resources.")

(defstruct resource 
  name type properties file data object modified-p)

;; The extra `object' field is not saved in .PAK files; it is used to
;; store driver-dependent loaded resources (i.e. SDL image surface
;; objects and so on). This is used in the resource table.
;; The modified-p field is likewise not stored. 

(defun resource-to-plist (res)
  "Convert the resource record RES into a property list.

This prepares it for printing as part of a PAK file."
  (list :name (resource-name res)
	:type (resource-type res)
	:properties (resource-properties res)
	:file (resource-file res)
	:data (resource-data res)
	:object nil))

;; First we need routines to read and write raw s-expressions to and
;; from text files.

(defconstant *keyword-package* (find-package :keyword))

(defun write-sexp-to-file (filename sexp)
  (message "Writing data to file ~S" filename)
  (with-open-file (file filename :direction :output 
			:if-exists :overwrite
			:if-does-not-exist :create)
    (let ((*package* *keyword-package*))
      (format file "~S" sexp)))
  (message "Writing data to file ~S... Done." filename))

(defun read-sexp-from-file (filename)
  (message "Reading data from ~A..." filename)
  (with-open-file (file filename :direction :input)
    (prog1 (read file)
      (message "Reading data from ~A... Done." filename))))

;; Now tie it all together with routines that read and write
;; collections of records into PAK files.

(defun write-pak (filename resources)
  "Write the RESOURCES to the PAK file FILENAME."
  (write-sexp-to-file filename (mapcar #'resource-to-plist resources)))

(defun read-pak (filename)
  "Return a list of resources from the PAK file FILENAME."
  (mapcar #'(lambda (plist)
	      (apply #'make-resource plist))
	  (read-sexp-from-file filename)))

;;; Resources and modules

(defvar *resource-table* nil 
  "A hash table mapping resource names to resource records. All loaded
resources go in this one hash table.

The `resource table' maps resource names to their corresponding
records. `Indexing' a resource means that its resource record is
added to the resource table. `Loading' a resource means that any
associated driver-dependent object (SDL image surface, audio buffer
object, etc) is created. This value is stored into the OBJECT field
of the resource record upon loading; see `load-resource'.

The loading operation may be driver-dependent, so each resource
type (i.e. :image, :text, :sound) is handled by its own plugin
function (see `*resource-handlers*').

`Finding' a resource means looking up its record in the resource
table, and loading the resource if it hasn't been loaded already.
A lookup failure results in an error. See `find-resource'.

A `module' is a directory full of resource files. The name of the
module is the name of the directory. Each module must contain a
file called {module-name}.pak, which should contain an index of
all the module's resources. Multiple modules may be loaded at one
time. In addition the special resource .startup will be loaded;
if this is type :lisp, the startup code for your game can go in
that external lisp file.")

(defun initialize-resource-table ()
  "Create a new empty resource table."
   (setf *resource-table* (make-hash-table :test 'equal)))

(defun index-resource (resource)
  "Add the RESOURCE's record to the resource table.
If a record with that name already exists, it is replaced.  However,
if the resource is an :alias, just the string name of the target
resource is stored; see also `find-resource'."
  (let ((val (if (eq :alias (resource-type resource))
		 (resource-data resource)
		 resource)))
    (setf (gethash (resource-name resource)
		   *resource-table*) 
	  val)))

(defvar *executable* nil)

(defvar *module-directories* 
  (list (if *executable*
            (make-pathname :directory (pathname-directory (car #+sbcl sb-ext:*posix-argv*
                                                               #+clozure ccl:*command-line-argument-list*)))
	    (make-pathname :directory 
			   (pathname-directory 
			    (make-pathname
			     :host (pathname-host #.(or *compile-file-truename*
							*load-truename*))
			     :device (pathname-device #.(or *compile-file-truename*
							    *load-truename*))
			     :directory (pathname-directory #.(or *compile-file-truename*
								  *load-truename*)))))))
  "List of directories where XE2 will search for modules.
Directories are searched in list order.")
;; (load-time-value 
;; (or #.*compile-file-truename* *load-truename*))))

(defun find-module-path (module-name)
  "Search the `*module-directories*' path for a directory with the
name MODULE-NAME. Returns the pathname if found, otherwise nil."
  (let ((dirs *module-directories*))
    (message "Probing directories ~S..." dirs)
    (or 
     (loop 
       for dir in dirs for path
	 = (probe-file (make-pathname :directory 
				      (append (pathname-directory
					       dir) (list module-name))
			    :defaults dir))
       when path return path)
     (error "Cannot find module ~s in paths ~S. 
You must set the variable XE2:*MODULE-DIRECTORIES* in the configuration file ~~/.xe2rc
Please see the included file BINARY-README for instructions."
	    module-name dirs))))

(defun find-module-file (module-name file)
  "Make a pathname for FILE within the module MODULE-NAME."
  (merge-pathnames file (find-module-path module-name)))

(defun directory-is-module-p (dir)
  "Test whether a PAK index file exists in a directory."
  (let ((index-filename (concatenate 'string
				     (file-namestring dir)
				     *pak-file-extension*)))
    (probe-file (make-pathname :name index-filename
			       :directory (if (stringp dir)
					      dir
					      (namestring dir))))))

(defun find-modules-in-directory (dir)
  "Search DIR for modules and return a list of their names."
  (let ((dirnames (mapcar #'(lambda (s)
			      (subseq s 0 (1- (length s))))
			  (mapcar #'namestring
				  (directory (concatenate 'string (namestring dir) "/*/"))))))
    (remove-if-not #'directory-is-module-p dirnames)))

(defun find-all-modules ()
  (mapcar #'file-namestring
	  (mapcan #'find-modules-in-directory *module-directories*)))

(defvar *pending-autoload-resources* '())

(defun index-pak (module-name pak-file)
  "Add all the resources from the pak PAK-FILE to the resource
table. File names are relative to the module MODULE-NAME."
  (let ((resources (read-pak pak-file)))
    (dolist (res resources)
      (if (eq :pak (resource-type res))
	  ;; we're including another pak file. if :data is specified,
	  ;; take this as the name of the module where to look for
	  ;; that pak file and its resources.
	  (let ((include-module (or (resource-data res) 
				    module-name)))
	    (index-pak include-module (find-module-file include-module
							(resource-file res))))
	  ;; we're indexing a single resource.
	  (progn
	    (index-resource res)
	    ;; change the file field into a full pathname, for resources
	    ;; that need to load data from an external file later.
	    (when (resource-file res)
	      (setf (resource-file res)
		    (merge-pathnames (resource-file res)
				     (find-module-path module-name))))
	    ;; save the resource name for later autoloading, if needed
	    (when (getf (resource-properties res) :autoload)
	      (push res *pending-autoload-resources*)))))))

(defparameter *object-index-filename* "objects.pak")

(defun index-module (module-name)
  "Add all the resources from the module MODULE-NAME to the resource
table."
  (let ((index-file (find-module-file module-name
				      (concatenate 'string module-name ".pak"))))
    (index-pak module-name index-file)))

;;; Standard resource names

(defvar *startup* ".startup")

(defvar *default-font* ".default-font")

;;; Creating, saving, and loading object resources in PAK files

;; See also the documentation string for `*pak-file-extension*'.

;; Object resources are PAK resources with type :object. These are
;; used to save serialized objects to disk and read them back
;; again. Each page is stored in one PAK file, containing a single
;; resource with the serialized data stored in the :DATA field of the
;; resource record. Page-names are resource-names, and therefore must
;; be unique within a given XE2 module. A page's PAK file is stored in
;; {MODULENAME}/{PAGENAME}.pak, and for a given module these PAKs will
;; all be included by {MODULENAME}/OBJECTS.PAK, which is an
;; automatically generated PAK index linking to all the serialized
;; page PAK files.

(defun make-object-resource (name object)
  "Make an object resource named NAME (a string) with the CLON object
OBJECT as the data."
  (message "Creating new object resource ~S." name)
  (let ((resource (make-resource :name name 
				 :type :object
				 :object object)))
    (prog1 resource
      (index-resource resource))))

(defun save-object-resource (resource &optional (module *module*))
  "Save an object resource to disk as {RESOURCE-NAME}.PAK."
  (let ((name (resource-name resource)))
    (message "Serializing resource ~S..." name)
;    (assert (clon:object-p (resource-object resource)))
    (setf (resource-data resource) (clon:serialize (resource-object resource)))
    (message "Saving resource ~S..." name)
    (write-pak (find-module-file module 
				 (concatenate 'string (resource-name resource)
					      *pak-file-extension*))
	       (list resource))
    (message "Saving resource ~S... Done." name)
    (setf (resource-modified-p resource) nil)
    (setf (resource-data resource) nil)))

(defun is-special-resource (resource)
  (string= "*" (string (aref (resource-name resource) 0))))
  
(defun save-modified-objects (&optional force)
  (let (index)
    (labels ((save (name resource)
	       (when (not (stringp resource))
		 (when (eq :object (resource-type resource))
		   (unless (is-special-resource resource)
		     ;; we want to index them all, whether or not we save them all.
		     ;; make a link resource (i.e. of type :pak) to pull this in later
		     (let ((link-resource (make-resource :type :pak 
							 :file (concatenate 'string
									    (resource-name resource)
									    *pak-file-extension*))))
		       (push link-resource index))
		     (when (or force (resource-modified-p resource))
		       (save-object-resource resource)))))))
      (maphash #'save *resource-table*))
    ;; write auto-generated index
    (write-pak (find-module-file *module* *object-index-filename*) index)))

;; (save-modified-objects t)

(defun load-object-resource (resource)
  "Loads a serialized :OBJECT resource from the Lisp data in the 
:DATA field of the RESOURCE argument. Returns the rebuilt object. See
also the documentation for CLON:DESERIALIZE."
  (let ((object (clon:deserialize (resource-data resource))))
    (assert (clon:object-p object))
    (setf (resource-data resource) nil) ;; no longer needed
    object))

;;; Driver-dependent resource object loading handlers

(defun load-image-resource (resource)
  "Loads an :IMAGE-type pak resource from a :FILE on disk."
  ;; handle zooming
  (let ((image 
         (sdl-image:load-image (namestring (resource-file resource))
                               :alpha 255)))
    (if (or (= 1 *zoom-factor*)
	    (not (is-zoomed-resource resource)))
	image 
	;; TODO get rid of this. subclass viewport instead
	;; if you want to zoom everything. 
	(zoom-image image *zoom-factor*))))

(defun load-sprite-sheet-resource (resource)
  "Loads a :SPRITE-SHEET-type pak resource from a :FILE on disk. Looks
for :SPRITE-WIDTH and :SPRITE-HEIGHT properties on the resource to
control the size of the individual frames or subimages."
  (let* ((image (load-image-resource resource))
	 (props (resource-properties resource))
	 (w (or (getf props :width)
                (image-width image)))
	 (h (or (getf props :height)
                (image-height image)))
	 (sw (getf props :sprite-width))
	 (sh (getf props :sprite-height))
	 (sprite-cells (loop for y from 0 to (- h sh) by sh
			     append (loop for x from 0 to (- w sw) by sw
					  collect (list x y sw sh)))))
    (setf (sdl:cells image) sprite-cells)
    (setf (getf props :sprite-cells) sprite-cells)
    image))

(defun load-bitmap-font-resource (resource)
  (let ((props (resource-properties resource)))
    (if (null props)
	(error "Must set properties for bitmap font.")
	(destructuring-bind (&key width height character-map color-key) props
	  (sdl-gfx:initialise-font (make-instance 'SDL:simple-font-definition
						  :width width :height height
						  :character-map character-map
						  :color-key (apply #'sdl:color color-key)
						  :filename (resource-file resource)
						  :pad-x 0 :pad-y 0))))))
    
(defun load-text-resource (resource)
  (with-open-file (file (resource-file resource)
			:direction :input
			:if-does-not-exist nil)
    (loop for line = (read-line file nil)
	  while line collect line)))

(defun load-formatted-text-resource (resource)
  (read-sexp-from-file (resource-file resource)))
    
(defun load-lisp-resource (resource)
  (let* ((source (resource-file resource))
	 (fasl (compile-file-pathname source)))
    ;; do we need recompilation?
    (if (probe-file fasl)
    	(if (> (file-write-date source)
    	       (file-write-date fasl))
	    ;; recompile. 
    	    (load (compile-file source))
    	    ;; no, just load the fasl
    	    (load fasl))
	;; create the fasl for the first time. 
	(load (compile-file source)))))

(defun load-canvas-resource (resource)
  (destructuring-bind (&key width height background)
      (resource-properties resource)
    (let ((canvas (create-image width height)))
      (prog1 canvas
	(when background
	  (draw-box 0 0 width height 
		    ;; TODO support arbitrary rgb and other drawing commands
		    :stroke-color background
		    :color background
		    :destination canvas))))))

(defun load-color-resource (resource)
  (destructuring-bind (red green blue)
      (resource-data resource)
    (sdl:color :r red :g green :b blue)))

(defun load-font-resource (resource)
  (let ((font-name (string-upcase (concatenate 'string 
					       "*font-" 
					       (resource-data resource)
					       "*"))))
    (sdl:initialise-font (symbol-value (intern font-name :lispbuilder-sdl)))))

(defun load-music-resource (resource)
  (when *use-sound*
    (sdl-mixer:load-music (namestring (resource-file resource)))))

(defun load-sample-resource (resource)
  (when *use-sound*
    (let ((chunk (sdl-mixer:load-sample (namestring (resource-file resource)))))
      (prog1 chunk
	(when (resource-properties resource)
	  (destructuring-bind (&key volume) (resource-properties resource)
	    (when (numberp volume)
	      (setf (sdl-mixer:sample-volume chunk) volume))))))))

(defvar *resource-handlers* (list :image #'load-image-resource
				  :lisp #'load-lisp-resource
				  :object #'load-object-resource
				  :sprite-sheet #'load-sprite-sheet-resource
				  :color #'load-color-resource
				  :music #'load-music-resource
				  :bitmap-font #'load-bitmap-font-resource
				  :text #'load-text-resource
				  :formatted-text #'load-formatted-text-resource
				  :sample #'load-sample-resource
				  :canvas #'load-canvas-resource
				  :font #'load-font-resource)
  "A property list mapping resource type keywords to handler functions.
Each function should accept one resource record, and return an
object (possibly driver-dependent). When a resource is loaded (with
`load-resource'), the appropriate handler is looked up, and invoked on
the resource record.  The return value is stored in the OBJECT field
of the record.")

;;; Transforming resources

(defvar *resource-transformation-delimiter* #\:)

(defun is-transformable-resource (name)
  (eq (aref name 0)
      *resource-transformation-delimiter*))

(defun next-transformation (name)
  (assert (is-transformable-resource name))
  (let ((delimiter-pos (position *resource-transformation-delimiter* 
				 (subseq name 1))))
    (when delimiter-pos 
      (let* ((*read-eval* nil)
	     (xform-command (subseq name 1 (1+ delimiter-pos))))
	(read-from-string (concatenate 'string 
				       "(" 
				       xform-command
				       ")"))))))

(defun next-source (name)
  (assert (is-transformable-resource name))
  (let ((delimiter-pos (position *resource-transformation-delimiter*
				 (subseq name 1))))
    (if (numberp delimiter-pos)
	(subseq name (1+ delimiter-pos))
	(subseq name 1))))

(defun rotate-image (resource degrees)
  (sdl:rotate-surface degrees :surface (resource-object resource)))

(defun subsect-image (resource x y w h)
(let ((image (sdl:copy-surface :cells (sdl:rectangle :x x :y y :w w :h h)
			       :surface (resource-object resource) :inherit t)))
  (sdl:set-surface-* image :x 0 :y 0)
  image))

(defun scale-image (resource scale)
  (zoom-image (resource-object resource) scale))

(defvar *resource-transformations* 
  (list :rotate #'rotate-image
	:subimage #'subsect-image
	:scale #'scale-image))

;;; Main user-level functions for finding and loading resources.

(defun load-resource (resource)
  "Load the driver-dependent object of RESOURCE into the OBJECT field
so that it can be fed to the console."
  (message "Attempting to load resource ~S." (resource-name resource))
  (let ((handler (getf *resource-handlers* (resource-type resource))))
    (assert (functionp handler))
    ;; fill in the object field by invoking the handler, if needed
    (when (null (resource-object resource))
      (setf (resource-object resource)
	    (funcall handler resource)))
    (if (null (resource-object resource))
	(error "Failed to load resource ~S." (resource-name resource))
	(message "Loaded resource ~S with result type ~S." 
		 (resource-name resource)
		 (type-of (resource-object resource))))))

(defun find-resource (name &optional noerror)
  "Obtain the resource named NAME, performing any necessary loading
and/or transformations. Unless NOERROR is non-nil, signal an error
when NAME cannot be found."
  ;; can we find the resource straight off? 
  (let ((res (gethash name *resource-table*)))
    (cond ((resource-p res)
	   ;; yes, load-on-demand
	   (prog1 res
	     (when (null (resource-object res))
	       (load-resource res))))
	  ;; no, is it an alias?
	  ((stringp res)
	   ;; look up the real one and make the alias map to the real resource
	   (setf (gethash name *resource-table*) 
		 (find-resource res)))
	  ;; not found and not an alias. try to xform
	  ((null res)
	   (if (is-transformable-resource name)
	       ;; ok. let's xform and cache the result
	       (let ((xform (next-transformation name))
		     (source-name (next-source name)))
		 (setf (gethash name *resource-table*) 
		       (if (null xform)
			   (find-resource source-name)
			   (destructuring-bind (operation . arguments) xform
			     (let* ((xformer (getf *resource-transformations* 
						   (make-keyword operation)))
				    (source-res (find-resource source-name))
				    (source-type (resource-type source-res))
				    (source (resource-object source-res))
				    (xformed-resource (apply xformer source-res
							     arguments)))
			       (make-resource :name name 
					      :type source-type
					      :object xformed-resource))))))
	       ;; can't xform. 
	       (if noerror
		   nil
		   (error "Cannot find resource.")))))))

(defun find-resource-object (name &optional noerror)
  "Obtain the resource object named NAME, or signal an error if not
found."
  (let ((val (find-resource name noerror)))
    (if (resource-p val)
	(resource-object val)
	(if noerror nil (error "Resource ~S not found." name)))))

(defun find-resource-property (resource-name property)
  "Read the value of PROPERTY from the resource RESOURCE-NAME."
  (getf (resource-properties (find-resource resource-name))
	property))

(defun set-resource-modified-p (resource &optional (value t))
  (let ((res (find-resource resource)))
    (setf (resource-modified-p res) value)))

;;; Loading modules as a whole and autoloading resources

(defvar *module* nil "The name of the current module.")

(defparameter *after-load-module-hook* nil)

(defvar *module-package-name* nil)

(defun module-package-name (&optional (module-name *module*))
  (let ((default-name (or *module-package-name* (make-keyword module-name))))
    (prog1 default-name
      (unless (find-package default-name)
	(error "Cannot find package ~S" default-name)))))
    
(defun load-module (module &key (autoload t))
  "Load the module named MODULE. Load any resources marked with a
non-nil :autoload property. This operation also sets the default
object save directory (by setting the current `*module*'. See also
`save-object-resource')."
  (setf *module* module)
  (setf *pending-autoload-resources* nil)
  (index-module module)
  (when autoload 
    (mapc #'load-resource (nreverse *pending-autoload-resources*)))
  (setf *pending-autoload-resources* nil)
  ;; now load any objects
  (let ((object-index-file (find-module-file module *object-index-filename*)))
    (when (probe-file object-index-file)
      (message "Loading saved objects from ~S" object-index-file)
      (index-pak module object-index-file)))
  (run-hook '*after-load-module-hook*)
  (setf *package* (find-package (module-package-name))))

;;; Playing music and sound effects

(defvar *frequency* 44100)

(defvar *output-chunksize* 1024)

(defvar *output-channels* 2)

(defvar *sample-format* SDL-CFFI::AUDIO-S16LSB)

(defun cffi-sample-type (sdl-sample-type)
  (ecase sdl-sample-type
    (SDL-CFFI::AUDIO-U8 :uint8) ; Unsigned 8-bit samples
    (SDL-CFFI::AUDIO-S8 :int8) ; Signed 8-bit samples
    (SDL-CFFI::AUDIO-U16LSB :uint16) ; Unsigned 16-bit samples, in little-endian byte order
    (SDL-CFFI::AUDIO-S16LSB :int16) ; Signed 16-bit samples, in little-endian byte order
    ;; (SDL-CFFI::AUDIO-U16MSB nil) ; Unsigned 16-bit samples, in big-endian byte order
    ;; (SDL-CFFI::AUDIO-S16MSB nil) ; Signed 16-bit samples, in big-endian byte order
    (SDL-CFFI::AUDIO-U16 :uint16)  ; same as SDL(SDL-CFFI::AUDIO-U16LSB (for backwards compatability probably)
    (SDL-CFFI::AUDIO-S16 :int16) ; same as SDL(SDL-CFFI::AUDIO-S16LSB (for backwards compatability probably)
    (SDL-CFFI::AUDIO-U16SYS :uint16) ; Unsigned 16-bit samples, in system byte order
    (SDL-CFFI::AUDIO-S16SYS :int16) ; Signed 16-bit samples, in system byte order
    ))

(defun cffi-chunk-buffer (chunk)
  (sdl:fp chunk))

(defun convert-cffi-sample (chunk)
  (let* ((input-buffer (cffi-chunk-buffer chunk))
	 (type (cffi-sample-type *sample-format*))
	 (size (length (cffi:mem-ref input-buffer type))))
    (assert (eq *sample-format* SDL-CFFI::AUDIO-S16LSB)) ;; for now
    (let ((output-buffer (make-array size)))
	(prog1 output-buffer
	  (dotimes (n size)
	    (setf (aref output-buffer n)
		  (/ (float (cffi:mem-aref input-buffer type n))
		     32768.0)))))))

;(defun convert-internal-audio (input-buffer output-stream)

;; (REGISTER-MUSIC-MIXER 
;;     (lambda (user stream len)
;;       &#039;FILL-THE-AUDIO-OUTPUT-BUFFER))

(defvar *channels* 128 "Number of audio mixer channels to use.")

(defun set-music-volume (number)
  "Set the mixer music volume between 0 (silent) and 255 (full volume)."
  (when *use-sound*
    (setf (sdl-mixer:music-volume) number)))

(defun play-music (music-name &rest args)
  "Begin playing the music resource MUSIC-NAME. If the resource
MUSIC-NAME has the property :volume, its value is used as the volume
of the music."
  (when *use-sound*
    (let ((resource (find-resource music-name))
	  (volume (find-resource-property music-name :volume)))
      (assert (eq :music (resource-type resource)))
      (set-music-volume (or volume 255))
      (apply #'sdl-mixer:play-music 
	     (resource-object resource)
	     args))))

(defun halt-music (fade-milliseconds)
  "Stop all music playing."
  (when *use-sound*
    (sdl-mixer:halt-music fade-milliseconds)))

(defun play-sample (sample-name &rest args)
  "When sound is enabled, play the sample resource SAMPLE-NAME."
  (when *use-sound*
    (let ((resource (find-resource sample-name)))
      (assert (eq :sample (resource-type resource)))
      (apply #'sdl-mixer:play-sample 
	     (resource-object resource)
	     args))))

(defun halt-sample (channel &rest args)
  (when *use-sound*
    (apply #'sdl-mixer:halt-sample :channel channel args)))

;;; Font operations

;; A PAK entry for a font looks like this: 

;; (:name ".default-font" 
;;        :type :font 
;;        :properties (:height 14 :width 7) 
;;        :data "7x14")

;; Right now you can only specify built-in LISPBUILDER-SDL fonts.
;; See also `load-font-resource'.

(defun font-height (font)
  (find-resource-property font :height))

(defun font-width (font)
  (find-resource-property font :width))

(defun font-text-extents (string font)
  (* (length string)
     (font-width font)))

(defun draw-string-solid (string x y 
			  &key destination (font *default-font*) (color ".white"))
  (sdl:draw-string-solid-* string x y :surface destination :font (find-resource-object font)
			   :color (find-resource-object color)))

(defun draw-string-shaded (string x y &optional (foreground ".white") (background ".black")
			  &key destination (font *default-font*))
  (sdl:draw-string-shaded-* string x y (find-resource-object foreground)
			    (find-resource-object background)
			    :surface destination :font (find-resource-object font)))

;;; Standard colors

;; The X11 standard colors are loaded by default into the resource
;; table from the raw data in `*x11-color-data*'. See also rgb.lisp.

(defun initialize-colors ()
  "Load the X11 color data into the resource table."
  (dolist (color *x11-color-data*)
    (destructuring-bind (name red green blue) color
      (index-resource (make-resource :name (concatenate 'string "." name)
				     :type :color
				     :data (list red green blue))))))

;;; Icons

;; An icon is an image that corresponds to a cell method keyword. The
;; expression (icon-image :move) becomes the image ".move".
;; See cells.lisp for a list of keywords.

;; Standard icons for these are in the "standard" module. 

(defun icon-resource (key)
  "Return an icon resource for the key KEY.
The standard GEAR icon is used when no other applicable icon can be
found."
  (or (find-resource (concatenate 'string "."
				  (string-downcase (symbol-name key)))
		     :noerror)
      (find-resource ".gear")))

(defun icon-image (key)
  "Return an icon image name for KEY."
  (resource-name (icon-resource key)))

;;; Creating and displaying images

;; The "driver dependent objects" for XE2 images are just SDL:SURFACE
;; objects. (The situation is the same for XE2 colors, fonts, and so
;; on). So long as the clients treat the driver-dependent resource
;; objects as opaque, this thin wrapper is sufficient.

;; Below are some image handling functions.

(defun create-image (width height)
  "Create a new XE2 image of size (* WIDTH HEIGHT)."
  (assert (and (integerp width) (integerp height)))
  (sdl:create-surface width height))

(defun draw-image (image x y &key (destination sdl:*default-surface*) (render-cell nil))
  "Draw the IMAGE at offset (X Y) on the image DESTINATION.
The default destination is the main window."
  (sdl:draw-surface-at-* image x y :cell render-cell :surface destination))

(defun draw-resource-image (name x y &key (destination sdl:*default-surface*) (render-cell nil))
  "Draw the image named by NAME at offset (X Y) on the image DESTINATION.
The default destination is the main window."
  (draw-image (find-resource-object name) x y :render-cell render-cell :destination destination))

(defun image-height (image)
  "Return the height in pixels of IMAGE."
  (let ((img (if (stringp image)
		 (find-resource-object image)
		 image)))
    (sdl:height img)))

(defun image-width (image)
  "Return the width in pixels of IMAGE."
  (let ((img (if (stringp image)
		 (find-resource-object image)
		 image)))
    (sdl:width img)))

;;; Drawing shapes and other primitives

(defun draw-box (x y width height		
		 &key (stroke-color ".white")
		 (color ".black")
		 destination)
  "Draw a filled rectangle at (X Y) of size (* WIDTH HEIGHT)."
  (sdl:draw-box-* x y width height :color (find-resource-object color)
		  :stroke-color (find-resource-object stroke-color)
		  :surface destination))

(defun draw-rectangle (x y width height
		       &key (color ".white")
		       destination)
  (sdl:draw-rectangle-* x y width height :color (find-resource-object color)
			:surface destination))

(defun draw-line (x0 y0 x1 y1 
		     &key 
		     (color ".white")
		     destination)
  (sdl:draw-line-* x0 y0 x1 y1 :surface destination :color (find-resource-object color)))

(defun draw-pixel (x y &key 
		   (color ".white")
		   destination)
  (sdl:draw-pixel-* x y :surface destination :color (find-resource-object color)))

(defun draw-circle (x y radius &key 
		   (color ".white")
		    destination)
  (sdl:draw-circle-* x y radius :surface destination :color (find-resource-object color)))

;;; Keyboard status reading

(defparameter *key-identifiers*
  '((UNKNOWN 0)
  (:FIRST 0)
  (:BACKSPACE 8)
  (:TAB 9)
  (:CLEAR 12)
  (:RETURN 13)
  (:PAUSE 19)
  (:ESCAPE 27)
  (:SPACE 32)
  (:EXCLAIM 33)
  (:QUOTEDBL 34)
  (:HASH 35)
  (:DOLLAR 36)
  (:AMPERSAND 38)
  (:QUOTE 39)
  (:LEFTPAREN 40)
  (:RIGHTPAREN 41)
  (:ASTERISK 42)
  (:PLUS 43)
  (:COMMA 44)
  (:MINUS 45)
  (:PERIOD 46)
  (:SLASH 47)
  (:0 48)
  (:1 49)
  (:2 50)
  (:3 51)
  (:4 52)
  (:5 53)
  (:6 54)
  (:7 55)
  (:8 56)
  (:9 57)
  (:COLON 58)
  (:SEMICOLON 59)
  (:LESS 60)
  (:EQUALS 61)
  (:GREATER 62)
  (:QUESTION 63)
  (:AT 64)
  (:LEFTBRACKET 91)
  (:BACKSLASH 92)
  (:RIGHTBRACKET 93)
  (:CARET 94)
  (:UNDERSCORE 95)
  (:BACKQUOTE 96)
  (:a 97)
  (:b 98)
  (:c 99)
  (:d 100)
  (:e 101)
  (:f 102)
  (:g 103)
  (:h 104)
  (:i 105)
  (:j 106)
  (:k 107)
  (:l 108)
  (:m 109)
  (:n 110)
  (:o 111)
  (:p 112)
  (:q 113)
  (:r 114)
  (:s 115)
  (:t 116)
  (:u 117)
  (:v 118)
  (:w 119)
  (:x 120)
  (:y 121)
  (:z 122)
  (:DELETE 127)
  (:WORLD-0 160)
  (:WORLD-1 161)
  (:WORLD-2 162)
  (:WORLD-3 163)
  (:WORLD-4 164)
  (:WORLD-5 165)
  (:WORLD-6 166)
  (:WORLD-7 167)
  (:WORLD-8 168)
  (:WORLD-9 169)
  (:WORLD-10 170)
  (:WORLD-11 171)
  (:WORLD-12 172)
  (:WORLD-13 173)
  (:WORLD-14 174)
  (:WORLD-15 175)
  (:WORLD-16 176)
  (:WORLD-17 177)
  (:WORLD-18 178)
  (:WORLD-19 179)
  (:WORLD-20 180)
  (:WORLD-21 181)
  (:WORLD-22 182)
  (:WORLD-23 183)
  (:WORLD-24 184)
  (:WORLD-25 185)
  (:WORLD-26 186)
  (:WORLD-27 187)
  (:WORLD-28 188)
  (:WORLD-29 189)
  (:WORLD-30 190)
  (:WORLD-31 191)
  (:WORLD-32 192)
  (:WORLD-33 193)
  (:WORLD-34 194)
  (:WORLD-35 195)
  (:WORLD-36 196)
  (:WORLD-37 197)
  (:WORLD-38 198)
  (:WORLD-39 199)
  (:WORLD-40 200)
  (:WORLD-41 201)
  (:WORLD-42 202)
  (:WORLD-43 203)
  (:WORLD-44 204)
  (:WORLD-45 205)
  (:WORLD-46 206)
  (:WORLD-47 207)
  (:WORLD-48 208)
  (:WORLD-49 209)
  (:WORLD-50 210)
  (:WORLD-51 211)
  (:WORLD-52 212)
  (:WORLD-53 213)
  (:WORLD-54 214)
  (:WORLD-55 215)
  (:WORLD-56 216)
  (:WORLD-57 217)
  (:WORLD-58 218)
  (:WORLD-59 219)
  (:WORLD-60 220)
  (:WORLD-61 221)
  (:WORLD-62 222)
  (:WORLD-63 223)
  (:WORLD-64 224)
  (:WORLD-65 225)
  (:WORLD-66 226)
  (:WORLD-67 227)
  (:WORLD-68 228)
  (:WORLD-69 229)
  (:WORLD-70 230)
  (:WORLD-71 231)
  (:WORLD-72 232)
  (:WORLD-73 233)
  (:WORLD-74 234)
  (:WORLD-75 235)
  (:WORLD-76 236)
  (:WORLD-77 237)
  (:WORLD-78 238)
  (:WORLD-79 239)
  (:WORLD-80 240)
  (:WORLD-81 241)
  (:WORLD-82 242)
  (:WORLD-83 243)
  (:WORLD-84 244)
  (:WORLD-85 245)
  (:WORLD-86 246)
  (:WORLD-87 247)
  (:WORLD-88 248)
  (:WORLD-89 249)
  (:WORLD-90 250)
  (:WORLD-91 251)
  (:WORLD-92 252)
  (:WORLD-93 253)
  (:WORLD-94 254)
  (:WORLD-95 255)
  (:KP0 256)
  (:KP1 257)
  (:KP2 258)
  (:KP3 259)
  (:KP4 260)
  (:KP5 261)
  (:KP6 262)
  (:KP7 263)
  (:KP8 264)
  (:KP9 265)
  (:KP-PERIOD 266)
  (:KP-DIVIDE 267)
  (:KP-MULTIPLY 268)
  (:KP-MINUS 269)
  (:KP-PLUS 270)
  (:KP-ENTER 271)
  (:KP-EQUALS 272)
  (:UP 273)
  (:DOWN 274)
  (:RIGHT 275)
  (:LEFT 276)
  (:INSERT 277)
  (:HOME 278)
  (:END 279)
  (:PAGEUP 280)
  (:PAGEDOWN 281)
  (:F1 282)
  (:F2 283)
  (:F3 284)
  (:F4 285)
  (:F5 286)
  (:F6 287)
  (:F7 288)
  (:F8 289)
  (:F9 290)
  (:F10 291)
  (:F11 292)
  (:F12 293)
  (:F13 294)
  (:F14 295)
  (:F15 296)
  (:NUMLOCK 300)
  (:CAPSLOCK 301)
  (:SCROLLOCK 302)
  (:RSHIFT 303)
  (:LSHIFT 304)
  (:RCTRL 305)
  (:LCTRL 306)
  (:RALT 307)
  (:LALT 308)
  (:RMETA 309)
  (:LMETA 310)
  (:LSUPER 311)
  (:RSUPER 312)
  (:MODE 313)
  (:COMPOSE 314)
  (:HELP 315)
  (:PRINT 316)
  (:SYSREQ 317)
  (:BREAK 318)
  (:MENU 319)
  (:POWER 320)
  (:EURO 321)
  (:UNDO 322)))

(defparameter *sdl-key-identifiers*
  '((:SDL-KEY-FIRST 0)
  (:SDL-KEY-BACKSPACE 8)
  (:SDL-KEY-TAB 9)
  (:SDL-KEY-CLEAR 12)
  (:SDL-KEY-RETURN 13)
  (:SDL-KEY-PAUSE 19)
  (:SDL-KEY-ESCAPE 27)
  (:SDL-KEY-SPACE 32)
  (:SDL-KEY-EXCLAIM 33)
  (:SDL-KEY-QUOTEDBL 34)
  (:SDL-KEY-HASH 35)
  (:SDL-KEY-DOLLAR 36)
  (:SDL-KEY-AMPERSAND 38)
  (:SDL-KEY-QUOTE 39)
  (:SDL-KEY-LEFTPAREN 40)
  (:SDL-KEY-RIGHTPAREN 41)
  (:SDL-KEY-ASTERISK 42)
  (:SDL-KEY-PLUS 43)
  (:SDL-KEY-COMMA 44)
  (:SDL-KEY-MINUS 45)
  (:SDL-KEY-PERIOD 46)
  (:SDL-KEY-SLASH 47)
  (:SDL-KEY-0 48)
  (:SDL-KEY-1 49)
  (:SDL-KEY-2 50)
  (:SDL-KEY-3 51)
  (:SDL-KEY-4 52)
  (:SDL-KEY-5 53)
  (:SDL-KEY-6 54)
  (:SDL-KEY-7 55)
  (:SDL-KEY-8 56)
  (:SDL-KEY-9 57)
  (:SDL-KEY-COLON 58)
  (:SDL-KEY-SEMICOLON 59)
  (:SDL-KEY-LESS 60)
  (:SDL-KEY-EQUALS 61)
  (:SDL-KEY-GREATER 62)
  (:SDL-KEY-QUESTION 63)
  (:SDL-KEY-AT 64)
  (:SDL-KEY-LEFTBRACKET 91)
  (:SDL-KEY-BACKSLASH 92)
  (:SDL-KEY-RIGHTBRACKET 93)
  (:SDL-KEY-CARET 94)
  (:SDL-KEY-UNDERSCORE 95)
  (:SDL-KEY-BACKQUOTE 96)
  (:SDL-KEY-a 97)
  (:SDL-KEY-b 98)
  (:SDL-KEY-c 99)
  (:SDL-KEY-d 100)
  (:SDL-KEY-e 101)
  (:SDL-KEY-f 102)
  (:SDL-KEY-g 103)
  (:SDL-KEY-h 104)
  (:SDL-KEY-i 105)
  (:SDL-KEY-j 106)
  (:SDL-KEY-k 107)
  (:SDL-KEY-l 108)
  (:SDL-KEY-m 109)
  (:SDL-KEY-n 110)
  (:SDL-KEY-o 111)
  (:SDL-KEY-p 112)
  (:SDL-KEY-q 113)
  (:SDL-KEY-r 114)
  (:SDL-KEY-s 115)
  (:SDL-KEY-t 116)
  (:SDL-KEY-u 117)
  (:SDL-KEY-v 118)
  (:SDL-KEY-w 119)
  (:SDL-KEY-x 120)
  (:SDL-KEY-y 121)
  (:SDL-KEY-z 122)
  (:SDL-KEY-DELETE 127)
  (:SDL-KEY-WORLD-0 160)
  (:SDL-KEY-WORLD-1 161)
  (:SDL-KEY-WORLD-2 162)
  (:SDL-KEY-WORLD-3 163)
  (:SDL-KEY-WORLD-4 164)
  (:SDL-KEY-WORLD-5 165)
  (:SDL-KEY-WORLD-6 166)
  (:SDL-KEY-WORLD-7 167)
  (:SDL-KEY-WORLD-8 168)
  (:SDL-KEY-WORLD-9 169)
  (:SDL-KEY-WORLD-10 170)
  (:SDL-KEY-WORLD-11 171)
  (:SDL-KEY-WORLD-12 172)
  (:SDL-KEY-WORLD-13 173)
  (:SDL-KEY-WORLD-14 174)
  (:SDL-KEY-WORLD-15 175)
  (:SDL-KEY-WORLD-16 176)
  (:SDL-KEY-WORLD-17 177)
  (:SDL-KEY-WORLD-18 178)
  (:SDL-KEY-WORLD-19 179)
  (:SDL-KEY-WORLD-20 180)
  (:SDL-KEY-WORLD-21 181)
  (:SDL-KEY-WORLD-22 182)
  (:SDL-KEY-WORLD-23 183)
  (:SDL-KEY-WORLD-24 184)
  (:SDL-KEY-WORLD-25 185)
  (:SDL-KEY-WORLD-26 186)
  (:SDL-KEY-WORLD-27 187)
  (:SDL-KEY-WORLD-28 188)
  (:SDL-KEY-WORLD-29 189)
  (:SDL-KEY-WORLD-30 190)
  (:SDL-KEY-WORLD-31 191)
  (:SDL-KEY-WORLD-32 192)
  (:SDL-KEY-WORLD-33 193)
  (:SDL-KEY-WORLD-34 194)
  (:SDL-KEY-WORLD-35 195)
  (:SDL-KEY-WORLD-36 196)
  (:SDL-KEY-WORLD-37 197)
  (:SDL-KEY-WORLD-38 198)
  (:SDL-KEY-WORLD-39 199)
  (:SDL-KEY-WORLD-40 200)
  (:SDL-KEY-WORLD-41 201)
  (:SDL-KEY-WORLD-42 202)
  (:SDL-KEY-WORLD-43 203)
  (:SDL-KEY-WORLD-44 204)
  (:SDL-KEY-WORLD-45 205)
  (:SDL-KEY-WORLD-46 206)
  (:SDL-KEY-WORLD-47 207)
  (:SDL-KEY-WORLD-48 208)
  (:SDL-KEY-WORLD-49 209)
  (:SDL-KEY-WORLD-50 210)
  (:SDL-KEY-WORLD-51 211)
  (:SDL-KEY-WORLD-52 212)
  (:SDL-KEY-WORLD-53 213)
  (:SDL-KEY-WORLD-54 214)
  (:SDL-KEY-WORLD-55 215)
  (:SDL-KEY-WORLD-56 216)
  (:SDL-KEY-WORLD-57 217)
  (:SDL-KEY-WORLD-58 218)
  (:SDL-KEY-WORLD-59 219)
  (:SDL-KEY-WORLD-60 220)
  (:SDL-KEY-WORLD-61 221)
  (:SDL-KEY-WORLD-62 222)
  (:SDL-KEY-WORLD-63 223)
  (:SDL-KEY-WORLD-64 224)
  (:SDL-KEY-WORLD-65 225)
  (:SDL-KEY-WORLD-66 226)
  (:SDL-KEY-WORLD-67 227)
  (:SDL-KEY-WORLD-68 228)
  (:SDL-KEY-WORLD-69 229)
  (:SDL-KEY-WORLD-70 230)
  (:SDL-KEY-WORLD-71 231)
  (:SDL-KEY-WORLD-72 232)
  (:SDL-KEY-WORLD-73 233)
  (:SDL-KEY-WORLD-74 234)
  (:SDL-KEY-WORLD-75 235)
  (:SDL-KEY-WORLD-76 236)
  (:SDL-KEY-WORLD-77 237)
  (:SDL-KEY-WORLD-78 238)
  (:SDL-KEY-WORLD-79 239)
  (:SDL-KEY-WORLD-80 240)
  (:SDL-KEY-WORLD-81 241)
  (:SDL-KEY-WORLD-82 242)
  (:SDL-KEY-WORLD-83 243)
  (:SDL-KEY-WORLD-84 244)
  (:SDL-KEY-WORLD-85 245)
  (:SDL-KEY-WORLD-86 246)
  (:SDL-KEY-WORLD-87 247)
  (:SDL-KEY-WORLD-88 248)
  (:SDL-KEY-WORLD-89 249)
  (:SDL-KEY-WORLD-90 250)
  (:SDL-KEY-WORLD-91 251)
  (:SDL-KEY-WORLD-92 252)
  (:SDL-KEY-WORLD-93 253)
  (:SDL-KEY-WORLD-94 254)
  (:SDL-KEY-WORLD-95 255)
  (:SDL-KEY-KP0 256)
  (:SDL-KEY-KP1 257)
  (:SDL-KEY-KP2 258)
  (:SDL-KEY-KP3 259)
  (:SDL-KEY-KP4 260)
  (:SDL-KEY-KP5 261)
  (:SDL-KEY-KP6 262)
  (:SDL-KEY-KP7 263)
  (:SDL-KEY-KP8 264)
  (:SDL-KEY-KP9 265)
  (:SDL-KEY-KP-PERIOD 266)
  (:SDL-KEY-KP-DIVIDE 267)
  (:SDL-KEY-KP-MULTIPLY 268)
  (:SDL-KEY-KP-MINUS 269)
  (:SDL-KEY-KP-PLUS 270)
  (:SDL-KEY-KP-ENTER 271)
  (:SDL-KEY-KP-EQUALS 272)
  (:SDL-KEY-UP 273)
  (:SDL-KEY-DOWN 274)
  (:SDL-KEY-RIGHT 275)
  (:SDL-KEY-LEFT 276)
  (:SDL-KEY-INSERT 277)
  (:SDL-KEY-HOME 278)
  (:SDL-KEY-END 279)
  (:SDL-KEY-PAGEUP 280)
  (:SDL-KEY-PAGEDOWN 281)
  (:SDL-KEY-F1 282)
  (:SDL-KEY-F2 283)
  (:SDL-KEY-F3 284)
  (:SDL-KEY-F4 285)
  (:SDL-KEY-F5 286)
  (:SDL-KEY-F6 287)
  (:SDL-KEY-F7 288)
  (:SDL-KEY-F8 289)
  (:SDL-KEY-F9 290)
  (:SDL-KEY-F10 291)
  (:SDL-KEY-F11 292)
  (:SDL-KEY-F12 293)
  (:SDL-KEY-F13 294)
  (:SDL-KEY-F14 295)
  (:SDL-KEY-F15 296)
  (:SDL-KEY-NUMLOCK 300)
  (:SDL-KEY-CAPSLOCK 301)
  (:SDL-KEY-SCROLLOCK 302)
  (:SDL-KEY-RSHIFT 303)
  (:SDL-KEY-LSHIFT 304)
  (:SDL-KEY-RCTRL 305)
  (:SDL-KEY-LCTRL 306)
  (:SDL-KEY-RALT 307)
  (:SDL-KEY-LALT 308)
  (:SDL-KEY-RMETA 309)
  (:SDL-KEY-LMETA 310)
  (:SDL-KEY-LSUPER 311)
  (:SDL-KEY-RSUPER 312)
  (:SDL-KEY-MODE 313)
  (:SDL-KEY-COMPOSE 314)
  (:SDL-KEY-HELP 315)
  (:SDL-KEY-PRINT 316)
  (:SDL-KEY-SYSREQ 317)
  (:SDL-KEY-BREAK 318)
  (:SDL-KEY-MENU 319)
  (:SDL-KEY-POWER 320)
  (:SDL-KEY-EURO 321)
  (:SDL-KEY-UNDO 322)))
  
(defparameter *key-modifiers*
  '((:NONE #x0000)
    (:LSHIFT #x0001)
    (:RSHIFT #x0002)
    (:LCTRL #x0040)
    (:RCTRL #x0080)
    (:LALT #x0100)
    (:RALT #x0200)
    (:LMETA #x0400)
    (:RMETA #x0800)
    (:NUM #x1000)
    (:CAPS #x2000)
    (:MODE #x4000)
    (:RESERVED #x8000)))

(defparameter *sdl-key-modifiers*
  '((:SDL-KEY-MOD-NONE #x0000)
    (:SDL-KEY-MOD-LSHIFT #x0001)
    (:SDL-KEY-MOD-RSHIFT #x0002)
    (:SDL-KEY-MOD-LCTRL #x0040)
    (:SDL-KEY-MOD-RCTRL #x0080)
    (:SDL-KEY-MOD-LALT #x0100)
    (:SDL-KEY-MOD-RALT #x0200)
    (:SDL-KEY-MOD-LMETA #x0400)
    (:SDL-KEY-MOD-RMETA #x0800)
    (:SDL-KEY-MOD-NUM #x1000)
    (:SDL-KEY-MOD-CAPS #x2000)
    (:SDL-KEY-MOD-MODE #x4000)
    (:SDL-KEY-MOD-RESERVED #x8000)))

(defun keyboard-id (key)
  (let ((entry (find key *sdl-key-identifiers* :key #'car)))
    (second entry)))

(defun keyboard-mod (mod)
  (let ((entry (find mod *sdl-key-modifiers* :key #'car)))
    (second entry)))

(defun keyboard-held-p (key) 
  "Returns the duration in seconds that the key has been depressed over a number of game loops."
  (sdl:key-held-p (keyboard-id key)))

(defun keyboard-pressed-p (key)
  "Returns t if the key has just been depressed in the current game loop."
  (sdl:key-pressed-p (keyboard-id key)))

(defun keyboard-released-p (key)
  "Returns t if the key has just been released in the current game loop."
  (sdl:key-released-p (keyboard-id key)))

(defun keyboard-time-in-current-state (key)
  "Returns the duration in seconds that key is either pressed or depressed."
  (sdl:key-time-in-current-state (keyboard-id key)))

(defun keyboard-time-in-previous-state (key)
  "Returns the duration in seconds that key was in its previous state either pressed or depressed."
  (sdl:key-time-in-previous-state (keyboard-id key)))

(defun keyboard-down-p (key)
  "Returns t if the key is depressed."
  (sdl:key-down-p (keyboard-id key)))

(defun keyboard-keys-down ()
  "Returns a list of the keys that are depressed."
  (labels ((translate (key)
	     (let ((entry (find key *sdl-key-identifiers* :key #'first)))
	       (let ((entry2 (find (second entry) *key-identifiers* :key #'second)))
		 (first entry2)))))
    (mapcar #'translate (sdl:keys-down-p))))

(defun keyboard-modifier-down-p (mod)
  "Returns t if the modifier key is depressed."
  (sdl:mod-down-p (keyboard-mod mod)))

(defun keyboard-modifiers () 
  "Returns a list of the modifier keys that are depressed."
  (labels ((translate (mod)
	     (let ((entry (find mod *sdl-key-modifiers* :key #'first)))
	       (let ((entry2 (find (second entry) *key-modifiers* :key #'second)))
		 (first entry2)))))
    (mapcar #'translate (sdl:mods-down-p))))

;;; Engine status

(defun quit (&optional shutdown)
  (when shutdown 
    (setf *quitting* t))
  (setf *next-module* nil)
  (sdl:push-quit-event))

(defun reset (&optional (module-name "standard"))
  (setf *quitting* nil)
  (setf *next-module* module-name)
  (sdl:push-quit-event))

(defvar *copyright-text*
"XE2 Game Engine
Copyright (C) 2006, 2007, 2008, 2009, 2010 David O'Toole
<dto@gnu.org>

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
 (at your option) any later version.

This program is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>.

This program uses libSDL 1.2 (Simple Direct Media Layer), which is
provided under the terms of the GNU Lesser General Public License. See
also the file LIBSDL-LICENSE for details.
")

(defvar *library-search-paths-setup-hook* nil)

(defun setup-library-search-paths ()
(run-hook '*library-search-paths-setup-hook*)
#+darwin (setf cffi:*foreign-library-directories*
	       (union cffi:*foreign-library-directories*
		      '(#P"/opt/local/lib" #P"/sw/lib/")
		      :test #'equal)))

(defvar *play-args* nil)

(defun play (&optional (module-name "standard") &rest args)
  "This is the main entry point to XE2. MODULE-NAME is loaded 
and its .startup resource is loaded."
  (format t "~A" *copyright-text*)
  (initialize-resource-table)
  (setf *module-package-name* nil)
  (setf *physics-function* nil)
  (setf *initialization-hook* nil)
  (setf *play-args* args)
  (setf *random-state* (make-random-state t))
  ;; override module to play?
  (setf *next-module* module-name)
  ;; add library search paths for Mac if needed
  (setup-library-search-paths)
 ;; now play modules until done
  (loop while (and (not *quitting*)
		   *next-module*)
     do (unwind-protect
	     ;; dynamically load libs (needed for GNU/Linux but bad on windows)
	       (progn 
		 (unless *windows*
		   (cffi:define-foreign-library sdl
		     (:darwin (:or (:framework "SDL")
				   (:default "libSDL")))
		     (:unix (:or "libSDL-1.2.so.0.7.2"
				 "libSDL-1.2.so.0"
				 "libSDL-1.2.so"
				 "libSDL.so"
				 "libSDL")))
		   (cffi:use-foreign-library sdl)
		   ;;
		   (cffi:define-foreign-library sdl-mixer
		     (:darwin (:or (:framework "SDL_mixer")
				   (:default "libSDL_mixer")))
		     (:unix (:or "libSDL_mixer-1.2.so.0.7.2"
				 "libSDL_mixer-1.2.so.0"
				 "libSDL_mixer-1.2.so"
				 "libsdl_mixer-1.2.so.0.2.6" ;; eeebuntu?
				 "libSDL_mixer.so"
				 "libSDL_mixer")))
		   (cffi:use-foreign-library sdl-mixer)
		   ;;
		   (cffi:define-foreign-library sdl-gfx
		     (:darwin (:or (:framework "SDL_gfx")
				   (:default "libSDL_gfx")))
		     (:unix (:or "libSDL_gfx-1.2.so.0.7.2"
				 "libSDL_gfx-1.2.so.0"
				 "libSDL_gfx-1.2.so"
				 "libSDL_gfx.so.4"
				 "libSDL_gfx.so.13"
				 "libSDL_gfx.so"
				 "libSDL_gfx")))
		   (cffi:use-foreign-library sdl-gfx)
		   ;;
		   (cffi:define-foreign-library sdl-image
			(:darwin (:or (:framework "SDL_image")
				      (:default "libSDL_image")))
			(:unix (:or "libSDL_image-1.2.so.0.7.2"
				    "libSDL_image-1.2.so.0"
				    "libSDL_image-1.2.so.0.1.5" ;; eeebuntu?
				    "libSDL_image-1.2.so"
				    "libSDL_image.so"
				    "libSDL_image")))
		   (cffi:use-foreign-library sdl-image))
		 ;;
		 (sdl:with-init (sdl:SDL-INIT-VIDEO sdl:SDL-INIT-AUDIO sdl:SDL-INIT-JOYSTICK)
		   (load-user-init-file)	
		   (initialize-resource-table)
		   (initialize-colors)
		   (when *use-sound*
		     ;; try opening sound
		     (when (null (sdl-mixer:open-audio :frequency *frequency*
						       :chunksize *output-chunksize*
						       :format *sample-format*
						       :channels *output-channels*))
		       ;; if that didn't work, disable effects/music
		       (message "Could not open audio driver. Disabling sound effects and music.")
		       (setf *use-sound* nil))
		     ;; set to mix lots of sounds
		     (sdl-mixer:allocate-channels *channels*))
			(index-module "standard") 
			(load-module *next-module*)
		      (find-resource *startup*)
		      (run-main-loop)))
	  ;; close audio if crash
	  (when *use-sound* 
	    (sdl-mixer:close-audio t)))
	  (setf *quitting* t))
	(setf *quitting* nil)
	(when *use-sound* 
    (sdl-mixer:close-audio t)))
  ;; ;; free audio
  ;; (maphash #'(lambda (name resource)
  ;; 	       (declare (ignore name))
  ;; 	       (when (eq :music (resource-type resource))
  ;; 		 (sdl-mixer:free (resource-object resource))))
  ;; 	   *resource-table*))

;;; console.lisp ends here
