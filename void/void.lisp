
;; [[file:~/xe2/void/void.org][xe2-lisp-file]]
;; Copyright (C) 2010  David O'Toole

;; Author: David O'Toole <dto@gnu.org>
;; Keywords: 

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
(defpackage :void
  (:use :xe2 :common-lisp)
  (:export physics))

(in-package :void)
(defparameter *timestep* 20)
(defparameter *grid-size* 16)
(defparameter *address* '(=cloud=))
(defparameter *width* 1280)
(defparameter *height* 720)
(defvar *form*)
(defvar *pager*)
(defvar *prompt*)
(defvar *player*)
(defvar *viewport*)
(defparameter *numpad-keybindings* 
  '(("KP8" nil "move :north .")
    ("KP4" nil "move :west .")
    ("KP6" nil "move :east .")
    ("KP2" nil "move :south .")
    ;; 
    ("UP" nil "move :north .")
    ("LEFT" nil "move :west .")
    ("RIGHT" nil "move :east .")
    ("DOWN" nil "move :south .")
    ;; 
    ("KP8" (:shift) "move :north .")
    ("KP4" (:shift) "move :west .")
    ("KP6" (:shift) "move :east .")
    ("KP2" (:shift) "move :south .")
    ;; 
    ("UP" (:shift) "move :north .")
    ("LEFT" (:shift) "move :west .")
    ("RIGHT" (:shift) "move :east .")
    ("DOWN" (:shift) "move :south .")))

(defparameter *qwerty-keybindings*
  (append *numpad-keybindings*
          '(("K" nil "move :north .")
            ("H" nil "move :west .")
            ("L" nil "move :east .")
            ("J" nil "move :south .")
            ;;
            ("K" (:shift) "move :north .")
            ("H" (:shift) "move :west .")
            ("L" (:shift) "move :east .")
            ("J" (:shift) "move :south .")
            ;;
            ("Z" nil "rotate .")
            ("X" nil "act .")
            ("C" nil "pop .")
            ("0" (:control) "do-exit .")
            ;;
            ("P" (:control) "pause .")
            ("PAUSE" nil "pause .")
            ("ESCAPE" nil "restart .")
            ("Q" (:control) "quit ."))))
  
(define-prototype void-prompt (:parent xe2:=prompt=))

(define-method install-keybindings void-prompt ()
  (dolist (k *qwerty-keybindings*)
    (apply #'bind-key-to-prompt-insertion self k)))

;; (define-method install-keybindings void-prompt ()
;;   (let ((keys (ecase xe2:*user-keyboard-layout* 
;;              (:qwerty *qwerty-keybindings*)
;;              (:alternate-qwerty *alternate-qwerty-keybindings*)
;;              (:dvorak *dvorak-keybindings*))))
;;     (dolist (k keys)
;;       (apply #'bind-key-to-prompt-insertion self k))))
(defun physics (&rest ignore)
  (when *world* [run-cpu-phase *world* t]))
(defcell vaccuum 
  (tile :initform "vaccuum"))

(defcell red-plasma
  (tile :initform "red-plasma"))

(defcell blue-plasma
  (tile :initform "blue-plasma"))

(define-prototype cloud (:parent xe2:=world=)
  (name :initform "DVO UV Shield Cloud")
  (scale :initform '(50 m))
  (ambient-light :initform :total)
  (description :initform "foo"))

(define-method begin-ambient-loop cloud ()
  (play-music "passageway" :loop t))

(define-method drop-plasma cloud
  (&optional &key (object =red-plasma=)
             distance 
             (row 0) (column 0)
             (graininess 0.3)
             (density 100)
             (cutoff 0))
  (clon:with-field-values (height width) self
    (let* ((h0 (or distance height))
           (w0 (or distance width))
           (r0 (- row (truncate (/ h0 2))))
           (c0 (- column (truncate (/ w0 2))))
           (plasma (xe2:render-plasma h0 w0 :graininess graininess))
           (value nil))
      (dotimes (i h0)
        (dotimes (j w0)
          (setf value (aref plasma i j))
          (when (< cutoff value)
            (when (or (null distance)
                      (< (distance (+ j r0) (+ c0 i) row column) distance))
              (percent-of-time density
                [drop-cell self (clone object) (+ r0 i) (+ c0 j) :no-collisions t]))))))))

(define-method generate cloud (&key (height 100)
                                       (width 100)
                                       (protostars 30)
                                       (sequence-number (genseq)))
  (setf <height> height <width> width)
  [create-default-grid self]
  (dotimes (i width)
    (dotimes (j height)
      [drop-cell self (clone (if (zerop (random 7))
                                 =vaccuum= 
                                 =blue-plasma=))
                 i j]))
  [drop-plasma self]
  ;; (dotimes (i protostars)
  ;;   (let ((r (random height))
  ;;      (c (random width)))
  ;;     [drop-plasma self :object =protogas= :distance 12 :row r :column c :graininess 0.3]
  ;;     [drop-plasma self :object =crystal= :density 7 :distance 16 :row r :column c :graininess 0.3]
  ;;     [drop-cell self (clone =protostar=) r c]))
  [drop-cell self (clone =launchpad=) (random height) (random width)])
(defparameter *react-shield-time* 30)

(defparameter *energy-recovery-interval* 200)

(defcell agent 
  (tile :initform "voyager")
  (firing :initform nil)
  (items :initform nil)
  (direction :initform :north)
  (last-direction :initform :north :documentation "Last direction actually moved.")
  (dead :initform nil)
  (last-turn-moved :initform 0)
  (team :initform :player)
  (call-clock :initform 0)
  (call-interval :initform 7)
  (hit-points :initform (make-stat :base 20 :min 0 :max 20))
  (energy :initform (make-stat :base 80 :min 0 :max 80))
  (oxygen :initform (make-stat :base 80 :min 0 :max 80))
  (movement-cost :initform (make-stat :base 10))
  (speed :initform (make-stat :base 10 :min 0 :max 10))
  (hearing-range :initform 25)
  (stepping :initform t)
  (light-radius :initform 7)
  (react-shield-clock :initform 0)
  (energy-clock :initform *energy-recovery-interval*)
  (categories :initform '(:actor :obstacle :player :target :container :light-source)))

(define-method loadout agent ()
  [emote self '((("I'd better get moving."))
                (("Use the arrow keys (or numpad)"))
                (("to move, and SHIFT to fire.")))])

(define-method start agent ()
  (clon:with-fields (segments) self
    (setf <direction> :north)
    (setf <last-direction> :north)
    (if (field-value :overworld *world*)
        (progn (setf <tile> "player32")
               (unless <first-start>
                 (setf <first-start> t)
                 ;; enter the first room on the map. FIXME
                 [act self]))
        (clon:with-field-values (row column) self
          (setf <tile> "agent-north")
          [make-segments self]))))

(define-method expend-energy agent (points)
  (if (>= [stat-value self :energy] points)
      (prog1 t [stat-effect self :energy (- points)])
      (prog1 nil 
        [say self "Insufficient energy."]
        [play-sample self "error"])))

(define-method damage agent (points)
  (if (zerop <react-shield-clock>)
      (labels ((do-circle (image)
                 (prog1 t
                   (multiple-value-bind (x y) 
                       [image-coordinates self]
                     (let ((x0 (+ x 8))
                           (y0 (+ y 8)))
                       (draw-circle x0 y0 25 :destination image)
                       (draw-circle x0 y0 30 :destination image)
                       (draw-circle x0 y0 35 :destination image)
                       (draw-circle x0 y0 40 :destination image))))))
        (setf <react-shield-clock> *react-shield-time*)
        [play-sample self "shield-warning"]
        [>>add-overlay :viewport #'do-circle]
        [parent>>damage self points])
      [play-sample self "ice"]))
  
(define-method pause agent ()
  [pause *world*])

(defparameter *agent-tiles* '(:north "agent-north"
                             :south "agent-south"
                             :east "agent-east"
                             :west "agent-west"))

(define-method aim agent (direction)
  (setf <direction> direction)
  (setf <tile> (getf *agent-tiles* direction)))

(define-method move agent (&optional direction)
  (unless <dead>
    (let ((phase (field-value :phase-number *world*))
          (dir (or direction <direction>)))
      (unless (= <last-turn-moved> phase)
        (setf <last-turn-moved> phase)
        [aim self dir]
        (when [parent>>move self dir]
          (setf <last-direction> dir))))))

(define-method space-at-head agent ()
  (values <row> <column>))

(define-method category-at-head agent (category)
  (multiple-value-bind (row column) 
      [space-at-head self]
    [category-at-p *world* row column category]))

(define-method item-at-head agent ()
  [category-at-head self :item])

(define-method obstacle-at-head agent ()
  [category-at-head self :obstacle])
  
(define-method push agent () 
  (unless <dead>
    (if (= (length <items>) <tail-length>)
        (progn 
          [say self "Maximum capacity reached."]
          [play-sample self "error"])
        (let ((item [item-at-head self]))
          (if item
              (progn (setf <items> (append <items> (list item)))
                     [play-sample self "doorbell"]
                     [print-items self]
                     [delete-from-world item])
              [say self "Nothing to push."])))))
        
(define-method pop agent ()
  (unless (or <dead> [in-overworld self])
    (clon:with-fields (items) self
      (multiple-value-bind (row column)
          [space-at-head self]
        (let ((item (car items)))
          (if (clon:object-p item)
              (progn (setf items (delete item items))
                     [play-sample self "doorbell2"]
                     [drop-cell *world* item row column]
                     [print-items self])
              [say self "Nothing to drop."]))))))
  
(define-method act agent ()
  (unless <dead>
    (let ((gateway [category-at-p *world* <row> <column> :gateway]))
      (if (clon:object-p gateway)
          [activate gateway]
          (cond ([category-at-head self :action]
                 [do-action [category-at-head self :action]])
                ([category-at-head self :item]
                 [push self])
                (t 
                 [play-sample self "error"]
                 [say self "Nothing to do here."]))))))

(define-method expend-item agent ()
  (pop <items>)
  [print-items self])

(define-method rotate agent () 
  (unless <dead>
    (clon:with-fields (items) self
      (if items
          (let ((tail (car (last items)))
                (newlist (butlast items)))
            [play-sample self "doorbell3"]
            (setf items (cons tail newlist))
            [print-items self])
          (progn 
            [play-sample self "error"]
            [say self "Cannot rotate empty list."])))))

(define-method call agent (&optional direction)
  (unless <dead>
    (when (zerop <call-clock>)
      (when direction
        [aim self direction])
      (let ((item (car <items>)))
        (if (and item [in-category item :item]
                 (clon:has-method :call item))
            (progn 
              (when [expend-energy self (field-value :energy-cost item)]
                [call item self]
                (setf <call-clock> (field-value :call-interval item))))
            [say self "Cannot call."])))))

(define-method print-items agent ()
  (labels ((print-item (item)
             [>>print :narrator nil :image (field-value :tile item)]
             [>>print :narrator "  "]
             [>>print :narrator (get-some-object-name item)]
             [>>print :narrator "  "])
           (newline ()
             [>>newline :narrator]))
    [>>print :narrator " ITEMS: "]
    (dolist (item <items>)
      (print-item item))
    (newline)))
      
(define-method run agent () 
;;  [update-tiles self]
  (when (plusp <call-clock>)
    (decf <call-clock>))
  (when (plusp <energy-clock>)
    (decf <energy-clock>))
  (when (zerop <energy-clock>)
    (setf <energy-clock> *energy-recovery-interval*)
    [stat-effect self :energy 1])
  (when (plusp <react-shield-clock>)
    (decf <react-shield-clock>)
    [play-sample self "shield-sound"]
    (labels ((do-circle (image)
               (prog1 t
                 (multiple-value-bind (x y) 
                     [image-coordinates self]
                   (let ((x0 (+ x 8))
                         (y0 (+ y 8)))
                     (draw-circle x0 y0 (+ 25 (random 3)) :destination image :color (car (one-of (list ".cyan" ".hot pink" ".white"))))
                     (draw-circle x0 y0 (+ 30 (random 3))  :destination image :color (car (one-of (list ".cyan" ".hot pink" ".white")))))))))
      [>>add-overlay :viewport #'do-circle]))
  (when (or (keyboard-modifier-down-p :lshift)
            (keyboard-modifier-down-p :rshift))
    [call self <direction>])
  (dolist (item <items>)
    (when [in-category item :actor]
      [run item])))

(define-method quit agent ()
  (xe2:quit :shutdown))

(define-method do-exit agent ()
  [exit *universe*])

(define-method exit agent ()
  (dolist (segment <segments>)
    [die segment])
  (setf <segments> nil))

(define-method die agent ()
      (unless <dead>
    (setf <tile> "agent-disabled")
    (dolist (segment <segments>)
      [die segment])
    (setf <segments> nil)
    (dotimes (n 30)
      [drop self (clone =explosion=)])
    [play-sample self "gameover"]
    [say self "You died. Press escape to reset."]
    (setf <dead> t)))

(define-method restart agent ()
  (let ((agent (clone =agent=)))
    [say self "Restarting CONS..."]
    (halt-sample t)
    (setf *player* agent)
    [destroy *universe*]
    [set-player *universe* agent]
;;    [set-prompt *form* agent]
    [set-character *status* agent]
    [play *universe*
          :address (list '=zeta-x= :sequence-number (genseq))]
    [loadout agent]))

;;; Player upgrade

(defcell tail-defun 
  (name :initform "Body Extender Segment")
  (tile :initform "tail-defun")
  (call-interval :initform 20)
  (energy-cost :initform 0)
  (categories :initform '(:item :target :defun)))

(define-method call tail-defun (caller)
  [upgrade caller]
  [expend-item caller])
(defgame :void
    (:title "Void Mission"
     :description "A sci-fi roguelike game in Common Lisp."
     :creator "David T. O'Toole <dto@gnu.org>"
     :screen-width *width*
     :screen-height *height*
     :timestep *timestep*
     :physics-function #'void:physics)
  ;; create some objects
  (setf *prompt* (prompt (clone =void-prompt=)))
  (setf *universe* (clone =universe=))
  (setf *player* (clone =agent=))
  (setf *viewport* (clone =viewport=))
  ;; configure the view
  [resize viewport :height *height* :width *width*]
  [move viewport :x 0 :y 0]
  [set-origin viewport :x 0 :y 0 
              :height (truncate (/ *height* *grid-size*))
              :width (truncate (/ *width* *grid-size*))]
  [adjust viewport]
  (xe2:install-widgets *viewport*)
  (xe2:enable-classic-key-repeat 100 60)
  ;; now play!
  [play universe
        :player *player*
        :address *address*
        :prompt *prompt*
        :viewport *viewport*])
;; xe2-lisp-file ends here
