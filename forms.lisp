;;; forms.lisp --- port of cell-mode to common lisp

;; Copyright (C) 2006, 2007, 2010  David O'Toole

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

(in-package :xe2)

(defun generate-page-name (world)
  (concatenate 'string (get-some-object-name world) "::" (format nil "~S" (genseq))))

(defun create-blank-page (&key height width name)
  (let ((world (clone =world= :height height :width width)))
    (prog1 world
      (setf (field-value :name world)
	    (or name (generate-page-name world)))
      [generate world])))

(defun find-page (page)
  (etypecase page
    (clon:object (prog1 page (make-object-resource 
			      (or (field-value :name page)
				  (generate-page-name page)) page)))
    (string (or (find-resource-object page :noerror)
		(progn (make-object-resource page (create-blank-page :name page))
		       (let ((object (find-resource-object page)))
			 (prog1 object
			   (setf (field-value :name object) page))))))))

;; (maphash #'(lambda (k v) (when (resource-p v)
;; 			   (when (eq :object (resource-type v))
;; 			     (message "XXobject ~S" (resource-object v)))))
;; 	 *resource-table*)

;;; A generic data cell just prints out the stored value.

(defparameter *data-cell-style* '(:foreground ".gray40" :background ".white"))

(defcell data-cell data)

(define-method set data-cell (data)
  (setf <data> data))

(define-method get data-cell ()
  <data>)

(define-method compute data-cell ()
  ;; update the label
  (setf <label> (list (cons (format nil " ~S  " <data>) *data-cell-style*))))

;;; The form widget browses workbook pages

(define-prototype form 
    (:parent =widget= :documentation  "An interactive graphical spreadsheet.")
  prompt narrator
  (page-name :initform nil)
  (world :documentation "The xe2:=world= of objects to be displayed.")
  rows columns
  (entered :initform nil :documentation "When non-nil, forward key events to the entry and/or any attached widget.")
  (cursor-row :initform 0) 
  (cursor-column :initform 0)
  (view-style :initform :label)
  (tile-size :initform 16)
  (cursor-color :initform ".yellow")
  (origin-row :initform 0 :documentation "Row number of top-left displayed cell.") 
  (origin-column :initform 0 :documentation "Column number of top-left displayed cell.")
  (column-widths :documentation "A vector of integers where v[x] is the pixel width of form column x.")
  (row-heights :documentation "A vector of integers where v[x] is the pixel height of form row x.")
  (column-styles :documentation "A vector of property lists used to customize the appearance of columns.")
  (row-spacing :initform 1 :documentation "Number of pixels to add between rows.")
  (zebra-stripes :documentation "When non-nil, zebra stripes are drawn.")
  (row-styles :documentation "A vector of property lists used to customize the appearance of rows.")
  (border-style :initform t :documentation "When non-nil, draw cell borders.")
  (draw-blanks :initform t :documentation "When non-nil, draw blank cells.")
  (header-style :initform t :documentation "When non-nil, draw row and column headers.")
  (header-line :initform nil :documentation "Formatted line to be displayed at top of window above spreadsheet.")
  (status-line :initform nil :documentation "Formatted line to be displayed at top of window above spreadsheet.")
  (tool :initform :clone :documentation "Keyword symbol identifying the method to be applied.")
  (tool-methods :initform '(:clone :erase :inspect)))

(defparameter *default-page-name* "*scratch*")

(define-method initialize form (&optional (page *default-page-name*))
  (with-fields (entry) self
    (let ((world (find-page page)))
      [parent>>initialize self]
      [visit self page])))

(define-method generate form (&rest parameters)
  "Invoke the current page's default :generate method, passing PARAMETERS."
  [generate-with <world> parameters])

(define-method set-tool form (tool)
  "Set the current sheet's selected tool to TOOL."
  (assert (member tool <tool-methods>))
  (setf <tool> tool))

(define-method get-selected-cell-data form ()
  (let ((cell [selected-cell self]))
    (when cell
      [get cell])))

(define-method next-tool form ()
  "Switch to the next available tool." 
  (with-fields (tool tool-methods) self
    (let ((pos (position tool tool-methods)))
      (assert pos)
      (setf tool (nth (mod (1+ pos) (length tool-methods))
		      tool-methods))
      [say self (format nil "Changing tool operation to ~S" tool)])))

(define-method apply-tool form (data)
  "Apply the current form's tool to the DATA."
  (with-fields (tool tool-methods) self
    (send nil tool self data)))

(define-method clone form (data)
  "Clone the prototype named by the symbol DATA and drop the clone
at the current cursor location. See also APPLY-LEFT and APPLY-RIGHT."
  (if (and (symbolp data)
	   (boundp data)
	   (clon:object-p (symbol-value data)))
      [drop-cell <world> (clone (symbol-value data)) <cursor-row> <cursor-column>]
      [say self "Cannot clone."]))

(define-method erase form (&optional data)
  "Erase the top cell at the current location."
  [say self "Erasing top cell."]
  (let ((grid (field-value :grid <world>)))
    (vector-pop (aref grid <cursor-row> <cursor-column>))))

(define-method visit form (&optional (page *default-page-name*))
  "Visit the page PAGE with the current form. If PAGE is a =world=
object, visit it and add the page to the page collection. If PAGE is a
string, visit the named page. If the named page does not exist, a
default page is created. See also CREATE-WORLD."
  (let ((world (find-page page)))
    (assert (object-p world))
    (setf <page-name> (field-value :name world))
    [say self (format nil "Visiting page ~S" <page-name>)]
    (setf <world> world)
    (setf *world* world)
    [install-keybindings self]
    (setf <rows> (field-value :height world))
    (setf <columns> (field-value :width world))
    (assert (integerp <rows>))
    (assert (integerp <columns>))
    (setf <cursor-row> 0)
    (setf <cursor-column> 0)
    (setf <cursor-column> (min <columns> <cursor-column>))
    (setf <cursor-row> (min <rows> <cursor-row>))
    (setf <cursor-column> (min <columns> <cursor-column>))
    (setf <column-widths> (make-array (+ 1 <columns>) :initial-element 0)
	  <row-heights> (make-array (+ 1 <rows>) :initial-element 0)
	  <column-styles> (make-array (+ 1 <columns>))
	  <row-styles> (make-array (+ 1 <rows>)))))

(define-method cell-at form (row column)
  (assert (and (integerp row) (integerp column)))
  [top-cell-at <world> row column])

(define-method set-prompt form (prompt)
  (setf <prompt> prompt))

(define-method set-narrator form (narrator)
  (setf <narrator> narrator))

(define-method install-keybindings form ()
  (bind-key-to-method self "RETURN" nil :enter)
  (bind-key-to-method self "ESCAPE" nil :exit)
  (bind-key-to-method self "F9" nil :tile-view)
  (bind-key-to-method self "F10" nil :label-view)
  (bind-key-to-method self "X" '(:control) :goto-prompt)
  (bind-key-to-method self "T" '(:control) :next-tool)
  (bind-key-to-method self "UP" nil :move-cursor-up)
  (bind-key-to-method self "DOWN" nil :move-cursor-down)
  (bind-key-to-method self "LEFT" nil :move-cursor-left)
  (bind-key-to-method self "RIGHT" nil :move-cursor-right))

(define-method set-view-style form (style)
  "Set the rendering style of the current form to STYLE.
Must be one of (:tile :label)."
  (setf <view-style> style))

(define-method tile-view form ()
  "Switch to tile view in the current form."
  [set-view-style self :tile])

(define-method label-view form ()
  "Switch to label view in the current form."
  [set-view-style self :label])

(define-method goto-prompt form ()
  "Jump to the command prompt."
  (when <prompt>
    [goto <prompt>]))

(define-method selected-cell form ()
  [cell-at self <cursor-row> <cursor-column>])

(define-method select form ()
  (let ((cell [selected-cell self]))
    (when cell
      [select cell])))

(define-method eval form (&rest args)
  "Evaluate all the ARGS and print the result."
  (when <prompt> 
    [print-data <prompt> args :comment]))
 
(define-method say form (text)
  (when <prompt>
    [say <prompt> text]))

(define-method help form (&optional (command-name :commands))
  "Print documentation for the command COMMAND-NAME.
Type HELP :COMMANDS for a list of available commands."
  (let* ((command (make-keyword command-name))
	 (docstring (method-documentation command))
	 (arglist (method-arglist command)))
    (with-field-values (prompt) self
      (when prompt
	[print-data prompt (format nil "Command name: ~A" command) :comment]
	[print-data prompt (format nil "Arguments: ~S" (if (eq arglist :not-available)
					       :none arglist))
		    :comment]
	[print-data prompt (format nil" ~A" docstring) :comment]))))

(define-method save-all form ()
  [say self "Saving objects..."]
  (xe2:save-modified-objects t)
  [say self "Saving objects... Done."])

(define-method create-world form (&key height width name)
  "Create and visit a blank world of height HEIGHT, width WIDTH, and name NAME."
  (let ((world (create-blank-page :height height :width width :name name)))
    [visit self world]))
    
(define-method enter form ()
  "Begin entering LISP data into the current cell."
  (unless <entered>
    [say self "Now entering data. Press ESCAPE to stop editing."]
    (let ((entry (clone =textbox=))
	  (cell [selected-cell self]))
      (when (null cell)
	(setf cell (clone =data-cell=))
	[drop-cell <world> cell <cursor-row> <cursor-column>])
      [install-keybindings entry]
      [resize entry :width 120
	      :height 40]
      [move entry :x 0 :y 0]
      (setf <entered> t)
      (setf (field-value :widget cell)
	    entry))))
    
(define-method load-module form (name)
  "Load the XE2 module NAME for development."
  [say self (format nil"Loading module ~S" name)]
  (xe2:load-module name))

(define-method quit form ()
  "Quit XIODEV."
  (xe2:quit t))

(define-method exit form ()
  "Stop entering data into the current cell."
  (when <entered>
    (with-fields (widget) [selected-cell self]
      (let* ((str [get-buffer-as-string widget])
	     (data (handler-case
		       (read-from-string str)
		     (condition (c) 
		       [say self (format nil "Error reading data: ~S" c)]))))
	(when data
	  [set [selected-cell self] data])
	(setf widget nil)
	(setf <entered> nil)
	[say self "Finished entering data."]))))

(defparameter *blank-cell-string* '(" ........ "))

(define-method row-height form (row)
  (let ((height 0) cell)
    (dotimes (column <columns>)
      (setf cell [cell-at self row column])
      (when cell
	(setf height (max height [form-height cell]))))
    (ecase <view-style>
      (:label (max (formatted-string-height *blank-cell-string*) height))
      (:tile <tile-size>))))

(define-method column-width form (column)
  (let ((width 0) cell)
    (dotimes (row <rows>)
      (setf cell [cell-at self row column])
      (when cell
	(setf width (max width [form-width cell]))))
    (ecase <view-style> 
      (:label (max width (formatted-string-width *blank-cell-string*)))
      (:tile <tile-size>))))

(define-method compute-geometry form ()
  (dotimes (column <columns>)
    (setf (aref <column-widths> column)
	  [column-width self column]))
  (dotimes (row <rows>)
    (setf (aref <row-heights> row)
	  [row-height self row])))

(defparameter *even-columns-format* '(:background ".gray50" :foreground ".gray10"))
(defparameter *odd-columns-format* '(:background ".gray45" :foreground ".gray10"))

(define-method handle-key form (event)
  ;; possibly forward event to current cell. used for the event cell, see below.
  (if (equal "ESCAPE" (car event))
      [parent>>handle-key self event]
      (let* ((cell [selected-cell self])
	     (widget (when cell (field-value :widget cell))))
	(cond ((and cell (has-method :handle-key cell))
	       (or [handle-key cell event]
		   [parent>>handle-key self event]))
	      ((and widget <entered>)
	       [handle-key widget event])
	      (t [parent>>handle-key self event])))))

;; (define-method hit form (x0 y0) 
;;   (with-field-values (row-heights column-widths origin-row origin-column rows columns)
;;       self
;;     (let* ((x 0)
;; 	   (y 0)
;; 	   (selected-column 
;; 	    (loop for column from 0 to columns
;; 		  do (incf x (aref column-widths column))
;; 		  when (> x x0) return 
	  

(define-method compute form ()
  (with-fields (rows columns) self
    (let (data cell)
      (dotimes (row rows)
	(setf data nil)
	(dotimes (column columns)
	  (setf cell [cell-at self row column])
	  (if (null cell)
	      (setf data nil)
	      (progn 
		(when data [set cell data])
		[compute cell]
		(setf data [get cell]))))))))

;; TODO break up this method.

(define-method render form ()
  [clear self]
  (when <world>
    (with-field-values (cursor-row cursor-column row-heights world page-name 
				   origin-row origin-column header-line status-line
				   view-style header-style tool tool-methods
				   row-spacing rows columns draw-blanks column-widths) self
      [compute self]
      [compute-geometry self]
      (let* ((image <image>)
	     (widget-width <width>)
	     (widget-height <height>)
	     (rightmost-visible-column
	      (block searching
		(let ((width 0))
		  (loop for column from origin-column to columns 
			do (incf width (aref column-widths column))
			   (when (> width widget-width)
			     (return-from searching (- column 1))))
		  (return-from searching (- columns 1)))))
	     (bottom-visible-row
	      (block searching
		(let ((height (if (and header-line header-style)
				  (formatted-line-height header-line)
				  0)))
		  (loop for row from origin-row to rows 
			do (incf height (aref row-heights row))
			   (when (> height widget-height)
			     (return-from searching (- row 1))))
		  (return-from searching (- rows 1)))))
	     (x 0) 
	     (y 0)
	     (cursor-dimensions nil))
	;; see if current cell has a tooltip
	;; (let ((selected-cell [cell-at self cursor-row cursor-column]))
	;;   (when (object-p selected-cell)
	;;     (setf header-line (field-value :tooltip selected-cell))))
	;; draw header line with tooltip, if any
	(when (and header-line header-style)
	  (render-formatted-line header-line 0 y :destination image)
	  (incf y (formatted-line-height header-line)))
	;; create status line
	(setf status-line
	      (list 
	       (list (format nil " [ ~A ]     " page-name) :foreground ".yellow"
		     :background ".red")
	       (list (format nil " | Loc: (~S, ~S) | Data : ~A | Tool: ~S "
				   cursor-row cursor-column 
				  (when (field-value :paint <world>)
				    (get-some-object-name (field-value :paint <world>)))
				  tool)
			  :foreground ".white"
			  :background ".gray20")))
	;; draw status line
	(when status-line 
	  (render-formatted-line status-line 0 y :destination image)
	  (incf y (formatted-line-height status-line)))
	;; TODO column header, if any
	;; (message "GEOMETRY: ~S" (list :origin-row origin-row
	;; 			      :origin-column origin-column
	;; 			      :right rightmost-visible-column
	;; 			      :bottom bottom-visible-row))
	(loop for row from origin-row to bottom-visible-row do
	  (setf x 0)
	  (loop for column from origin-column to rightmost-visible-column do
	    (let ((column-width (aref column-widths column))
		  (row-height (aref row-heights row))
		  (cell [cell-at self row column]))
	      ;; render the cell
	      (if (null cell)
		  (when draw-blanks
		    (draw-box x y 
			      column-width 
			      row-height  
			      :stroke-color ".gray30"
			      :color (if (evenp column) ".gray50" ".gray45")
			      :destination image))
		  ;; see also cells.lisp
		  (progn 
		    (ecase view-style
		      (:label [form-render cell image x y column-width])
		      (:tile (when (field-value :tile cell)
			       (draw-image (find-resource-object 
					    (field-value :tile cell)) x y :destination image))))
		    (when <entered>
		      (draw-rectangle x y 
				column-width 
				row-height
				:color ".red"
				:destination image))))
	      ;; TODO possibly indicate more cells to right/left/up/down of screen portion
	      ;; possibly save cursor drawing info for this cell
	      (when (and (= row cursor-row) (= column cursor-column))
		(setf cursor-dimensions (list x y column-width row-height)))
	      ;; move to next column right
	      (incf x (aref column-widths column))))
	  ;; move to next row down
	  (incf y (+ (if (eq :tile view-style)
			 0 row-spacing) (aref row-heights row))))
	;; render cursor, if any 
	(when cursor-dimensions
	  (destructuring-bind (x y w h) cursor-dimensions
	    [draw-cursor self x y w h]))))))

;;; Cursor

(define-method draw-cursor form (x y width height)
  (draw-rectangle x y width height :color <cursor-color> :destination <image>))

(define-method move-cursor form (direction)
  "Move the cursor one step in DIRECTION. 
DIRECTION is one of :up :down :right :left."
  (with-field-values (cursor-row cursor-column rows columns) self
    (let ((cursor (list cursor-row cursor-column)))
      (setf cursor (ecase direction
		     (:up (if (/= 0 cursor-row)
			      (list (- cursor-row 1) cursor-column)
			      cursor))
		     (:left (if (/= 0 cursor-column)
				(list cursor-row (- cursor-column 1))
				cursor))
		     (:down (if (< cursor-row (- rows 1))
				(list (+ cursor-row 1) cursor-column)
				cursor))
		     (:right (if (< cursor-column (- columns 1))
				 (list cursor-row (+ cursor-column 1))
				 cursor))))
      (destructuring-bind (r c) cursor
	(setf <cursor-row> r <cursor-column> c)))))

(define-method move-cursor-up form ()
  [move-cursor self :up])

(define-method move-cursor-down form ()
  [move-cursor self :down])

(define-method move-cursor-left form ()
  [move-cursor self :left])

(define-method move-cursor-right form ()
  [move-cursor self :right])

;;; A var cell stores a value into a variable, and reads it.

(defparameter *var-cell-style* '(:foreground ".white" :background ".blue"))

(defcell var-cell variable)

(define-method initialize var-cell (variable)
  (setf <variable> variable))

(define-method set var-cell (value)
  [set-variable *world* <variable> value])

(define-method get var-cell ()
  [get-variable *world* <variable>])

(define-method compute var-cell ()
  (setf <label> (list (cons (format nil ">> ~A  " <variable>) *var-cell-style*))))

;;; Event cell picks up next event when clicked

(defparameter *event-cell-style* '(:foreground ".yellow" :background ".forest green"))

(defcell event-cell event capturing)

(define-method set event-cell (event)
  (setf <event> event))

(define-method get event-cell ()
  <event>)

(define-method handle-key event-cell (event)
  (when <capturing> 
    ;; signal that we handled this event
    (prog1 t
      (setf <event> event)
      (setf <capturing> nil))))
  
(define-method compute event-cell () 
  (setf <label> 
	(list (cons (if <capturing>
			" CAPTURING... "
			(destructuring-bind (key &rest modifiers) <event>
			  (if modifiers
			      (let ((mod-string (format nil " ~A" 
							(apply #'concatenate 'string 
							       (mapcar #'(lambda (mod)
									   (format nil "~A " mod))
								       modifiers)))))
				(if (string= "JOYSTICK" key)
				    (concatenate 'string key mod-string)
				    (concatenate 'string mod-string key " ")))
			      (concatenate 'string " " key " "))))
		    *event-cell-style*))))

(define-method select event-cell ()
  ;; capture next event
  (setf <capturing> t))

;;; Comment cell just displays text.

(defparameter *comment-cell-style* '(:foreground ".white" :background ".gray20"))

(defcell comment-cell comment)

(define-method initialize comment-cell (comment)
  (setf <comment> comment))
  
(define-method set comment-cell (comment) nil)

(define-method get comment-cell ()
  <comment>)

(define-method compute comment-cell () 
  (setf <label> (list (cons (format nil " ~A " <comment>) *comment-cell-style*))))

;;; Button cell executes some lambda.

(defparameter *button-cell-style* '(:foreground ".yellow" :background ".red"))

(defparameter *button-cell-highlight-style* '(:foreground ".red" :background ".white"))

(defparameter *button-cell-highlight-time* 15)

(defcell button-cell button clock)

(define-method initialize button-cell (&key closure text)
  (setf <closure> closure)
  (setf <clock> 0)
  (setf <label> (list (cons text *button-cell-style*))))
  
(define-method set button-cell (button) nil)

(define-method get button-cell () <closure>)

(define-method compute button-cell () 
  (with-fields (label clock) self
    (unless (zerop clock)
      (decf clock))
    (setf (cdr (first label))
	  (if (plusp clock)
	      *button-cell-highlight-style*
	      *button-cell-style*))))

(define-method select button-cell ()
  (funcall <closure>)
  (setf <clock> *button-cell-highlight-time*))

;;; forms.lisp ends here
