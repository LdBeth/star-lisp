;;; CA Example From "Instant *Lisp" Chapter
;;; by William R. Swanson, Thinking Machines Corporation

;;; Load into *Lisp package
(in-package '*lisp)

;;; --- Global Variables ---

;;; This defines a permanent pvar to hold the grid of cells
(*defvar *automata-grid* 0)

;;; Total number of states allowed per cell
(defvar *total-number-of-states* 10)

;;; Cell "neighborhood" to use for automata
(defvar *neighborhood* :neumann)

;;; --- Simple Tools ---

;;; Function to display the grid
(defun view (&optional (width 8) (height 5))
  (ppp *automata-grid* :mode :grid :end (list width height)))

;;; Functions to read/write individual cells
(defun read-cell (x y)
  (pref *automata-grid* (grid x y)))

(defun set-cell (x y newvalue)
  (*setf (pref *automata-grid* (grid x y)) newvalue))

;;; Function to set value of entire grid
(defun set-grid (newvalue)
  (*set *automata-grid*
	(mod!! newvalue *total-number-of-states*)))

;;; Function to randomly set the value of each cell
(defun random-grid ()
  (set-grid (random!! *total-number-of-states*)))

;;; Tools to set up a fixed pattern:
(defun set-cells (cell-list value)
  (dolist (cell cell-list)
    (set-cell (car cell) (cadr cell) value)))

;;; Clear grid, set up "r-pentomino" pattern, and display
(defun init ()
  (set-grid 0)
  (set-cells '((2 2) (3 1) (3 2) (3 3) (4 1))
	     1)
  (view))

;;; Tools to get information about neighboring cells.
;;; Count non-zero Von Neumann neighbors
(defun neumann-count (grid)
  (+!! (news!! grid 0  -1)   ;; north
       (news!! grid 0  1)    ;; south
       (news!! grid -1 0)    ;; west
       (news!! grid 1  0)    ;; east
       ))

;;; Count non-zero Moore neighbors
(defun moore-count (grid)
    (+!! (news!! grid 0  -1) ;; north
	 (news!! grid 0  1)  ;; south
	 (news!! grid -1 0)  ;; west
	 (news!! grid 1  0)  ;; east
	 (news!! grid -1 -1) ;; northwest
	 (news!! grid -1 1)  ;; southwest
	 (news!! grid 1  -1) ;; northeast
	 (news!! grid 1  1)  ;; southeast
	 ))

;;; Count neighbors for current *neighborhood*
(defun neighbor-count ()
  (*let ((grid (signum!! *automata-grid*)))
    (ecase *neighborhood*
      (:moore (moore-count grid))
      (:neumann (neumann-count grid)))))

;;; Function to run the automata defined by the function one-step.
(defun simple-run (&optional (n 1))
  (dotimes (i n)
    (set-grid (one-step))))

;;; Function to run automata for n steps, and then display the grid.
(defun view-step (&optional (n 1))
  (run n)
  (view))

;;; Tool to check whether all of the cells are "dead" (zero).
(defun deadp ()
  (zerop (*sum (signum!! *automata-grid*))))

;;; --- Simple Automaton Example ---
;;; This automaton obeys the following rules:
;;; If a cell is:
;;;    EVEN - divide its value by 2
;;;    ODD  - add 1 to its value and multiply by 2

;(defun one-step ()
;  (if!! (evenp!! *automata-grid*)
;     (floor!! *automata-grid* 2)
;     (*!! (1+!! *automata-grid*) 2)))

;;;; --- "9 Life" automata, based on Conway's Game of Life ---
;;;; Obeys the following rules:
;;;;  For each cell, count the number of non-zero neighbors.
;;;;    If it is <1, or >3, subtract 1 (zero cells remain zero).
;;;;    If it is 2 or 3, add 1
;;;;    Otherwise, do nothing

(defun one-step ()
  (*let ((count (neighbor-count)))
    (cond!!
      ;; When count is <1 or >3, subtract 1 if not zero.
      ((or!! (<!! count 1) (>!! count 3))
       (if!! (zerop!! *automata-grid*)
	     *automata-grid*
	     (1-!! *automata-grid*)))

      ;; When count is 2 or 3, add 1
      ((<=!! 2 count 3) (1+!! *automata-grid*))

      ;; Otherwise, leave cells unchanged
      (t *automata-grid*))))

;;; Extension of Material in Chapter 1:
;;; Tools to define and run generic automata:

;;; Macro that defines a named automaton
;;; as a list of two function objects.
;;; Init function sets up the *automata-grid*
;;; Step function calculates and returns single "step" of automata

(defmacro defautomaton (name &key init step)
  `(progn
     (defvar ,name)
     (setq ,name (list (lambda ,@init)
		       (lambda ,@step)))
     ',name))

(defun init-function (automaton) (car automaton))
(defun step-function (automaton) (cadr automaton))

;;; Definitions for the two automata we've already written

(defautomaton four2one
    ;;; init function takes no arguments, and randomizes the grid.
    :init ((&rest ignore)
	   (setq *total-number-of-states* 10)
	   (random-grid))
    ;;; step function takes no arguments, and calculates one step
    :step ((&rest ignore)
	   (if!! (evenp!! *automata-grid*)
		 (floor!! *automata-grid* 2)
		 (*!! (1+!! *automata-grid*) 2))))

(defautomaton 9life
    ;;; init function takes optional arguments defining
    ;;; the current neighborhood, the initial pattern,
    ;;; and the value stored into cells that are part of the pattern
    :init ((&optional (neighborhood *neighborhood*)
		      (start-pattern '((2 2) (3 1) (3 2) (3 3) (4 1)))
		      (start-value 1))
	   (setq *neighborhood* neighborhood
		 *total-number-of-states* 10)
	   (set-grid 0)
	   (set-cells start-pattern start-value))
    ;;; step function takes no arguments, and
    ;;; calculates a single step of the automaton
    :step ((&rest ignore)
	   (*let ((count (neighbor-count)))
	     (cond!! ((or!! (<!! count 1) (>!! count 3))
		      (if!! (zerop!! *automata-grid*)
			    *automata-grid*
			    (1-!! *automata-grid*)))
		     ((<=!! 2 count 3) (1+!! *automata-grid*))
		     (t *automata-grid*)))))

;;; --- Tools used to select an automaton to run ---

;;; Currently-selected automaton
(defvar *current-automaton*)

;;; Function to select an automaton and initialize the grid
(defun setup (automaton &rest init-args)
  (setq *current-automaton* automaton)
  (initialize init-args))

;;; Function to call the init function of the current automaton,
;;; and display the initial state of the grid.
(defun initialize (&optional init-args)
  (apply (init-function *current-automaton*) init-args)
  (view))

;;; Function to run the automaton for n steps and display the grid
(defun run (&optional (n 1) &rest step-args)
  (dotimes (i n)
    (set-grid
      (apply (step-function *current-automaton*)
	     step-args)))
  (view))

;;; The following sample session shows how to set up and
;;; run the automata defined by the above extensions:
;
;> (setup four2one) ;; Simple 4-2-1 loop automata
;
;5 7 9 3 2 3 1 9 
;7 0 3 4 3 3 0 3 
;9 4 2 3 8 9 2 5 
;7 3 3 5 8 2 9 3 
;1 7 9 1 8 6 9 6 
;
;> (run)
;
;2 6 0 8 1 8 4 0 
;6 0 8 2 8 8 0 8 
;0 2 1 8 4 0 1 2 
;6 8 8 2 4 1 0 8 
;4 6 0 4 4 3 0 3 
;
;> (run 50)
;
;4 1 0 2 2 2 1 0 
;1 0 2 4 2 2 0 2 
;0 4 2 2 1 0 2 4 
;1 2 2 4 1 2 0 2 
;1 1 0 1 1 4 0 4 
;
;> (setup 9life :neumann) ;; 9 Life, Neumann neighborhood
;
;0 0 0 0 0 0 0 0 
;0 0 0 1 1 0 0 0 
;0 0 1 1 0 0 0 0 
;0 0 0 1 0 0 0 0 
;0 0 0 0 0 0 0 0 
;
;> (run)
;
;0 0 0 0 0 0 0 0 
;0 0 1 2 1 0 0 0 
;0 0 1 2 1 0 0 0 
;0 0 1 1 0 0 0 0 
;0 0 0 0 0 0 0 0 
;
;> (run 50)
;
;0 0 0 0 0 0 0 0 
;0 0 0 8 3 0 0 0 
;0 0 5 0 7 0 0 0 
;0 0 4 5 9 0 0 0 
;0 0 0 0 0 0 0 0 
;
;> (setup 9life :moore) ;; 9 Life, Moore neighborhood
;
;0 0 0 0 0 0 0 0 
;0 0 0 1 1 0 0 0 
;0 0 1 1 0 0 0 0 
;0 0 0 1 0 0 0 0 
;0 0 0 0 0 0 0 0 
;
;> (run)
;
;0 0 0 1 1 0 0 0 
;0 0 1 2 2 0 0 0 
;0 0 2 0 0 0 0 0 
;0 0 1 2 1 0 0 0 
;0 0 0 0 0 0 0 0 
;
;> (run 50)
;
;0 0 0 0 4 1 0 1 
;5 7 7 2 1 1 1 0 
;0 0 0 0 1 1 5 2 
;4 4 9 2 1 0 4 0 
;0 0 0 0 1 2 0 2 
