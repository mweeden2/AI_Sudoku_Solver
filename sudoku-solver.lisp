;;;*******************************************************************************
;;;*******************************************************************************
;;;                          Sudoku Puzzle Solver
;;;*******************************************************************************
;;;*******************************************************************************
;;; This function solves sudoku puzzles. It takes as input the filename of the 
;;; file with any number of unsolved puzzles. This file must have the following
;;; format:
;;;          i.   The first line must contain the number of puzzles to be solved.
;;;          ii.  The puzzles follow, separated by a blank line.
;;;          iii. Each puzzle has its 9 rows on separate lines with spaces between
;;;               each of the 9 columns.
;;;          iv.  Unassigned cells are indicated by a zero.
;;;
;;;      Example:
;;;
;;;          1
;;;          1 9 5 6 0 0 0 2 3
;;;          7 0 0 0 0 0 0 0 0
;;;          3 0 4 9 0 0 0 0 8
;;;          0 0 0 0 8 1 0 5 9
;;;          0 5 8 0 6 9 0 0 0
;;;          6 0 0 0 0 2 0 7 0
;;;          9 0 0 3 0 0 0 8 6
;;;          0 0 0 0 0 0 2 0 0
;;;          0 0 0 0 7 5 4 0 0
;;;
;;; This file is meant to be used in conjunction with "print-sudoku.lisp", 
;;; "read-sudoku.lisp", "arcs-sudoku.lisp", and "constraints-sudoku.lisp". The 
;;; first few lines of this file may be uncommented to automatically load these
;;; files when "sudoku-solver.lisp" is loaded.
;;;
;;; For each puzzle, this function prints: 
;;;      i.   The initial puzzle
;;;      ii.  The remaining possible values that can be assigned to each cell when
;;;           using forward checking.
;;;      iii. The remaining possible values that can be assigned to each cell when
;;;           using AC3 (arc-consistency).
;;;      iv.  For each method of solving (backtracking depth-first, backtracking
;;;           with forward checking, and backtracking with AC3), the function 
;;;           prints:
;;;                    a. The solution produced.
;;;                    b. The number of backtracks performed.
;;;
;;;
;;; NOTES:  - This solver prints the remaining pozzlible values of eac puzzle in a
;;;           way that may require the output console to very wide (depending on 
;;;           the puzzle) to be able to view them in the intended arrangement.
;;;         - In the comments of this program, the word "constraints" is often
;;;           used to refer to the 2-dimensional indices of other cells related to
;;;           some "current" cell by a contraint (in the same sub-board, row, or 
;;;           column).
;;;         - List and array versions of a puzzle are used in different functions 
;;;           so as to utilize the ability to iterate over lists as well as the
;;;           faster indexing of arrays.
;;;*******************************************************************************


 (load 'read-sudoku.lisp)
 (load 'print-sudoku.lisp)
 (load 'arcs-sudoku.lisp)
 (load 'constraints-sudoku.lisp)


;; this global variable keeps track of the number of backtracks while the 
;; backtracking function iterates using the loop operator; it is reset at the 
;; beginning of every non-recursive call to backtracking.
(setf *num-backtracks* 0)


(defun solve-sudoku (filename &optional (puzzles 'uninitialized) (count 0))
  (if (eq puzzles 'uninitialized)
      (progn (format t "~%================== Sudoku Solver ==================")
	     (solve-sudoku filename (read-file filename)))
    (if (null puzzles)
	nil
      (let ((puzzle (first puzzles)))
	(progn (format t "~&~%Puzzle ~a:~%" (1+ count))
	       (print-puzzle puzzle)

	       (format t "~&~%Puzzle ~a Foward-checking remaining values:~%"
		       (1+ count))
	       (print-domain (get-domain (puzzle-to-array puzzle)))

	       (format t "~&~%Puzzle ~a AC3 remaining values:~%"
		       (1+ count))
	       (print-domain
		(AC3 0 puzzle
		     (get-domain (puzzle-to-array puzzle))
		     nil t))
	       
	       (format t "~&~%====== Solutions ======~%")
	       (format t "~&~%~~Backtracking/Depth-First~~~%~%")
	       (time (progn (print-puzzle (backtracking puzzle 0))
                            (format t "  (~a backtracks)~%~%" *num-backtracks*)))
	       
	       (format t "~&~%~~Backtracking with Forward-checking~~~%~%")
	       (time (progn (print-puzzle (backtracking puzzle 1))
                            (format t "  (~a backtracks)~%~%" *num-backtracks*)))
	       
	       (format t "~&~%~~Backtracking with AC3~~~%~%")
	       (time (progn (print-puzzle (backtracking puzzle 2))
			    (format t "  (~a backtracks)~%~%" *num-backtracks*)))
	       
	       (format t "~&=======================================")

	       (solve-sudoku 'none (rest puzzles) (1+ count)))))))
	

;;;*******************************************************************************
;;; This function implements the backtracking search algorithm and returns a 
;;; puzzle. It can implement forward checking, AC3 (arc consistency), or neither 
;;; as it's inference function. Backtracking with no inference is equivalent to 
;;; pure depth-first search.
;;; INPUT:  puzzle - the list of rows that contains the pre-assigned cells
;;;         mode - 0 for depth-first (no inference), 1 for forward checking, and
;;;                2 for AC3 
;;;         remaining-values - a 1-d array of the remaining values (or domain) 
;;;                            for each cell in puzzle
;;;*******************************************************************************

(defun backtracking (puzzle mode 
			    &optional (remaining-values nil))
  ;; initially generate the remaining-possible-values
  (if (null remaining-values)
      (progn (setf *num-backtracks* 0)	; reset *num-backtracks*
	     (backtracking puzzle mode
			   (get-domain (puzzle-to-array puzzle))))
    (if (completep puzzle)
	puzzle
      (let* ((cell (select-unassigned-cell puzzle))
	     (remaining-cell-values (aref remaining-values cell)))
	;; if this unassaigned cell has an empty domain
	(if (equal remaining-cell-values '(0))
	    nil	
	  (loop for x in remaining-cell-values do
		(if (assignment-allowed x cell remaining-values)
		    (let* ((new-puzzle (assign x cell puzzle))
			   (inferences
			    (infer cell new-puzzle remaining-values mode)))
		      ;; if inferences didn't fail
		      (if (not (null inferences))
			  ;; this line backtracks and implicitly throws away the 
			  ;; last cell assignment
			  (let ((result (backtracking 
					; (assign-from-inferences 
					;    new-puzzle inferences) 
					 new-puzzle
					 mode inferences)))
			    (if (null result)
				(incf *num-backtracks*)
			      (return result)))
			(incf *num-backtracks*))))))))))


;;;*******************************************************************************
;;; This function implements forward checking, which updates the domains
;;; (remaining cell values) for each cell related to the recently-assigned cell
;;; by a constraint.
;;; INPUTS: cell - the 1d index (0..80) of the recently-assigned cell
;;;         puzzle - list of 9 lists of 9
;;;         remaining-values - the current array (length = 81) of cell domains
;;;         constraints - the list of constraints (other cells) that are related
;;;                       to the current cell.
;;;*******************************************************************************

(defun forward-checking (cell puzzle remaining-values
			      &optional (constraints nil))
  ;; initialize by converting puzzle to an array, getting cell constraints, and
  ;; updating the current cell's domain to '(0)
  (if (and (null constraints) (not (arrayp puzzle)))
      (let ((puzzle-array (puzzle-to-array puzzle)))
	(forward-checking 
	 cell puzzle-array 
	 ;; non-destructivley replace the current cell's domain with '(0)
	 (concatenate 'array
		      (subseq remaining-values 0 cell)
		      (make-array 1 :initial-contents (list '(0)))
		      (subseq remaining-values (1+ cell) 81))
	 (row-col-constraints-a cell puzzle-array 
			      (sub-board-constraints-a cell puzzle-array))))
    (if (null constraints)    ; all constraints have been checked and updated
	remaining-values
      (let* ((row (floor cell 9))
	     (col (mod cell 9))
	     (c-row (first (first constraints)))
	     (c-col (second (first constraints)))
	     (c-cell (+ c-col (* 9 c-row)))
	     (new-cell-domain
	      (remove (aref puzzle row col) (aref remaining-values c-cell))))
	;; non-destructivley replace the current cell's domain with '(0)
	(let ((rmnng-vls
	       (concatenate 'array
			    (subseq remaining-values 0 cell)
			    (make-array 1 :initial-contents (list '(0)))
			    (subseq remaining-values (1+ cell) 81))))
	  (if (null new-cell-domain)
	      nil
	    (forward-checking 
	     cell puzzle 
	     ;; non-destructively replace the constraint cell's domain with its
	     ;; updated domain
	     (concatenate 'array
			  (subseq rmnng-vls 0 c-cell)
			  (make-array 
			   1
			   :initial-contents (list new-cell-domain))
			  (subseq rmnng-vls (1+ c-cell) 81))
	     (rest constraints))))))))


;;;*******************************************************************************
;;; This function implements forward checking, which updates the domains
;;; (remaining cell values) for each cell related to the recently-assigned cell
;;; by a constraint.
;;; INPUTS: cell - the 1d index (0..80) of the recently-assigned cell
;;;         puzzle - list of 9 lists of 9
;;;         remaining-values - the current array (length = 81) of cell domains
;;;         constraints - the list of constraints (other cells) that are related
;;;                       to the current cell.
;;;*******************************************************************************

(defun AC3 (cell puzzle remaining-values &optional (arcs nil) (top-call nil))
  ;; initialize by converting puzzle to an array, getting cell constraints, and
  ;; updating the current cell's domain to '(0)
  (if (and (null arcs) top-call)
      (AC3
       cell puzzle
       ;; non-destructivley replace the current cell's domain with '(0)
       (concatenate 'array
		    (subseq remaining-values 0 cell)
		    (make-array 1 :initial-contents (list '(0)))
		    (subseq remaining-values (1+ cell) 81))
       (get-all-arcs puzzle))
    (if (not (arrayp puzzle))
	(let ((puzzle-array (puzzle-to-array puzzle)))
	  (AC3
	   cell puzzle-array remaining-values arcs))
      (if (null arcs)
	  remaining-values
	(let* ((arc (first arcs))
	       (revised-remaining-values (revise-domain arc remaining-values)))
	  (if (null revised-remaining-values) ; if no revision was made
	      (AC3 cell puzzle remaining-values (rest arcs))
	    ;; if a revision was made to the remaining-values (domain)
	    (let* ((current-cell 
		    (+ (second (first arc)) (* 9 (first (first arc)))))
		   (current-cell-revised-domain 
		    (aref revised-remaining-values current-cell)))
	      (if (eq 0 (length current-cell-revised-domain))
		  nil
		(AC3 cell puzzle revised-remaining-values
		     ;; including the first element in arcs when calling
		     ;; get-neighbor-arcs will ensure that that arc is not 
		     ;; re-added
		     (append (get-neighbor-arcs current-cell puzzle arcs)
			     (rest arcs)))))))))))
    

;;;*******************************************************************************
;;; This function makes inferences using either foward checking or AC3 (mode
;;; equals 1 and 2 respectfully) and returns the remaining values (domain) for
;;; each cell in the puzzle. If mode is 0, no inferences are made and the
;;; unchanged remaining-values is returned.
;;; INPUT:  cell - the number (0..80) of the cell just assigned
;;;         puzzle - the list of 9 rows, which are lists of 9 cell assignments
;;;         remaining-values - a 1-d array (length = 81) of remaining possible
;;;                            values for each cell in the puzzle
;;;         mode - 1 for forward checking, 2 for AC3, 0 (or other) for no
;;;                inference (purely depth-first seach)
;;;*******************************************************************************

(defun infer (cell puzzle remaining-values mode)
  (cond ((eq mode 1)
         (forward-checking cell puzzle remaining-values))
        ((eq mode 2)
         (AC3 cell
              (puzzle-to-array puzzle)
	      ;; use forward-checking to update domains affected by the previous
	      ;; cell assignement
              (forward-checking cell puzzle remaining-values)
              nil t))
        (t
         (get-domain (puzzle-to-array puzzle)))))


;;;*******************************************************************************
;;; This function tests whether a puzzle is complete (no assignment is 0)
;;; INPUT:  puzzle - list of 9 rows, which are lists of 9 cell assignments
;;;         result - a local variable used to iterate and collect the completeness
;;;                  of each line in the puzzle
;;;*******************************************************************************

(defun completep (puzzle &optional (result nil))
  (if (eq 9 (length result))
      (every #'null result) ; returns T if each result is nil
    (completep (rest puzzle) (append (list (find 0 (first puzzle))) result))))


;;;*******************************************************************************
;;; This function selects one unassigned (assigned to 0) cell number (0..80) from
;;; the puzzle.
;;; INPUT:  puzzle - the list of 9 rows, which are lists of 9 cell assignments
;;;*******************************************************************************

(defun select-unassigned-cell (puzzle)
  (if (null puzzle)
      nil
    (let ((cell (position 0 (first puzzle))))
      (if (null cell)
	  (select-unassigned-cell (rest puzzle))
	(+ cell (* 9 (- 9 (length puzzle))))))))


;;;*******************************************************************************
;;; This function tests whether an assignment x to cell is allowed by the
;;; remaining values in its domain (remaining-values).
;;; INPUT:  value - the value being assigned to the cell
;;;         cell - the number (0..80) of the cell being assigned
;;;         remaining-values - a 1-d array (length = 81) of remaining possible
;;;                            values for each cell in the puzzle
;;; NOTE: "assign-allowed-puzzle" is a similar function to this but goes through
;;;       the more complicated process of checking if an assignment is allowed
;;;       by other assignments in a puzzle rather than in the remaining-values
;;;       of that cell.
;;;*******************************************************************************

(defun assignment-allowed (value cell remaining-values)
  (if (find value (aref remaining-values cell))
      t
    nil))


;;;*******************************************************************************
;;; This function assigns the cell in the puzzle to the value and returns a new
;;; puzzle (non-destructive).
;;; INPUT:  value - the value being assigned to the cell
;;;         cell - the number (0..80) of the cell being assigned
;;;         puzzle - the list of 9 rows, which are lists of 9 cell assignments
;;;*******************************************************************************

(defun assign (value cell puzzle)
  (let ((row (floor cell 9))
	(col (mod cell 9)))
    (append (subseq puzzle 0 row)
	    (list (append (subseq (nth row puzzle) 0 col)
			  (list value)
			  (subseq (nth row puzzle) (1+ col) 9)))
	    (subseq puzzle (1+ row) 9))))


;;;*******************************************************************************
;;; This function returns the remaining values (1-d array of the domains for each
;;; cell in the puzzle. Assigned cells have a domain of '(0).
;;; INPUT:  puzzle-array - array of 9 lists of 9
;;;         remaining-values - array of 81 lists of domains, one for each cell
;;;         count - incrementing argument
;;;*******************************************************************************

(defun get-domain (puzzle-array &optional 
			  (remaining-values (make-array 81 :initial-element '(0)))
			  (count 0))
    (if (eq 81 count)
	remaining-values
      ;; if the cell has already been assigned
      (if (eq 0 (aref puzzle-array (floor count 9) (mod count 9)))
	  (progn (setf (aref remaining-values count)
		       (get-cell-domain count puzzle-array))
		 (get-domain puzzle-array remaining-values (1+ count)))
	(get-domain puzzle-array remaining-values (1+ count)))))
		    
	
;;;*******************************************************************************
;;; This function returns the remaining cell values available to cell according
;;; to the other assignments in the puzzle. Assigned cells have a domain of 
;;; '(0).
;;; INPUT:  cell - index (0..80) of which cell's domain to return
;;;         puzzle-array - array of 9 lists of 9
;;;         remaining-cell-values - accumulating list of possible values
;;;         count - incrementing argument
;;;*******************************************************************************

(defun get-cell-domain (cell puzzle-array 
			     &optional (remaining-cell-values nil) (count 0))
  (if (eq 9 count)
      (if (null remaining-cell-values) ; there are no remaining values
	  (list 0)
	remaining-cell-values)
    (let ((value-allowed (assign-allowed-puzzle (1+ count) cell puzzle-array)))
      (if value-allowed
	  (get-cell-domain cell puzzle-array
			   (append remaining-cell-values (list (1+ count))) 
			   (1+ count))
	(get-cell-domain cell puzzle-array 
			 remaining-cell-values 
			 (1+ count))))))


;;;*******************************************************************************
;;; This function takes a single arc and returns the updated cell domain of the 
;;; first cell based on the values of each cell's domains. If the first cell's 
;;; domain is not changed, nil is returned.
;;; INPUTS:  arc - a list of two cells, each represented by their 2d indices
;;;          remaining-values - array of domains for each cell in the puzzle
;;;          count - incrementing argument
;;;          revised - local variable to indicate whether any cell domain was
;;;                    changed or not
;;;*******************************************************************************
(defun revise-domain (arc remaining-values &optional (count 999) (revised nil))
  (let* ((cell-a (+ (second (first arc)) (* 9 (first (first arc)))))
	 (cell-b (+ (second (second arc)) (* 9 (first (second arc)))))
	 (domain-a (aref remaining-values cell-a))
	 (domain-b (aref remaining-values cell-b)))
    ;; initialize count
    (if (eq count 999)
	(revise-domain arc remaining-values (length domain-a))
      (if (eq 0 count)
	  (if revised
	      remaining-values
	    nil)
	;; if there are no more values in domain-b after removing this values
	;; in domain-a
	       (if (eq 0 (length (remove (nth (1- count) domain-a) domain-b)))
		   ;; non-destructivley replace the cell-a's domain
		   (let ((rmnng-vls
			  (concatenate 'array
				       (subseq remaining-values 0 cell-a)
				       (make-array
					1
					:initial-contents 
					(list 
					 ;; remove this value from domain-a
					 (remove 
					  (nth (1- count) domain-a) domain-a)))
				       (subseq remaining-values (1+ cell-a) 81))))
		     (revise-domain arc rmnng-vls (1- count) t))
		 (revise-domain arc remaining-values (1- count) revised))))))


;;;*******************************************************************************
;;; This function is similar to assignment-allowed, but uses the puzzle's current
;;; assignments to judge if an assignment of value to cell would be allowed.
;;; INPUT:  value - the assignment value proposed for cell in puzzle
;;;         cell - the index (0..80) of which cell is being checked
;;;         puzzle-array - the 1-d array of the currently-assigned cells
;;;*******************************************************************************

(defun assign-allowed-puzzle (value cell puzzle-array)
  (check-constraints value
		     (list (floor cell 9) (mod cell 9))
		     puzzle-array
		     (row-col-constraints-a cell puzzle-array
					    (sub-board-constraints-a
					     cell puzzle-array))))


;;;*******************************************************************************
;;; This function converts the puzzle (list of 9 lists of 9) to an array for 
;;; certain computations.
;;; INPUT:  puzzle - list of 9 lists of 9
;;;*******************************************************************************

(defun puzzle-to-array (puzzle)
  (make-array (list (length puzzle)
		    (length (first puzzle)))
	      :initial-contents puzzle))


;;;*******************************************************************************
;;; This function converts a puzzle-array to a puzzle (list of 9 lists of 9) for
;;; certain computations.                                
;;; INPUT:  puzzle-array - array of 9 lists of 9
;;;         puzzle - accumulating variable containing the new list puzzle
;;;         count - incrementing argument
;;;*******************************************************************************

(defun array-to-puzzle (puzzle-array &optional (puzzle nil) (count 0))
  (if (eq 9 count)
      puzzle
    (array-to-puzzle 
     puzzle-array 
     (append puzzle 
	     (list (loop for col upto 8
			 collect (aref puzzle-array count col))))
     (1+ count))))
	



;;;*******************************************************************************
;;;                             Unused Functions
;;;*******************************************************************************


;;;*******************************************************************************
;;; This function cycles through the array of remaining possible values and
;;; assigns cells with a length-1 domain to the value in that domain and returns
;;; the puzzle with these assignments.
;;; INPUTS:  puzzle - list of 9 rows, which are lists of 9 column values
;;;          inferences - a list of remaining possible cell values
;;;          count - incrementing argument
;;;*******************************************************************************

(defun assign-from-inferences (puzzle inferences &optional (count 0))
  (if (not (arrayp puzzle))
      (assign-from-inferences (puzzle-to-array puzzle) inferences count)
    (if (eq count 81)
        (array-to-puzzle puzzle)
      (let ((row (floor count 9))
            (col (mod count 9))
            (domain (aref inferences count)))
        (if (and (not (eq 0 (first domain)))
                 (eq 1 (length domain)))
            (assign-from-inferences
             (assign (first domain) count (array-to-puzzle puzzle))
             inferences
             (1+ count))
          (assign-from-inferences puzzle inferences (1+ count)))))))
  

;;;*******************************************************************************
;;; This function unassigns (assigns to 0) the cell in the puzzle and returns a
;;; new puzzle (non-destructive).
;;; INPUT:  cell - the number (0..80) of the cell being assigned
;;;         puzzle - the list of 9 rows, which are lists of 9 cell assignments
;;;*******************************************************************************

(defun unassign (cell puzzle)
  (let ((row (floor cell 9))
        (col (mod cell 9)))
    (append (subseq puzzle 0 row)
            (list (append (subseq (nth row puzzle) 0 col)
                          (list 0)
                          (subseq (nth row puzzle) (1+ col) 9)))
            (subseq puzzle (1+ row) 9))))