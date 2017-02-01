;;;*******************************************************************************
;;;                         Sudoku Constraint Functions
;;;*******************************************************************************


;;;*******************************************************************************
;;; This function adds the list of cells (list of (row col) 2d indices) that are   
;;; in the same sub-board as the cell.                                             
;;; INPUTS:  cell - the index (0..80) of which cell is being checked               
;;;          constraints - previously-defined list of constraints to add to
;;;*******************************************************************************

(defun sub-board-constraints-a (cell puzzle &optional (constraints nil))
  (if (not (arrayp puzzle))
      (sub-board-constraints cell (puzzle-to-array puzzle) constraints))
  (let* ((row (floor cell 9))
         (col (mod cell 9))
         (sub-row
          (cond ((<= row 2) 0)
                ((>= row 6) 2)
                (t 1)))
         (sub-col
          (cond ((<= col 2) 0)
                ((>= col 6) 2)
                (t 1)))
         (sub-b-r (mod row 3))
         (sub-b-c (mod cell 3)))
    (append constraints
            ;; 2-d array indices of sub-board cells                                
            (let ((first-r (* sub-row 3))
                  (first-c (* sub-col 3)))
              (remove nil
                      (list
                       (if (not (and (eq sub-b-r 0) (eq sub-b-c 0)))
                           (list first-r first-c))
                       (if (not (and (eq sub-b-r 0) (eq sub-b-c 1)))
                           (list first-r (1+ first-c)))
                       (if (not (and (eq sub-b-r 0) (eq sub-b-c 2)))
                           (list first-r (+ 2 first-c)))
                       (if (not (and (eq sub-b-r 1) (eq sub-b-c 0)))
                           (list (1+ first-r) first-c))
                       (if (not (and (eq sub-b-r 1) (eq sub-b-c 1)))
                           (list (1+ first-r) (1+ first-c)))
                       (if (not (and (eq sub-b-r 1) (eq sub-b-c 2)))
                           (list (1+ first-r) (+ 2 first-c)))
                       (if (not (and (eq sub-b-r 2) (eq sub-b-c 0)))
                           (list (+ 2 first-r) first-c))
                       (if (not (and (eq sub-b-r 2) (eq sub-b-c 1)))
                           (list (+ 2 first-r) (1+ first-c)))
                       (if (not (and (eq sub-b-r 2) (eq sub-b-c 2)))
                           (list (+ 2 first-r) (+ 2 first-c)))))))))


;;;*******************************************************************************
;;; This function adds the list of cells (list of (row col) 2d indices) that are   
;;; in the same row and column as the cell !!!except for those in the same         
;;; sub-board!!!.                                                                  
;;; INPUTS:  cell - the 1d index (0..80) of the cell being checked            
;;;          constraints - pre-determined list of constraints to add to   
;;;*******************************************************************************

(defun row-col-constraints-a (cell puzzle &optional (constraints nil))
  (if (not (arrayp puzzle))
      (row-col-constraints cell (puzzle-to-array puzzle) constraints))
  (let* ((row (floor cell 9))
         (col (mod cell 9))
         (sub-row
          (cond ((<= row 2) 0)
                ((>= row 6) 2)
                (t 1)))
         (sub-col
          (cond ((<= col 2) 0)
                ((>= col 6) 2)
                (t 1))))
    (append constraints
            ;; add the row constraints                                             
            (remove nil
                    (append (if (not (eq sub-col 0))
                                (list (list row 0)
                                      (list row 1)
                                      (list row 2)))
                            (if (not (eq sub-col 1))
                                (list (list row 3)
                                      (list row 4)
                                      (list row 5)))
                            (if (not (eq sub-col 2))
                                (list (list row 6)
                                      (list row 7)
                                      (list row 8)))))
            ;; add the column constraints                                          
            (remove nil
                    (append (if (not (eq sub-row 0))
                                (list (list 0 col)
                                      (list 1 col)
                                      (list 2 col)))
                            (if (not (eq sub-row 1))
                                (list (list 3 col)
                                      (list 4 col)
                                      (list 5 col)))
                            (if(not (eq sub-row 2))
                                (list (list 6 col)
                                      (list 7 col)
                                      (list 8 col))))))))


;;;*******************************************************************************
;;; this function tests whether the value can be assigned to cell in the puzzle    
;;; considering the other assigned values of that cell's sub-board (3x3 square).   
;;; INPUTS:  value - the assignment value proposed for cell in puzzle              
;;;          cell-2d - the index ((0 0)..(8 8)) of which cell is being checked     
;;;          puzzle-array - the 1-d array of the currently-assigned cells
;;;          constraints - this is a list of constraints (2-d indices of cells
;;;                        related to the current cell (cell-2d) by a constraint
;;;*******************************************************************************

(defun check-constraints (value cell-2d puzzle-array constraints)
  (if (null constraints)
      t
    (let* ((cnstrnt (first constraints))
           (row (first cnstrnt))
           (col (second cnstrnt)))
      (if (eq (aref puzzle-array row col) value)
          nil                   ; this sub-board cell has this value               
        (check-constraints value cell-2d puzzle-array
                           (rest constraints))))))