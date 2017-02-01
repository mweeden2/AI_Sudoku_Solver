;;;******************************************************************************* 
;;;                             Sudoku Arc Functions
;;;******************************************************************************* 


;;;*******************************************************************************
;;; This function returns the arcs between the cell and its neighbors that aren't 
;;; already in the list of arcs.
;;; INPUTS: cell - the index (0..80) of the cell whose neighbor-arcs are being    
;;;                generated                                                      
;;;         arcs - the list of current arcs not to be included in this generation 
;;;*******************************************************************************

(defun get-neighbor-arcs (cell puzzle arcs &optional (new-arcs nil) (count 999))
  (if (eq count 999)
      (get-neighbor-arcs cell puzzle arcs (get-cell-arcs cell puzzle) 20)
    (if (eq count 0)
        new-arcs
      ;; check this new-arc against each arc in arcs                               
      (if (loop for arc in arcs do
                ;; if this new-arc is equal to this arc                            
                (if (equal (nth (1- count) new-arcs) arc)
                    (return nil))
                ;; this line make the loop pass as TRUE if nil is never returned   
                collect t)
          (get-neighbor-arcs cell puzzle arcs new-arcs (1- count))
        (get-neighbor-arcs
         cell puzzle arcs
         (append (subseq new-arcs 0 (1- count))
                 (subseq new-arcs
                         count
                         (length new-arcs)))
         (1- count))))))


;;;*******************************************************************************
;;; This function returns all arcs, which are length-2 lists of the 2-d indices    
;;; of each pair of cells that are related by a constraint (in the same sub-board,
;;; row, or column.                                                                
;;; INPUTS:  puzzle - a list of 9 rows, which are lists of 9 column values
;;;          arcs - list of arcs that accumulates as get-all-arcs is called        
;;;                 recursively                                                    
;;;          cell - the cell number whose arcs are currently being generated       
;;;*******************************************************************************

(defun get-all-arcs (puzzle &optional (arcs nil) (cell 0))
  (if (not (arrayp puzzle))
      (get-all-arcs (puzzle-to-array puzzle))
    (if (eq cell 81)
        arcs
      (let ((row (floor cell 9))
            (col (mod cell 9)))
        (if (eq 0 (aref puzzle row col)) ; if this cell is not assigned            
            (get-all-arcs
             puzzle (append (get-cell-arcs cell puzzle) arcs) (1+ cell))
          (get-all-arcs puzzle arcs (1+ cell)))))))


;;;*******************************************************************************
;;; This function returns all arcs, which are length-2 lists of the 2-d indices    
;;; of each pair of cells that are related by a constraint (in the same sub-board,
;;; row, or column.                                                                
;;; INPUTS:  puzzle - a list of 9 rows, which are lists of 9 column values
;;;          arcs - list of arcs that accumulates as get-all-arcs is called        
;;;                 recursively                                                    
;;;          cell - the cell number whose arcs are currently being generated       
;;;*******************************************************************************

(defun get-cell-arcs (cell puzzle &optional (arcs nil) (count 0))
  (if (eq count 20)
      arcs
    (let ((row (floor cell 9))
          (col (mod cell 9))
          (cell-constraints
           (let ((sub-const (sub-board-constraints-b cell puzzle)))
              (row-col-constraints-b cell puzzle sub-const))))
      (map 'list #'list
           (make-list 20 :initial-element (list row col)) cell-constraints))))



;;;*******************************************************************************
;;; This function adds the list of cells (list of (row col) 2d indices) that are   
;;; in the same sub-board as the cell. Compared to sub-board-constraints-a, this   
;;; function does not return the constraints for cells that are already assigned,  
;;; which is used for arc consistency.                                             
;;; INPUTS:  cell - the index (0..80) of which cell is being checked               
;;;          puzzle - a list of 9 rows, which are lists of 9 column values
;;;          constraints - previously-defined                                      
;;;*******************************************************************************

(defun sub-board-constraints-b (cell puzzle &optional (constraints nil))
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
                       (if (and
                            (not (and (eq sub-b-r 0) (eq sub-b-c 0)))
                            (eq (aref puzzle first-r first-c) 0))
                           (list first-r first-c))
                       (if (and
                            (not (and (eq sub-b-r 0) (eq sub-b-c 1)))
                            (eq (aref puzzle first-r (1+ first-c)) 0))
                           (list first-r (1+ first-c)))
                       (if (and
                            (not (and (eq sub-b-r 0) (eq sub-b-c 2)))
                            (eq (aref puzzle first-r (+ 2 first-c)) 0))
                           (list first-r (+ 2 first-c)))
                       (if (and
                            (not (and (eq sub-b-r 1) (eq sub-b-c 0)))
                            (eq (aref puzzle (1+ first-r) first-c) 0))
                           (list (1+ first-r) first-c))
                       (if (and
                            (not (and (eq sub-b-r 1) (eq sub-b-c 1)))
                            (eq (aref puzzle (1+ first-r) (1+ first-c)) 0))
                           (list (1+ first-r) (1+ first-c)))
                       (if (and
                            (not (and (eq sub-b-r 1) (eq sub-b-c 2)))
                            (eq (aref puzzle (1+ first-r) (+ 2 first-c)) 0))
                           (list (1+ first-r) (+ 2 first-c)))
                       (if (and
                            (not (and (eq sub-b-r 2) (eq sub-b-c 0)))
                            (eq (aref puzzle (+ 2 first-r) first-c) 0))
                           (list (+ 2 first-r) first-c))
                       (if (and
                            (not (and (eq sub-b-r 2) (eq sub-b-c 1)))
                            (eq (aref puzzle (+ 2 first-r) (1+ first-c)) 0))
                           (list (+ 2 first-r) (1+ first-c)))
                       (if (and
                            (not (and (eq sub-b-r 2) (eq sub-b-c 2)))
                            (eq (aref puzzle (+ 2 first-r) (+ 2 first-c)) 0))
                           (list (+ 2 first-r) (+ 2 first-c)))))))))


;;;*******************************************************************************
;;; This function adds the list of cells (list of (row col) 2d indices) that are   
;;; in the same row and column as the cell !!!except for those in the same         
;;; sub-board!!!. Compared to row-col-constraints-a, this function does not        
;;; include already-assigned cells.                                                
;;; INPUTS:  cell - the 1d index (0..80) of the cell being checked        
;;;          puzzle - a list of 9 rows, which are lists of 9 column values         
;;;          constraints - pre-determined list of constraints to add to            
;;;*******************************************************************************

(defun row-col-constraints-b (cell puzzle &optional (constraints nil))
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
                        (list
                         (if (eq (aref puzzle row 0) 0)
                             (list row 0))
                         (if (eq (aref puzzle row 1) 0)
                              (list row 1))
                         (if (eq (aref puzzle row 2) 0)
                              (list row 2))))
                    (if (not (eq sub-col 1))
                        (list
                         (if (eq (aref puzzle row 3) 0)
                             (list row 3))
                         (if (eq (aref puzzle row 4) 0)
                              (list row 4))
                         (if (eq (aref puzzle row 5) 0)
                              (list row 5))))
                    (if (not (eq sub-col 2))
                        (list
                         (if (eq (aref puzzle row 6) 0)
                             (list row 6))
                         (if (eq (aref puzzle row 7) 0)
                              (list row 7))
                         (if (eq (aref puzzle row 8) 0)
                              (list row 8))))))
            ;; add the column constraints                                          
            (remove nil
                    (append (if (not (eq sub-row 0))
                        (list
                         (if (eq (aref puzzle 0 col) 0)
                             (list 0 col))
                         (if (eq (aref puzzle 1 col) 0)
                              (list 1 col))
                         (if (eq (aref puzzle 2 col) 0)
                              (list 2 col))))
                    (if(not (eq sub-row 1))
                        (list
                         (if (eq (aref puzzle 3 col) 0)
                             (list 3 col))
                         (if (eq (aref puzzle 4 col) 0)
                              (list 4 col))
                         (if (eq (aref puzzle 5 col) 0)
                              (list 5 col))))
                    (if(not (eq sub-row 2))
                        (list
                         (if (eq (aref puzzle 6 col) 0)
                             (list 6 col))
                         (if (eq (aref puzzle 7 col) 0)
                              (list 7 col))
                         (if (eq (aref puzzle 8 col) 0)
                              (list 8 col)))))))))