;;;*******************************************************************************
;;;                          Sudoku Puzzle Read Files
;;;*******************************************************************************


;;;*******************************************************************************
;;; This function reads the file to extract the number of puzzles to solve and     
;;; calls read-puzzles to collect those puzzles. It returns the puzzles as a list  
;;; of puzzles, which is a list of 9 lists of 9.                                   
;;; INPUT:  filename - the name of the input file containing puzzle(s)             
;;;*******************************************************************************
(defun read-file (filename)
  (let ((in (open filename :if-does-not-exist nil)))
  (when in
    (let ((number-of-puzzles (parse-integer (read-line in nil))))
      (read-puzzles in number-of-puzzles)))))


;;;*******************************************************************************
;;; This function reads the puzzles form the input file (in-file) and returns the  
;;; the puzzles as a list.                                                         
;;; INPUT:  in-file - the name of the input file containing puzzle(s)              
;;;         num-puzzles - the number of puzzles in the file                        
;;;         puzzles - the puzzles being stored with each recursive call to         
;;;                   read-puzzles                                                 
;;;*******************************************************************************
(defun read-puzzles (in-file num-puzzles &optional (puzzles nil))
  (if (eq num-puzzles (length puzzles))
      (progn (close in-file)
             puzzles)
    (let ((next-puzzle (get-puzzle in-file)))
      (if (null puzzles)
          (read-puzzles in-file num-puzzles next-puzzle)
        (read-puzzles in-file num-puzzles
                      (append puzzles next-puzzle))))))


;;;*******************************************************************************
;;; This function reads a single puzzle from in-file and returns it in a list.     
;;; INPUT:  in-file - the name of the input file containing the puzzle             
;;;         puzzle - each line (row) of the puzzle being stored with each          
;;;                  recursive call to get-puzzle                                  
;;;*******************************************************************************
(defun get-puzzle (in-file &optional (puzzle nil))
  (if (eq 9 (length puzzle))
      (list puzzle)
    (get-puzzle in-file (append puzzle (get-line in-file)))))


;;;*******************************************************************************
;;; This function reads a single line from in-file and returns it in a list.       
;;; INPUT:  in-file - the name of the input file containing the puzzle             
;;;         line - each item (cell) of the line being stored with each             
;;;                  recursive call to get-line                                    
;;;*******************************************************************************
(defun get-line (in-file &optional (line nil))
  (if (eq 9 (length line))
      (list line)
    (get-line in-file (append line (list (read in-file))))))