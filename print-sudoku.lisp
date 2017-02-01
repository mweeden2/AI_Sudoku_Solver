;;;*******************************************************************************
;;;                          Sudoku Printing Functions
;;;*******************************************************************************


;;;*******************************************************************************
;;; This function prints a puzzle to the standard output.                          
;;; INPUTS:  puzzle - list of 9 lists of 9                                         
;;;*******************************************************************************

(defun print-puzzle (puzzle &optional (count 0))
  (if (null puzzle)
      (format t "~&There is no solution to this puzzle.")
    (if (not (arrayp puzzle))
	(print-puzzle (puzzle-to-array puzzle))
      (if (eq 9 count)
	  nil
	(progn (if (or (eq 3 count) (eq 6 count))
		   (format t "~& ---------------------"))
	       (progn (format t "~& ~a ~a ~a | ~a ~a ~a | ~a ~a ~a"
			      (aref puzzle count 0)
			      (aref puzzle count 1)
			      (aref puzzle count 2)
			      (aref puzzle count 3)
			      (aref puzzle count 4)
			      (aref puzzle count 5)
			      (aref puzzle count 6)
			      (aref puzzle count 7)
			      (aref puzzle count 8))
		      (print-puzzle puzzle (1+ count))))))))


;;;*******************************************************************************
;;; This function prints the remaining possible values of a puzzle.                
;;; INPUTS: remaining-values - and array of cell domains.                          
;;;*******************************************************************************

(defun print-domain (remaining-values &optional (count 0) (longest-domain 0))
  (if (null remaining-values)
      nil
    (if (eq 0 longest-domain)
	(print-domain remaining-values count
		      (longest-domain remaining-values))
      (if (eq 9 count)
	  nil
	(progn (if (or (eq 3 count) (eq 6 count))
		   (format t "~& ~a"
			   (make-string
			    (* 20 longest-domain) :initial-element #\-)))
	       (let ((dom-0 (aref remaining-values (* 9 count)))
		     (dom-1 (aref remaining-values (1+ (* 9 count))))
		     (dom-2 (aref remaining-values (+ 2 (* 9 count))))
		     (dom-3 (aref remaining-values (+ 3 (* 9 count))))
		     (dom-4 (aref remaining-values (+ 4 (* 9 count))))
		     (dom-5 (aref remaining-values (+ 5 (* 9 count))))
		     (dom-6 (aref remaining-values (+ 6 (* 9 count))))
		     (dom-7 (aref remaining-values (+ 7 (* 9 count))))
		     (dom-8 (aref remaining-values (+ 8 (* 9 count)))))
		 (progn
		   (format t "~& ~a ~a~a ~a~a ~a| ~a ~a~a ~a~a ~a | ~a ~a~a ~a~a ~a"
			   dom-0
			   (make-string (* 2 (- longest-domain (length dom-0)))
					:initial-element #\ )
			   dom-1
			   (make-string (* 2 (- longest-domain (length dom-1)))
					:initial-element #\ )
			   dom-2
			   (make-string (* 2 (- longest-domain (length dom-2)))
					:initial-element #\ )
			   dom-3
			   (make-string (* 2 (- longest-domain (length dom-3)))
					:initial-element #\ )
			   dom-4
			   (make-string (* 2 (- longest-domain (length dom-4)))
					:initial-element #\ )
			   dom-5
			   (make-string (* 2 (- longest-domain (length dom-5)))
					:initial-element #\ )
			   dom-6
			   (make-string (* 2 (- longest-domain (length dom-6)))
					:initial-element #\ )
			   dom-7
			   (make-string (* 2 (- longest-domain (length dom-7)))
					:initial-element #\ )
			   dom-8
			   (make-string (* 2 (- longest-domain (length dom-8)))
					:initial-element #\ ))
		   (print-domain remaining-values (1+ count)))))))))


;;;*******************************************************************************
;;; This function returns the length of the longest cell domain in the             
;;; remaining-values array                                                         
;;; INPUTS:  remaining-values - array of cell domains                              
;;;*******************************************************************************

(defun longest-domain (remaining-values &optional (count 0) (max-length 0))
  (if (eq 81 count)
      max-length
    (let ((domain-length (length (aref remaining-values count))))
      (if (> domain-length max-length)
          (longest-domain remaining-values (1+ count) domain-length)
        (longest-domain remaining-values (1+ count) max-length)))))