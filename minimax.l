;Joseph Sarabia
;Lab 2 
;10/24/13
;CAP 4630

(defun m-deepest(l)
 	(cond
	 ((null l)		NIL)
	 ;get rid of all the atoms each time, then the last remaining list of only atoms will be the deepest
	 ((has-lists l)		(m-deepest (reduce #'append (kill-atoms l))))
	 (T			(car l))
	)
)
	 			
;function defined in class
(defun kill-atoms(L)
  (mapcan #'(lambda(x)
	(cond
	 ((atom x) nil)
	 (t (list x))
	)) L)
)

;checks to see if an element in the list is not an atom
(defun has-lists(L)
	(cond
	((null L)	NIL)
	((not(atom (car L)))		T)
	(T				(has-lists(cdr l)))
	)
)

(defun m-depth(l)
 	(cond 
	 ((null l)			0)
	 ((not (has-lists l))		1)
	 ;kill all atoms until all is left is a list of atoms, each time you keep going, add one
	 (T				(max (reduce #'max (mapcar #'1+ (mapcar #'m-depth (kill-atoms l))))))
	)
)

(defun minimax(l)
	(cond
	 ((null l)	nil)
	 ;find the index of the minimax value; while the root chooses max, the first branch chooses min, so use min for finding
	 ;the index. start with max for finding the minimax value
	 (T		(list (findindex (max (mmax l)) (mapcar #'mmin l) ) (max (mmax l)) ))
	)
)

;traverses through to find the index in a list with the given value
(defun findindex(n l)
 	(cond 
	 ((null l)		0)
	 ((atom l)		 1)
	 ((eq n (car l))		1)
 	 (T						(1+ (findindex n (cdr l))))
	)
)	

(defun mmax(l)
 	(cond
	;-1 is chosen because it will be lower than anything else- nil will break the max function
	 ( (null l)		-1)
	 ( (atom l)		l)
	 ;if the first element is an atom, compare it to the max of the rest of the list
	 ( (atom(car l))	(max (car l) (mmax (cdr l))))
	 ;if the first element is a list, pass it to min, then take the max of it with the rest of the list
	 ( T			(max (mmin (car l)) (mmax (cdr l))))
	)
)

;opposite of mmax
(defun mmin(l)
	(cond 
	 ( (null l)		301)
	 ( (atom l)		l)
	 ( (atom (car l))  	(min (car l) (mmin (cdr l))))
	 ( T			(min (mmax (car l)) (mmin (cdr l))))
	)
)
