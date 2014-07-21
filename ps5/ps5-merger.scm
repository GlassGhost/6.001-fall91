;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                          ;;
;; PS5-MERGER.SCM -- Scheme code for Merger Mania problem set 5   Fall 1991 ;;
;;                                                                          ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; This is the code for Problem Set 5 --- Merger Mania

;;;---------------------------------------------------------------------------
;;; LOOKUP, INSERT and DELETE of Big Aquamarine

(define (lookup-unordered name file)
  (define (lookup-aux records)
    (cond ((empty? records) empty-folder)
	  ((eq? name (id (curr-folder records)))
	   (curr-folder records))
	  (else (lookup-aux (rest-folders records)))))
  (if (table-header? file)
      (lookup-aux (folders file))
      (error "LOOKUP-UNORDERED: file must be a table header" file)))

(define (insert-unordered name record file)
  (define (insert-aux name record records)
    (cond ((empty? records)
	   (let ((folder (attach name record)))
	     (attach folder records)))
	  ((eq? (id (curr-folder records)) name)
	   (let ((curr-info (info (curr-folder records))))
	     (let ((new-info (if (and (record-table? curr-info)
				      (table-header? (body record)))
				 ;; Recursive file structure
				 (insert-aux (id   (body record))
					     (info (body record))
					     curr-info)
				 (cons (body record) curr-info))))
	       (let ((folder (attach name new-info)))
		 (attach folder (rest-folders records))))))
	  (else (attach (curr-folder records)
			(insert-aux name record (rest-folders records))))))
  (if (table-header? file)
      (if (record-table? record)
	  (attach (header file)
		  (insert-aux name record (folders file)))
	  (error "INSERT-UNORDERED: record must be created by MAKE-RECORD-TABLE"
		 record))
      (error "INSERT-UNORDERED: file must be a table header" file)))

(define (delete-unordered name file)
  (define (delete-aux records)
    (cond ((empty? records) records)
	  ((eq? (id (curr-folder records)) name)
	   (rest-folders records))
	  (else (attach (curr-folder records)
		        (delete-aux (rest-folders records))))))
  (if (table-header? file)
      (attach (header file)
	      (delete-aux (folders file)))
      (error "DELETE-UNORDERED: file must be a table header" file)))

;;;---------------------------------------------------------------------------
;;; LOOKUP, INSERT and DELETE for Quince 


; #!$@%^*%#@!&$$#%@!... Oops! Destroyed in a fire.


;;;------------------------------------------------------------
;;; Constructor of Quince and Big Aquamarine's employee record

(define (make-record-table id info)
  (list (cons id info)))

(define body car)

(define id   car)
(define info cdr)

(define (record-table? obj)   ;; Non-null list of pairs (a non-null assoc list)
  (if (pair? obj)
      (if (pair? (car obj))
	  (if (null? (cdr obj))
	      true
	      (record-table? (cdr obj)))
	  false)
      false))

;;;-------------------------
;;; Abstraction definitions ... used throughout

(define (table-header? obj)	;; Tagged list of table-records
  (and (symbol? (car obj))
       (or (null? (cdr obj))
	   (record-table? (cdr obj)))))

(define attach      cons)
(define header       car)
(define folders      cdr)
(define curr-folder  car)
(define rest-folders cdr)
(define empty-folder '())
(define empty?     null?)


;;;---------------------------------------------------------------------------
;;; LOOKUP, INSERT and DELETE for Helios

(define (lookup-tree name file)
  (define (lookup-aux records)
    (let ((next-branch (choose-branch name records)))
      (cond ((not next-branch) empty-folder)
	    ((leaf? next-branch) (content next-branch))
	    (else (lookup-aux next-branch)))))
  (if (tree-header? file)
      (lookup-aux (folders file))
      (error "LOOKUP-TREE: file must be a tree header" file)))

(define (choose-branch name tree)
  (let (( left-branch (left  tree))
	(right-branch (right tree)))
    (cond ((memq name (symbols  left-branch))  left-branch)
	  ((memq name (symbols right-branch)) right-branch)
	  (else false))))

(define (insert-tree name record file)
  (define (insert-aux name record records)
    (if (empty? records)
	(insert-aux name record (make-tree bare-tree bare-tree))
	(let ((path (choose-path name records)))
	  (if (not path)
	      (let ((new-leaf (make-leaf name record))
		    (l-branch (left  records))
		    (r-branch (right records))
		    (leftist? (zero? (random 2)))) ;; 50/50 chance
		;; Try to maintain some semblance of balance...
		(cond ((and (bare? l-branch) (bare? r-branch))
		       (if leftist?
			   (make-tree new-leaf r-branch)
			   (make-tree l-branch new-leaf)))
		      ((bare? l-branch)
		       (make-tree new-leaf r-branch))
		      ((bare? r-branch)
		       (make-tree l-branch new-leaf))
		      ((and (leaf? l-branch) (leaf? r-branch))
		       (if leftist?
			   (make-tree (make-tree new-leaf l-branch)
				      r-branch)
			   (make-tree l-branch
				      (make-tree r-branch new-leaf))))
		      ((leaf? l-branch)
		       (make-tree (make-tree new-leaf l-branch)
				  r-branch))
		      ((leaf? r-branch)
		       (make-tree l-branch
				  (make-tree r-branch new-leaf)))
		      (else
		       (if leftist?
			   (make-tree (insert-aux name record l-branch)
				      r-branch)
			   (make-tree l-branch
				      (insert-aux name record r-branch))))))
	      (let ((branch (choose-branch name records)))
		(if (leaf? branch)
		    (let ((new-branch
			    (if (and (tree? (value branch))
				     (record-tree? record))
				;; Recursive file structure
				(let ((new-entry (if (bare? (left record))
						     (right record)
						     (left  record))))
				  (make-leaf name
					     (insert-aux (symbol new-entry)
							 (value  new-entry)
							 (value branch))))
				(make-tree (make-leaf name record)
					   branch))))
		      (if (eq? path 'left)
			  (make-tree new-branch (right records))
			  (make-tree (left records) new-branch)))
		    ;; Invariant: must be a subtree
		    (if (eq? path 'left)
			(make-tree (insert-aux name record branch)
				   (right records))
			(make-tree (left records)
				   (insert-aux name record branch)))))))))
  (if (tree-header? file)
      (if (record-tree? record)
	  (attach (header file) (insert-aux name record (folders file)))
	  (error "INSERT-TREE: record must be created by MAKE-RECORD-TREE"
		 record))
      (error "INSERT-TREE: file must be a tree-header" file)))

(define (delete-tree name file)
  (define (delete-aux records)
    (let ((path (choose-path name records)))
      (cond ((eq? 'left path)
	     (if (leaf? (left records))
		 (if (bare? (right records))
		     bare-tree
		     (make-tree bare-tree (right records)))
		 (make-tree (delete-aux (left records))
			    (right records))))
	    ((eq? 'right path)
	     (if (leaf? (right records))
		 (if (bare? (left records))
		     bare-tree
		     (make-tree (left records) bare-tree))
		 (make-tree (left records)
			    (delete-aux (right records)))))
	    (else records))))
  (if (tree-header? file)
      (attach (header file) (delete-aux (folders file)))
      (error "DELETE-TREE: file must be a tree header" file)))


(define (choose-path name tree)
  (cond ((memq name (symbols (left  tree)))  'left)
	((memq name (symbols (right tree))) 'right)
	(else false)))


(define (tree-header? obj)	;; Appropriately tagged tree
  (and (symbol? (car obj))
       (not (memq (car obj) '(leaf tree)))
       (tree? (cdr obj))))

;;;---------------------------------------------------------
;;; Structuring a tree (abstraction definitions for Helios)

(define (make-tree left right)
  (list 'tree left right (append (symbols left) (symbols right))))

(define left  cadr )
(define right caddr)

(define (symbols tree)
  (cond ((bare? tree) '())
	((leaf? tree) (list (symbol tree)))
        (else (cadddr tree))))

(define (tree? obj)
  (or (bare?   obj)
      (leaf?   obj)
      (branch? obj)))

(define (branch? obj)
  (if (non-null-list? obj)
      (and (eq? (car obj) 'tree)
	   (if (= (length obj) 4)
	       (and (tree? (cadr   obj))
		    (tree? (caddr  obj))
		    (list? (cadddr obj)))
	       false))
      false))

(define (non-null-list? obj)
  (and (not (null? obj)) (list? obj)))


(define (make-leaf name record)
  (list 'leaf (cons name record)))

(define (leaf? obj)
  (if (non-null-list? obj)
      (and (eq? (car obj) 'leaf)
	   (if (= (length obj) 2)
	       (pair? (cadr obj))
	       false))
      false))

(define (content leaf)
  (cadr leaf))

(define (symbol leaf)
  (caadr leaf))

(define (value leaf)
  (cdadr leaf))


(define bare-tree '())

(define (bare? obj)
  (null? obj))


;;;----------------------------------------
;;; Constructor of Helios' employee record

(define (make-record-tree id info)
  (if (zero? (random 2))	;; 50/50 chance
      (make-tree (make-leaf id info) bare-tree)
      (make-tree bare-tree (make-leaf id info))))

(define (record-tree? obj) (branch? obj))

;;;------------------------------------------------
;;; All the personnel files (miniaturized versions)

;;;********
;;; CAVEAT  These are defined solely as examples of what the companies' actual
;;;******** internal representations look like. In a real personnel file, the
;;;         entries would have been created not by using QUOTE but by using
;;;         repeated calls to INSERT-<mumble> with arguments constructed out
;;;         of calls to MAKE-RECORD-TABLE and MAKE-RECORD-TREE. See below.

(define big-aqua (list 'big-aqua
		       '(moe (salary . 40000) (address 88 main  st))
		       '(joe (salary . 30000) (address 77 mass ave))))

(define quince (list 'quince
		     '(jane (address 350 memorial dr) (salary . 64000))
		     '(ruth (address  90   summer st))))

(define helios (list 'helios 'tree
			     '(leaf (bob tree
					 (leaf (address 23 summer st))
					 (leaf (salary . 45300))
					 (address salary)))
			     '(leaf (amy tree
					 ()
					 (leaf (address 79 winter st))
					 (address)))
			     '(bob amy)))

;;;
;;; You should confirm that these are equivalent to having performed:
;;;

;;; Big Aquamarine: <mumble>-unordered

(define (construct-big-aqua)
  (define (moe-a file)
    (insert-unordered 'moe (make-record-table 'address '(88 main  st)) file))
  (define (joe-a file)
    (insert-unordered 'joe (make-record-table 'address '(77 mass ave)) file))
  (define (joe-s file)
    (insert-unordered 'joe (make-record-table 'salary       30000    ) file))
  (define (moe-s file)
    (insert-unordered 'moe (make-record-table 'salary       40000    ) file))
  (let ((go-file (list 'big-aqua)))
    ;; NB: The order in which these are applied makes a difference.
    (moe-s (joe-s (joe-a (moe-a go-file))))))
		       
(define (big-aqua-wins?)
  (equal? big-aqua (construct-big-aqua)))


;;; Quince:  <mumble>-ordered... you must define <mumble>-ordered procs first.

(define (construct-quince)
  ;; Structured like as big-aqua except generated using ORDERED-INSERT
  (define (ru-a file)
    (insert-ordered 'ruth (make-record-table 'address '( 90 summer   st)) file))
  (define (ja-a file)
    (insert-ordered 'jane (make-record-table 'address '(350 memorial dr)) file))
  (define (ja-s file)
    (insert-ordered 'jane (make-record-table 'salary        64000       ) file))
  ;; NB: The order in which these are applied makes NO difference
  (let ((go-file (list 'quince)))
    (ja-s (ja-a (ru-a go-file)))))

(define (quince-wins?)
  (equal? quince (construct-quince)))


;;; Helios:  <mumble>-tree

(define (construct-helios)
  ;;; Grumble... we must stutter-step to account for randomized inserts
  (define (amy file)
    (let ((try (insert-tree 'amy (make-record-tree 'address '(79 winter st))
			    file)))
      (if (and (eq? 'right
		    (choose-path 'amy     (folders                   try )))
	       (eq? 'right
		    (choose-path 'address (folders (lookup-tree 'amy try)))))
	  try
	  (amy file))))
  (define (bob-a file)
    (let ((try (insert-tree 'bob (make-record-tree 'address '(23 summer st))
			    file)))
      (if (and (eq? 'left
		    (choose-path 'bob     (folders                   try )))
	       (eq? 'left
		    (choose-path 'address (folders (lookup-tree 'bob try)))))
	  try
	  (bob-a file))))
  (define (bob-s file)
    (let ((try (insert-tree 'bob (make-record-tree 'salary 45300)
			    file)))
      (if (eq? 'right (choose-path 'salary (folders (lookup-tree 'bob try))))
	  try
	  (bob-s file))))
  ;; NB: The order in which these are applied makes NO difference
  (let ((go-file (list 'helios)))
    (bob-s (bob-a (amy go-file)))))

(define (helios-wins?)
  (equal? helios (construct-helios)))
