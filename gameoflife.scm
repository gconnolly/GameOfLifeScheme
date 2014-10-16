(define (contains? element set)
	(cond
		((null? set) #f)
		((equal? element (car set)) #t)
	    (else (contains? element (cdr set)))
	)
)

(define (add-to-set element set)
	(cond 
		((null? set) (cons element '()))
	    ((equal? element (car set)) set)
	    (else (cons (car set) (add-to-set element (cdr set))))
	)
)

(define (reduce-to-set elements)
	(cond
		((null? elements) '())
		(else (add-to-set (car elements) (reduce-to-set (cdr elements))))
	)
)

(define (intersection-count set1 set2)
	(cond
		((null? set1) 0)
		((contains? (car set1) set2) (+ 1 (intersection-count (cdr set1) set2)))
		(else (intersection-count (cdr set1) set2))
	)
)

(define (get-neighbors cell)
	(list
		(list (+ (car cell) 1) 	(+ (cadr cell) 1)	)
		(list (+ (car cell) 1) 	(- (cadr cell) 1)	)
		(list (+ (car cell) 1) 	(cadr cell)			)

		(list (- (car cell) 1) 	(+ (cadr cell) 1)	)
		(list (- (car cell) 1) 	(- (cadr cell) 1)	)
		(list (- (car cell) 1) 	(cadr cell)			)

		(list (car cell) 		(+ (cadr cell) 1)	)
		(list (car cell) 		(- (cadr cell) 1)	)
	)
)

(define (determine-fate-of-cell cell living-cells)
	(define neighbor-count (intersection-count (get-neighbors cell) living-cells))
	(cond
		((contains? cell living-cells)
			(cond
				((or (= 3 neighbor-count) (= 2 neighbor-count)) #t)
				(else #f)
			)
		)
		(else
			(cond
				((= 3 neighbor-count) #t)
				(else #f)
			)
		)
	)
)

(define (determine-fate-of-cells cells living-cells)
	(cond
		((null? cells) '())
		((equal? #t (determine-fate-of-cell (car cells) living-cells)) (cons (car cells) (determine-fate-of-cells (cdr cells) living-cells)))
		(else (determine-fate-of-cells (cdr cells) living-cells))
	)
)

(define (get-cells-to-inspect living-cells)
	(cond
		((null? living-cells) '())
		(else 
			(cons 
				(car living-cells) 
				(append 
					(get-neighbors (car living-cells)) 
					(get-cells-to-inspect (cdr living-cells))
				)
			)
		)
	)
)

(define (evolve living-cells)
	(determine-fate-of-cells (reduce-to-set (get-cells-to-inspect living-cells) ) living-cells)
)

(evolve '((1 2) (2 4) (3 1) (3 2) (3 5) (3 6) (3 7)))

(evolve '((1 2) (2 3) (3 1) (3 2) (3 3)))
