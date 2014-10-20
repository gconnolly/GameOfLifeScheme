;;;functions to manage a set of tuples

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
		((member (car set1) set2) (+ 1 (intersection-count (cdr set1) set2)))
		(else (intersection-count (cdr set1) set2))
	)
)

;;;functions to execute conway's game of life

(define (get-neighbors cell)
	(list
		(list (+ (car cell) 1) 	(+ (cadr cell) 1)	)
		(list (+ (car cell) 1) 	(- (cadr cell) 1)	)
		(list (+ (car cell) 1) 	(cadr cell)		)

		(list (- (car cell) 1) 	(+ (cadr cell) 1)	)
		(list (- (car cell) 1) 	(- (cadr cell) 1)	)
		(list (- (car cell) 1) 	(cadr cell)		)

		(list (car cell)	(+ (cadr cell) 1)	)
		(list (car cell)	(- (cadr cell) 1)	)
	)
)

(define (cell-will-live? cell living-cells)
	(define neighbor-count (intersection-count (get-neighbors cell) living-cells))
	(or 
		(and 
			(member cell living-cells) 
			(or 
				(= 3 neighbor-count) 
				(= 2 neighbor-count)
			)
		) 
		(= 3 neighbor-count)
	)
)

(define (determine-fate-of-cells cells living-cells)
	(cond
		((null? cells) '())
		((cell-will-live? (car cells) living-cells) 
			(cons (car cells) (determine-fate-of-cells (cdr cells) living-cells))
		)
		(else 
			(determine-fate-of-cells (cdr cells) living-cells)
		)
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

(define (repeat func val times)
	(cond
		((= 0 times) val)
		(else
			(apply func (list (repeat func val (- times 1))))
		)
	)
)

;;;functions to draw a bounded grid of the current state

(define (draw-tuple-grid ominX minX maxX minY maxY set)
	(cond
		((and (> minX maxX) (> minY maxY)) "")
		((> minX maxX)
			(string-append 
				(draw-tuple-grid ominX ominX maxX (+ 1 minY) maxY set)
				(string #\newline)
			)
		)			
		(else
			(string-append 
				(draw-tuple-grid ominX (+ 1 minX) maxX minY maxY set)
				(cond ((member (list minX minY) set) "*") (else  " ") )
			)
		)
	)
)

(define (all-x set)
	(map (lambda (e) (car e)) set)
)

(define (all-y set)
	(map (lambda (e) (cadr e)) set)
)

(define (draw-tuple-set set)
	;Determine bounding rectangle
	(define minX (apply min (all-x set)))
	(define maxX (apply max (all-x set)))
	(define minY (apply min (all-y set)))
	(define maxY (apply max (all-y set)))

	(draw-tuple-grid minX minX maxX minY maxY set)
)

;;;functions execute from command-line-interpreter

(define (main args) 
	(print 
		(draw-tuple-set 
			(repeat 
				evolve
				'((1 2) (2 4) (3 1) (3 2) (3 5) (3 6) (3 7))
				;'((1 2) (2 3) (3 1) (3 2) (3 3)) 
				(string->number (car args))
			)
		)
	)
)
