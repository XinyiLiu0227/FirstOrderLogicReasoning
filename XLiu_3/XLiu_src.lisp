(defun explode (s)
; Convert s to list of characters.
  (coerce (string (if (numberp s) (write-to-string s) s)) 'list)
) ; END explode

(defun implode (l)
; Convert list of characters to symbol.
  (intern (coerce l 'string))
) ; END implode

(defun type-of-pred (pred)
; Gets the extension of a predicate (v, n, p, a, or f), or returns nil if not a pred.
  (car (remove-if-not (lambda (c) (member c '(v n p a f)))
    (mapcar #'intern (mapcar #'string (last (member #\. (explode pred)))))))
) ; END type-of-pred

(defun name-of-pred (pred)
	; get the type of the argument
	(implode (reverse (cdr (member #\. (reverse (explode pred))))))
) ;END name-of-pred

(defun type-of-argument (a)
	(implode (remove-if #'digit-char-p (explode a)))
)

(defun constant-p (x)
  (and (symbolp x) (let ((chs (explode x)))
    (and (not (find #\. chs)) (digit-char-p (car (last chs)))))))

(defun binary-p (fact)
  ; binary predication can only be of type p or v
  (and (= 3 (length fact)) (member (type-of-pred (car fact)) '(p v))
    (every #'constant-p (cdr fact))))

(defun monadic-p (fact)
  ; monadic predication can only be of type n, a or v
  (and (= 2 (length fact)) (member (type-of-pred (car fact)) '(n a v))
    (every #'constant-p (cdr fact))))

(defun nominal-pred-p (fact)
  (and (monadic-p fact) (equal (type-of-pred (car fact)) 'n)))

(defun episodic-p (fact)
	; episodic predication can only be of type v, contains at most 4 constant arguments
	(and (equal (type-of-pred (car fact)) 'v) (<= (length (cdr fact)) 4)
	 (every #'constant-p (cdr fact)) (equal (type-of-argument (car (last fact))) 'E))

) ;END episodic-p

(defun right-before-p (fact econstants)
	; right-before predication must be binary with distinct episode arguments
	(and (= (length fact) 3) (equal (car fact) 'right-before.p)
	 (not (equal (second fact) (third fact))) 
	 (every (lambda(x)(member x econstants)) (cdr fact)))
) ;END right-before-p

(defun strip-last (x)
	(reverse (cdr (reverse x)))
)


(defun valid-facts-p (facts)
; `````````````````````````````
; Checks whether the given list of facts satisfies the requirements.
;
  (and (listp facts) 
      (let* ((episodic (remove-if-not #'episodic-p facts)) (nominal (remove-if-not #'nominal-pred-p facts))
    	   (nominal-set (mapcar #'second nominal)) (econstants (apply #'append (mapcar #'last episodic)))
    	   (right-before (remove-if-not (lambda(x) (right-before-p x econstants)) facts))
           (episodic-set (remove-duplicates (apply #'append (mapcar (lambda(x) (strip-last (cdr x))) episodic)))))
      (and
      	;(null (set-difference facts (remove-duplicates (append binary monadic episodic) :test #'equal) :test #'equal))
        (>= (length episodic) 2) ; facts need to contains at least two episodic predications
        (>= (length right-before) 1) ; facts need to contains at least one binary 'Right-before.p' predication
        (= (length nominal-set) (length (remove-duplicates nominal-set))) ; exactly one type predication for each constant
        (every (lambda (const) (member const nominal-set)) episodic-set)))) ; every constant in episodic-set occurs in monadic type predicate
) ; END valid-facts-p

(defun form-episode-pairs (facts)
	;````````````````````````````
	; Generate a list of episodic pair from facts about some entity. 
	;
	; episodic pair: 
	;   * the first two elements are distinct episodic predications related by right-before.p predication
	;   * the last element is a list of type predications about the arguments of first two element,
	;     except for the episode argument

	 (when (not (valid-facts-p facts))
      (format t "The list of facts does not satisfy the given requirements.~%")
      (return-from form-episode-pairs '**ERROR**))
	 (let* ((episodic (remove-if-not #'episodic-p facts)) (nominal (remove-if-not #'nominal-pred-p facts))
	    (econstants (apply #'append (mapcar #'last episodic)))
	    (right-before (mapcar #'cdr (remove-if-not (lambda(x) (right-before-p x econstants)) facts)))
	 	(nominal-ht (make-hash-table)) (episodic-ht (make-hash-table)))
	 	; For each nominal fact, add the type predicate (e.g. Monkey.n) to hash table with constant (e.g. Simeon1) as key
	 	(mapcar (lambda (nominal-fact) (setf (gethash (second nominal-fact) nominal-ht) (first nominal-fact))) nominal)
	 	; For each episodic predication, add the episodic predicate to hash table with its last episode argument as key
	 	(mapcar (lambda (episodic-fact) (setf (gethash (car (last episodic-fact)) episodic-ht) episodic-fact)) episodic)
	 	; Generate a list of episodic pair
	 	(mapcar (lambda(right-before-fact) (apply #'append (list 
	 		; a list of two distinct episodic predications
	 		(mapcar (lambda(right-before-episode) (gethash right-before-episode episodic-ht)) right-before-fact) 
	 		; a list of a list of type predications about the arguments of the two episodic predications
	 		(list (mapcar (lambda(right-before-episode) (mapcar (lambda(x) (gethash x nominal-ht)) 
	 		(strip-last (cdr (gethash right-before-episode episodic-ht))))) right-before-fact))))) right-before)
	 )
); END form-episode-pairs

(defun create-name (li)
	; create the final predicate name from a list of names
	(intern (concatenate 'string (format nil "~{~A~^-~}" li) '(#\. #\v)))
)

(defun create-e-fun (cnt)
	; create e{cnt}.f
	(intern (coerce (list #\e (digit-char cnt) #\. #\f) 'string))
)

(setq cnt 1) 

(defun hypothesize4 (epi-wff1 epi-wff2 type-facts)
	; generate hypotheses for each episodic pair
	(let ( antecedent consequent)
		(setf cnt (+ 1 cnt))
		(setq antecedent (list (create-name (mapcar #'name-of-pred (cons (car epi-wff1) (car type-facts)))) '?e))
		(setq consequent (create-name (mapcar #'name-of-pred (cons (car epi-wff2) (second type-facts)))) )
		(list
			;the main "forward" implication between the hyphenated episodic predicates 
			(list antecedent '=> (list consequent (list (create-e-fun cnt) '?e)))
			; the hypothesis predicting the 'Right-before.p' relation for the arguments of the preceding hypothesis
			(list antecedent '=> (list 'Right-before.p '?e (list (create-e-fun cnt) '?e) ))

			; the implication from the first hyphenated episodic predicate to the general one, and 
			; the type of subject, indirect object (if any), object (if any)
			(list antecedent '=> (list (car epi-wff1) '?e))
			(list antecedent '=> (list (car (car type-facts)) '(subj.f ?e)))
			(if (= (length (car type-facts)) 3) (list antecedent '=> (list (second (car type-facts)) '(iobj.f ?e)) ))
			(if (> (length (car type-facts)) 1) (list antecedent '=> (list (car (last (car type-facts))) '(obj.f ?e))))

			; the implication from the second hyphenated episodic predicate to the general one, and 
			; the type of subject, indirect object (if any), object (if any)
			(list (list consequent '?e) '=> (list (car epi-wff2) '?e))
			(list (list consequent '?e) '=> (list (car (second type-facts)) '(subj.f ?e)))
			(if (= (length (second type-facts)) 3) (list (list consequent '?e) '=> (list (second (second type-facts)) '(iobj.f ?e)) ))
			(if (> (length (second type-facts)) 1) (list (list consequent '?e) '=> (list (car (last (second type-facts))) '(obj.f ?e))))

			; the two retrodictive hypotheses
			(list (list consequent '?e) '=> (list (car antecedent) (list (create-e-fun (+ 1 cnt)) '?e)))
			(list (list consequent '?e) '=> (list 'Right-before.p (list (create-e-fun (+ 1 cnt)) '?e)))
		)

	)

); END hypothesize4

(defun hypothesize5 (facts)
	; initialize the index for e{index}.f
	(setf cnt 1)
	(let ((pairs (form-episode-pairs facts)))
		(if (not (listp pairs))
			(return-from hypothesize5 '**ERROR**))
		; remove the duplications
		(remove-duplicates (apply #'append (mapcar (lambda(episodic-pair) 
			(setf cnt (+ 1 cnt)) (hypothesize4 (car episodic-pair) (second episodic-pair) (third episodic-pair)))
			 pairs)) :test #'equal)
	)
)



