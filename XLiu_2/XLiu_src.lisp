;;; use the answer code as code base for the programming tasks
(defun valid-facts-p (facts)
; `````````````````````````````
; Checks whether the given list of facts satisfies the requirements of the 'hypothesize' function.
;
  (and (listp facts)
    (let* ((binary (remove-if-not #'binary-p facts)) (monadic (remove-if-not #'monadic-p facts))
            (binary-set (remove-duplicates (apply #'append (mapcar #'cdr binary)))))
      (and
        (null (set-difference facts (append binary monadic) :test #'equal)) ; every fact is monadic or binary
        ;; relax the requirement that every binary fact has same first argument
        ; (every (lambda (fact) (equal (second fact) subj)) binary)
        ;; just ignore monadic fact that does not have argument from binary set
        ; (every (lambda (fact) (member (second fact) binary-set)) monadic) 
        (every (lambda (const) (find-if (lambda (fact) ; every constant in binary-set occurs in monadic type predicate
          (and (nominal-pred-p fact) (equal (second fact) const))) monadic)) binary-set))))
) ; END valid-facts-p

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

(defun constant-p (x)
  (and (symbolp x) (let ((chs (explode x)))
    (and (not (find #\. chs)) (digit-char-p (car (last chs)))))))


(defun type-of-pred (pred)
;```````````````````````````
; Gets the extension of a predicate (v, n, p, a, or f), or returns nil if not a pred.
;
  (car (remove-if-not (lambda (c) (member c '(v n p a f)))
    (mapcar #'intern (mapcar #'string (last (member #\. (explode pred)))))))
) ; END type-of-pred



(defun nominal-pred-to-function (li)
; Converts a nominal predicate (e.g., tail.n) to a functional term (e.g., tail.f).
  (let ((pred (first li)) (cnt (second li)))
    ; only add number to the function when the count of the function is more than 1
    (cond ((and (equal (type-of-pred pred) 'n) (> cnt 1))
      (implode (append (butlast (butlast (explode pred))) (list (digit-char cnt)) '(#\.) '(#\F))))
      ((equal (type-of-pred pred) 'n)
        (implode (append (butlast (explode pred)) '(#\F)))))
)) ; END nominal-pred-to-function



(defun explode (s)
; Convert s to list of characters.
  (coerce (string (if (numberp s) (write-to-string s) s)) 'list)
) ; END explode

(defun implode (l)
; Convert list of characters to symbol.
  (intern (coerce l 'string))
) ; END implode

(defun create-key (fact C nominal-ht)
  ;create a string key for counter hash table, which is concatenated by the predication of the binary fact, the position of
  ;the constant and the constant's type
  (if (equal (second fact) C)
    (concatenate 'string (string (first fact)) (write-to-string 2) (string (gethash (nth 2 fact) nominal-ht)))
  (concatenate 'string (string (first fact)) (write-to-string 1) (string (gethash (second fact) nominal-ht))))
)


(defun hypothesize2 (li)
;````````````````````````````
; Generate hypotheses from facts about certain subject. 
;
; facts: a list of monadic or binary predications over constant arguments:
;   * the monadic predications consist of exactly one nominal predication ('.n')
;     about each constant occurring in the binary predications, plus possibly
;     additional non-nominal ('.a', '.v', or '.p') predications about them.
;   * individual constants must have numeric suffixes.
;   * each constant in binary facts has exactly one monadic with type-predicate

  ; set certainty (0.1), and make HTs for keeping track of nominal types and count of each nominal types
  (let* ((C (car li)) (facts (cdr li)) (cert 0.1) (counter (make-hash-table :test #'equal)) (func (make-hash-table))
        (nominal-ht (make-hash-table)) (binary-set  (remove-duplicates (apply #'append (mapcar #'(lambda(fact) (if (binary-p fact) (cdr fact))) facts)))) 
        antecedent consequent)
    (print facts)
    (when (not (valid-facts-p facts))
      (format t "The list of facts does not satisfy the given requirements.~%")
      (return-from hypothesize2 '**ERROR**))
    ; For each nominal fact, add the type predicate (e.g. dog.n) to hash table with constant (e.g. Dog1) as key
    (mapcar (lambda (nominal-fact) (setf (gethash (second nominal-fact) nominal-ht) (first nominal-fact)))
      (remove-if-not #'nominal-pred-p facts))
    ; if for the same P, there are two binary facts (P subj B), (P subj C) where B and C are of same type, e.g. car.n
    ; then we have the key 'P2car.n' in counter with a value of 2
    (mapcar (lambda(fact) (setq cnt (+ 1 (or (gethash (create-key fact C nominal-ht) counter) 0)))
      (setf (gethash (create-key fact C nominal-ht) counter) cnt)
      (setf target (first (remove-if (lambda(x) (equal x C)) (cdr fact))))
      (setf (gethash target func) (list (gethash target nominal-ht) cnt))) 
      (remove-if-not #'binary-p facts))

    ; Set antecedent as the type predicate for C followed by ?x, and delete that predication from facts
    (setq antecedent (list (gethash C nominal-ht) '?x))
    (setq facts (append '(1) (remove (list (gethash C nominal-ht) C) facts :test #'equal)))
    ; Iterate over each remaining fact in facts
    (mapcar (lambda (fact)
        ; Create consequent
        (if (equal fact 1) C
          (progn
            (setq consequent (cons (car fact) (mapcar (lambda (const)
              ; Replace each constant with ?x (if C) or a function on ?x derived from the nominal type
              (if (equal const C)
                '?x
                (list (nominal-pred-to-function (gethash const func)) '?x)))
            (cdr fact))))
          ; Construct formula
          `((,antecedent => ,consequent) ,cert)
          )))
      facts)

)) ; END hypothesize2

(defun extract-related-subsets (facts)
  ; ````````````````````````````````````
  ; generate subsets of facts given a fixed subject
  (let* ((binary (remove-if-not #'binary-p facts))
    ; subject candidates occur at least once as first argument of a binary predication
    (cand  (remove-duplicates (mapcar #'second binary)))
    (nominal-pred (mapcar #'second (remove-if-not #'nominal-pred-p facts))) 
    (monadic (remove-if-not #'monadic-p facts))
    )
    ; check there are not more than one type-predicate for each constant
    (when (not (equal (length nominal-pred) (length (remove-duplicates nominal-pred))))
      (format t "more than one type-predicate for one constant")
      (return-from extract-related-subsets '**ERROR**))
    ; check if there exists type-predicate fact and binary facts
    (when (or (null binary) (null nominal-pred))
      (return-from extract-related-subsets '**ERROR**))
    (remove-if-not (lambda(set) (> (length set) 1))
      (mapcar (lambda(x)
      ;binary facts need to contain the subject and each constant has exactly one type-predicate
      (setq cand-binary (remove nil (mapcar (lambda(fact) 
        (if (and (member x fact) (and (member (second fact) nominal-pred) (member (nth 2 fact) nominal-pred))) fact)) binary)
      ))
      (setq cand-binary-set (remove-duplicates (apply #'append (mapcar #'cdr cand-binary))))
      ;every monadic is about one of the arguments occurring in the binary relations
      (setq cand-monadic (remove nil (mapcar (lambda(fact) (if (member (second fact) cand-binary-set) fact)) monadic)))
      (append (list x) cand-binary cand-monadic))
      cand))

)); END extract-related-subsets

(defun hypothesize3 (facts)
  (let ((related-subsets (extract-related-subsets facts)))
    (mapcar #'hypothesize2 related-subsets)
)); END hypothesize 3
