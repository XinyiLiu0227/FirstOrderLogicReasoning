(load "XLiu_src.lisp")
(extract-related-subsets '((person.n Jane1) (Owns.v Jane1 book1) (book.n book1) (book.n book2) (owns.v Jane1 book2)))
;output: ((JANE1 (OWNS.V JANE1 BOOK1) (OWNS.V JANE1 BOOK2) (PERSON.N JANE1) (BOOK.N BOOK1) (BOOK.N BOOK2)))
(hypothesize3 '((person.n Jane1) (Owns.v Jane1 book1) (book.n book1) (book.n book2) (owns.v Jane1 book2)))
;output contains only one list of hypotheses: ((JANE1 (((PERSON.N ?X) => (OWNS.V ?X (BOOK.F ?X))) 0.1) (((PERSON.N ?X) => (OWNS.V ?X (BOOK2.F ?X))) 0.1)(((PERSON.N ?X) => (BOOK.N (BOOK.F ?X))) 0.1)
;(((PERSON.N ?X) => (BOOK.N (BOOK2.F ?X))) 0.1)))


(extract-related-subsets '((person.n Jane1) (person.n Ann2) (Owns.v Ann2 book1) (book.n book1) (book.n book2) (owns.v Jane1 book2)))
;output: ((ANN2 (OWNS.V ANN2 BOOK1) (PERSON.N ANN2) (BOOK.N BOOK1)) (JANE1 (OWNS.V JANE1 BOOK2) (PERSON.N JANE1) (BOOK.N BOOK2)))
(hypothesize3 '((person.n Jane1) (person.n Ann2) (Owns.v Ann2 book1) (book.n book1) (book.n book2) (owns.v Jane1 book2)))
;output is a list of two lists of hypotheses: ((ANN2 (((PERSON.N ?X) => (OWNS.V ?X (BOOK.F ?X))) 0.1) (((PERSON.N ?X) => (BOOK.N (BOOK.F ?X))) 0.1))
; (JANE1 (((PERSON.N ?X) => (OWNS.V ?X (BOOK.F ?X))) 0.1) (((PERSON.N ?X) => (BOOK.N (BOOK.F ?X))) 0.1)))


(extract-related-subsets '((mother.n Mary1)(daughter.n Juliet1)(Love.v Mary1 Juliet1)(Love.v Juliet1 Mary1)))
;output: ((MARY1 (LOVE.V MARY1 JULIET1) (LOVE.V JULIET1 MARY1) (MOTHER.N MARY1) (DAUGHTER.N JULIET1))
; (JULIET1 (LOVE.V MARY1 JULIET1) (LOVE.V JULIET1 MARY1) (MOTHER.N MARY1) (DAUGHTER.N JULIET1)))
(hypothesize3 '((mother.n Mary1)(daughter.n Juliet1)(Love.v Mary1 Juliet1)(Love.v Juliet1 Mary1)))
;output contains two lists of hypotheses: 
 ; ((MARY1 (((MOTHER.N ?X) => (LOVE.V ?X (DAUGHTER.F ?X))) 0.1)
 ;  (((MOTHER.N ?X) => (LOVE.V (DAUGHTER.F ?X) ?X)) 0.1)
 ;  (((MOTHER.N ?X) => (DAUGHTER.N (DAUGHTER.F ?X))) 0.1))
 ; (JULIET1 (((DAUGHTER.N ?X) => (LOVE.V (MOTHER.F ?X) ?X)) 0.1)
 ;  (((DAUGHTER.N ?X) => (LOVE.V ?X (MOTHER.F ?X))) 0.1)
 ;  (((DAUGHTER.N ?X) => (MOTHER.N (MOTHER.F ?X))) 0.1)))


 (extract-related-subsets '((mother.n Mary1) (dog.n Rob2) (daughter.n Juliet1)(Love.v Mary1 Juliet1)(Love.v Juliet1 Mary1)))
 ; The same output as previous example, just ignore the type predicate of constants that are not in any binary facts
 (hypothesize3 '((mother.n Mary1) (dog.n Rob2) (daughter.n Juliet1)(Love.v Mary1 Juliet1)(Love.v Juliet1 Mary1)))
 ; again the same output as before


 (extract-related-subsets '((daughter.n Juliet1)(Love.v Mary1 Juliet1)(Love.v Juliet1 Mary1)))
 ; output is nil as there is no subject have a valid subsets of facts satisifying the requirements
 (hypothesize3 '((daughter.n Juliet1)(Love.v Mary1 Juliet1)(Love.v Juliet1 Mary1)))
 ; output is also nil


 (extract-related-subsets '((mother.n Mary1) (person.n Mary1) (daughter.n Juliet1)(Love.v Mary1 Juliet1)(Love.v Juliet1 Mary1)))
 ;output: report the error that more than one type-predicate for one constan and exit
 (hypothesize3 '((mother.n Mary1) (person.n Mary1) (daughter.n Juliet1)(Love.v Mary1 Juliet1)(Love.v Juliet1 Mary1)))
 ;output: also report the error and exist

