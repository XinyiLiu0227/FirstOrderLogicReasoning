(load "XLiu_src.lisp")
(form-episode-pairs  '((Get-up.v lena1 E1) (Sleep.v lena1 E2) (creature.n lena1) (Right-before.p E2 E1)))
;output:(((SLEEP.V LENA1 E2) (GET-UP.V LENA1 E1) ((CREATURE.N) (CREATURE.N))))
(hypothesize5 '((Get-up.v lena1 E1) (Sleep.v lena1 E2) (creature.n lena1) (Right-before.p E2 E1)))
;output: (((|SLEEP-CREATURE.v| ?E) => (|GET-UP-CREATURE.v| (|e3.f| ?E)))
		 ; ((|SLEEP-CREATURE.v| ?E) => (RIGHT-BEFORE.P ?E (|e3.f| ?E)))
		 ; ((|SLEEP-CREATURE.v| ?E) => (SLEEP.V ?E))
		 ; ((|SLEEP-CREATURE.v| ?E) => (CREATURE.N (SUBJ.F ?E)))
		 ; ((|GET-UP-CREATURE.v| ?E) => (GET-UP.V ?E))
		 ; ((|GET-UP-CREATURE.v| ?E) => (CREATURE.N (SUBJ.F ?E))) NIL
		 ; ((|GET-UP-CREATURE.v| ?E) => (|SLEEP-CREATURE.v| (|e4.f| ?E)))
		 ; ((|GET-UP-CREATURE.v| ?E) => (RIGHT-BEFORE.P (|e4.f| ?E))))

(form-episode-pairs '((Get-up.v lena1 E1) (Sleep.v lena1 E2) (creature.n lena1) (human.n lena1) (Right-before.p E2 E1)))
; output: The list of facts does not satisfy the given requirements.
; **ERROR** (since there is more than one monadic nominal predicate for constant lena1)
(hypothesize5 '((Get-up.v lena1 E1) (Sleep.v lena1 E2) (creature.n lena1) (human.n lena1) (Right-before.p E2 E1)))
; output: The list of facts does not satisfy the given requirements.
; **ERROR** (since there is more than one monadic nominal predicate for constant lena1)


(form-episode-pairs '((Get-up.v lena1 E1) (Sleep.v lena1 bed2 E2) (human.n lena1) (Right-before.p E2 E1)))
; The list of facts does not satisfy the given requirements.
; **ERROR** (as the bed2 does not have type predicate)
(hypothesize5 '((Get-up.v lena1 E1) (Sleep.v lena1 bed2 E2) (human.n lena1) (Right-before.p E2 E1)))
; The list of facts does not satisfy the given requirements.
; **ERROR** (as the bed2 does not have type predicate)

(form-episode-pairs '((Get-up.v lena1 E1) (Sleep.v lena1 E2) (human.n lena1) (wash-face.v lena1 e3) (Right-before.p e2 e1) (Right-before.p e1 e3)))
; (((SLEEP.V LENA1 E2) (GET-UP.V LENA1 E1) ((HUMAN.N) (HUMAN.N)))
;  ((GET-UP.V LENA1 E1) (WASH-FACE.V LENA1 E3) ((HUMAN.N) (HUMAN.N))))
(hypothesize5 '((Get-up.v lena1 E1) (Sleep.v lena1 E2) (human.n lena1) (wash-face.v lena1 e3) (Right-before.p e2 e1) (Right-before.p e1 e3)))
; (((|SLEEP-HUMAN.v| ?E) => (|GET-UP-HUMAN.v| (|e3.f| ?E)))
;  ((|SLEEP-HUMAN.v| ?E) => (RIGHT-BEFORE.P ?E (|e3.f| ?E)))
;  ((|SLEEP-HUMAN.v| ?E) => (SLEEP.V ?E))
;  ((|SLEEP-HUMAN.v| ?E) => (HUMAN.N (SUBJ.F ?E)))
;  ((|GET-UP-HUMAN.v| ?E) => (|SLEEP-HUMAN.v| (|e4.f| ?E)))
;  ((|GET-UP-HUMAN.v| ?E) => (RIGHT-BEFORE.P (|e4.f| ?E)))
;  ((|GET-UP-HUMAN.v| ?E) => (|WASH-FACE-HUMAN.v| (|e5.f| ?E)))
;  ((|GET-UP-HUMAN.v| ?E) => (RIGHT-BEFORE.P ?E (|e5.f| ?E)))
;  ((|GET-UP-HUMAN.v| ?E) => (GET-UP.V ?E))
;  ((|GET-UP-HUMAN.v| ?E) => (HUMAN.N (SUBJ.F ?E)))
;  ((|WASH-FACE-HUMAN.v| ?E) => (WASH-FACE.V ?E))
;  ((|WASH-FACE-HUMAN.v| ?E) => (HUMAN.N (SUBJ.F ?E))) NIL
;  ((|WASH-FACE-HUMAN.v| ?E) => (|GET-UP-HUMAN.v| (|e6.f| ?E)))
;  ((|WASH-FACE-HUMAN.v| ?E) => (RIGHT-BEFORE.P (|e6.f| ?E))))
