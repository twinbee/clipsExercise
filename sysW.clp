; Matthew Bennett 3-29-04
: System W CLIPS assignment
; 3-29-04 AI CSC 412

;System W implements a series of 11 rules so that a 
; proposition can be simplified and evaluated.
; Eventually the expert system will confirm or
; reject proposition. watch out for the gorilla.
;; RULE FIRING SEQUENCE: (0),1,2A,2b,<3>,4,5,(6),(7),(8),(9)
;; next rule to implement is 3 (4-10-04)

(clear)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; INITIALIZATION AREA ;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftemplate proposition
 (slot id)
 (multislot lside)
 (multislot rside)
 (slot rulecount)
 (slot is-taut))

(deftemplate vars
 (slot logod)
 (slot numrules))
;;

(deffacts factlist (proposition
  (id "prop1: A ^ B ^ nB, T ; C v D, F")
  (lside A ^ B ^ nB, T)
  (rside C v D, F)
  (rulecount -1)
  (is-taut FALSE))

 (proposition
  (id "prop2: A ^ B ^ nE, T ; C v D, F")
  (lside A ^ B ^ nE, T)
  (rside C v D, F)
  (rulecount -1)
  (is-taut FALSE))

 (proposition
  (id "prop3: A ^ B ^ nE, F ; C v D, F")
  (lside A ^ B ^ nE, F)
  (rside C v D, F)
  (rulecount -1)
  (is-taut FALSE))

;; "GLOBAL VARS"
 (vars
  (logod 0)
  (numrules 5))
)

;;
(deffunction product
 ($?phrase) (bind ?comma (first$ (create$ , , ))) (bind $?result (create$ )) (if (> (length$ $?phrase) 0) then  (bind ?comp (first$ $?phrase) )   (while (and (neq ?comma ?comp) (> (length$ $?phrase) 0))    (bind ?result $?result ?comp)    (bind ?phrase (rest$ $?phrase))    (bind ?comp (first$ $?phrase))   ) ;end while	)  ;end if $?result) ;return(deffunction product-del ($?phrase) (if (> (length$ $?phrase) 0) then  (bind $?fproduct(product $?phrase))  (bind ?len (length$ $?fproduct))  (bind $?phrase (delete$ $?phrase 1 ?len))  (if (> (length$ $?phrase) 0) then   (bind $?phrase (delete$ $?phrase 1 1) )	) ;end if )  ;end if $?phrase) ;return

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; STARTING CONDITIONS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defrule logo
 (vars
  (logod 0))
=>
 (printout t crlf)
 (printout t "------------------------------------------------------------" crlf)
 (printout t "------------------------- SYSTEM W -------------------------" crlf)
 (printout t "-------------------------(04-10-04)-------------------------" crlf)
)

(defrule logo2
; (vars
;  (logod 0))
=>
 (printout t "------------------------------------------------------------" crlf)
 (printout t "------------------------------------------------------------" crlf)
)

;; RULE FIRING SEQUENCE: (0),1,2A,2b,<3>,4,5,(6),(7),(8),(9)
;; next rule to implement is 3 (4-10-04)

;; RULE FOR CONVERTING TO DISJUNCTIVE NORMAL FORM

;; RULE FOR RECURSIVE NEGATION REMOVAL
;;(HELPER DEFFUN FOR RECURSIVE NEGATION REMOVAL)

;;RULE FOR RECURSIVE AND/OR REMOVAL
;(HELPER DEFFUN FOR RECURSIVE AND/OR REMOVAL)

;; THEOREM SPLITTING

;;STOPPING RULES

(defrule notTaut ?prop <- (proposition
											(id ?id)
           (lside $?l)
           (rside $?r)
           (rulecount 5)
           (is-taut FALSE)) => (modify ?prop (is-taut FALSE) ) (printout t "Propistion " ?id " X=X tautology" crlf)
);;
(defrule isTaut ?prop <- (proposition
											(id ?id)
           (lside $?l)
           (rside $?r)
           (rulecount ?)
           (is-taut TRUE)) => (printout t "Propistion " ?id " --> tautology" crlf ))
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;."`".;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;.-./ _=_ \.-.;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;{  (,(oYo),) }};;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;{{ |   "   |} };;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;{ { \(---)/  }};;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;{{  }'-=-'{ } };;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;{ { }._:_.{  }};;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;{{  } -:- { } };;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;{_{ }`===`{  _};;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;((((\)     (/))));;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; COMPLETING RULES ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;IF WE MATCH A RULE HERE THEN WE ARE DONE;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; RULE W.1
; check for the same element in premise and conclusion
;;(defrule W_1 ?prop <- (proposition
           (lside $?l)
           (rside $?r)
           (rulecount 0)
           (is-taut FALSE)) =>
 (bind ?done FALSE) (bind $?l $?l) ;(printout t "Entering W.1" crlf) ;(printout t "l side:    " $?l crlf) ;(printout t "r side:   " $?r crlf) ;check each l slot against each r slot
  ;if a match is found,  (while (and (> (length $?l) 0) (eq ?done FALSE) ) ;lhs loop (outer)  (bind $?lterm (product $?l) )  (bind $?l (product-del $?l) )  (bind $?r  $?r)
  (while (and (> (length$ $?r) 0) (eq ?done FALSE) )   (bind $?rterm (product $?r) )   (bind $?r (product-del $?r) )   (if (eq $?lterm $?rterm) then    ;(printout t "W.1-Match found!" crlf)    (bind ?done TRUE)   ) ;end if  ) ;end while (rhs) ) ;end while (lhs) (if (eq ?done TRUE) then  ;(printout t "Setting is taut to true" crlf)
  ;; this is a COMPLETING RULE-SEE ABOVE  (modify ?prop (is-taut TRUE) (rulecount 0)) ;endif else	(modify ?prop (rulecount 1)) ) ;(printout t "Exiting W.1" crlf)
);;

; RULE W.2A
(defrule W_2A ?prop <- (proposition
           (lside $?l)
           (rside $?r)
           (rulecount 1) ;2nd
           (is-taut FALSE)) => ;(printout t "Entering W.2A" crlf) ;(printout t "l side:    " $?l crlf) ;(printout t "r side:   " $?r crlf)
 (bind ?done FALSE) (if (member$ F $?l) then	(bind ?done TRUE)) ;endif (if (member$ T $?r) then  (bind ?done TRUE)) ;endif (if ( eq ?done TRUE) then  (modify ?prop (is-taut TRUE) (rulecount 0) ) else (modify ?prop (rulecount 2) ));endif ;(printout t "Exiting W.2A" crlf)
)
;;

; RULE W.3
;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; SIMPLIFICATION RULES ;;;;;;;;;;;;;;;;;;;;;;;;;
;; TRANSFORM THE LOGICAL CONSEQUENCES ;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; RULE W.0
; if the only thing on the left side is T, then 
;  the rhs is the only thing we need to evaluate
(defrule W_0 ?prop <- (proposition
           (lside $?l)
           (rside $?r)
           (rulecount -1) ;1st to eval
           (is-taut FALSE))=>
 ;(printout t "Entering W.0" crlf)
 ;(printout t "l side:    " $?l crlf) ;(printout t "r side:    " $?r crlf) (modify ?prop (rulecount 0))
 ;(printout t "Exiting W.0" crlf)
)


; RULE W.2B
;removes T from l AND F from r (meaningless parts)
;;
(defrule W_2B ?prop <- (proposition
           (lside $?l)
           (rside $?r)
           (rulecount 2) ;3rd to eval
           (is-taut FALSE))	=> ;(printout t "Entering W.2B" crlf)
 ;(printout t "l side:    " $?l crlf) ;(printout t "r side:   " $?r crlf) (bind ?changed FALSE) (bind $?sep (create$ ,)) (bind $?new-l (create$ )) (bind $?new-r (create$ ))  (while (> (length$ $?l) 0) ;for all in l, check if it is T  (bind $?p (product $?l))  (bind $?l (product-del $?l))  (if (and (eq (implode$ $?p) "T" ) (eq (implode$ $?p) "T" )) then  	(bind ?changed TRUE)  else
   (if (> (length$ $?new-l ) 0 ) then    (bind $?new-l $?new-l , $?p)   else	(bind $?new-l $?p)  ) );end if ); end while (while ( > (length$ $?r) 0)
 ;for all in r, check if it is F  (bind $?p (product $?r) )  (bind $?r (product-del $?r) )  (if (and (= (length$ $?p) 1) (eq (implode$ $?p) "F" )) then   (bind ?changed TRUE)  else (if (> (length$ $?new-r ) 0) then
   (bind $?new-r $?new-r , $?p)	else (bind $?new-r $?p))	);end if );end while	 (if (eq ?changed TRUE) then  (modify ?prop (lside $?new-l) (rside $?new-r) (rulecount 0) ) else (modify ?prop (rulecount 3)) ) ;end if 
 ;(printout t "Exiting W.2B..." crlf )
 ;;(printout t ?prop crlf));;

; RULE W.4
(defrule W_4
?prop <- (proposition
          (lside $?l)
          (rside $?r)
          (rulecount 3) ;4th to fire
          (is-taut FALSE)) =>
 (bind $?sep (create$ , )) (bind $?new-l (create$ )) (bind $?new-r (create$ )) (bind ?changed FALSE) ;(printout t "Entering W.4" crlf) ;(printout t "l side:    " $?l crlf) ;(printout t "r side:   " $?r crlf)
 (while (> (length$ $?l) 0) ;for all on the l
 ;;move nots r
  (bind $?p (product $?l))  (bind $?l (product-del $?l))  (bind ?s (implode$ $?p) )  ;(printout t "p is " $?p crlf)  (if (eq (str-index "n" ?s) 1) then   (bind $?p (explode$ (sub-string 2 (str-length ?s) ?s )))   (bind $?r $?r , $?p)   (bind ?changed TRUE)  else (if (> (length$ $?new-l) 0) then   (bind $?new-l $?new-l , $?p)  else   (bind $?new-l $?p) );endif	);endif );endwhile (while (> (length$ $?r) 0) ;for all on the r
 ;;move nots l
  (bind $?p (product $?r) )  (bind $?r (product-del $?r) )  (bind ?s (implode$ $?p))  (if (eq (str-index "n" ?s) 1) then   (bind $?p (explode$ (sub-string 2 (str-length ?s) ?s)) )    (bind $?new-l $new-l , $?p)   (bind ?changed TRUE)  else (if (> (length$ $?new-r ) 0) then   (bind $?new-r $?new-r , $?p)  else (bind $?new-r $?p));endif  );endif );end while (if (eq ?changed TRUE) then  (modify ?prop (lside $?new-l) (rside $?new-r) (rulecount 0) ) else (modify ?prop (rulecount 4)) ) ;end if ;(printout t "Exiting W.4" crlf)
)
;;

; RULE W.5
(defrule W_5 ?prop <- (proposition
           (lside $?l)
           (rside $?r)
           (rulecount 4)
           (is-taut FALSE)) => (bind $?sep (create$ ,) ) (bind $?new-l (create$ ) ) (bind $?new-r (create$ ) ) (bind ?changed FALSE) ;(printout t "Entering W.5" crlf)
 (while (> (length$ $?l) 0) ; for all on the l side			 ;; change ^ to ,
  (bind $?p (product $?l) )  (bind $?l (product-del $?l) )  (bind ?s (implode$ $?p) )  (if (member$ ^ $?p) then
   (bind $?p (replace-member$ $?p (create$ , ) (create$ ^)))   (bind ?changed TRUE) ); endif  (if (> (length$ $?new-l ) 0) then   (bind $?new-l $?new-l , $?p)
  else (bind $?new-l $?p) ) ; end if );end while (while (> (length$ $?r) 0 ) ; for all on the r
 ;; change v to ,			  (bind $?p (product $?r) )  (bind $?r (product-del $?r) )  (bind ?s (implode$ $?p) )  (if (member$ v $?p) then
   (bind $?p (replace-member$ $?p (create$ , ) (create$ v)))   (bind ?changed TRUE)	) ; end if	(if (> (length$ $?new-r ) 0) then
   (bind $?new-r $?new-r , $?p)  else (bind $?new-r $?p) );end if );end while	 (if (eq ?changed TRUE) then    (modify ?prop (lside $?new-l) (rside $?new-r) (rulecount 0)) else	(modify ?prop (rulecount 5) ) );end if ;(printout t "Exiting W.5" crlf )		)

; RULE W.6

; RULE W.7

; RULE W.8

; RULE W.9

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(reset)
(run)
