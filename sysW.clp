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

(deffacts factlist
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
 ($?phrase)

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

(defrule notTaut
											(id ?id)
           (lside $?l)
           (rside $?r)
           (rulecount 5)
           (is-taut FALSE))
)

											(id ?id)
           (lside $?l)
           (rside $?r)
           (rulecount ?)
           (is-taut TRUE))
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
;;
           (lside $?l)
           (rside $?r)
           (rulecount 0)
           (is-taut FALSE))
 (bind ?done FALSE)
  ;if a match is found, 
  (while (and (> (length$ $?r) 0) (eq ?done FALSE) )
  ;; this is a COMPLETING RULE-SEE ABOVE
)

; RULE W.2A
(defrule W_2A
           (lside $?l)
           (rside $?r)
           (rulecount 1) ;2nd
           (is-taut FALSE))
 (bind ?done FALSE)
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
(defrule W_0
           (lside $?l)
           (rside $?r)
           (rulecount -1) ;1st to eval
           (is-taut FALSE))
 ;(printout t "Entering W.0" crlf)
 ;(printout t "l side:    " $?l crlf)
 ;(printout t "Exiting W.0" crlf)
)


; RULE W.2B
;removes T from l AND F from r (meaningless parts)
;;
(defrule W_2B
           (lside $?l)
           (rside $?r)
           (rulecount 2) ;3rd to eval
           (is-taut FALSE))
 ;(printout t "l side:    " $?l crlf)
   (if (> (length$ $?new-l ) 0 ) then
 ;for all in r, check if it is F
   (bind $?new-r $?new-r , $?p)
 ;(printout t "Exiting W.2B..." crlf )
 ;;(printout t ?prop crlf)

; RULE W.4
(defrule W_4
?prop <- (proposition
          (lside $?l)
          (rside $?r)
          (rulecount 3) ;4th to fire
          (is-taut FALSE))
 (bind $?sep (create$ , ))

 ;;move nots r
  (bind $?p (product $?l))
 ;;move nots l
  (bind $?p (product $?r) )
)
;;

; RULE W.5
(defrule W_5
           (lside $?l)
           (rside $?r)
           (rulecount 4)
           (is-taut FALSE))
 (while (> (length$ $?l) 0)
  (bind $?p (product $?l) )
   (bind $?p (replace-member$ $?p (create$ , ) (create$ ^)))
  else (bind $?new-l $?p) ) ; end if
 ;; change v to ,			
   (bind $?p (replace-member$ $?p (create$ , ) (create$ v)))
   (bind $?new-r $?new-r , $?p)

; RULE W.6

; RULE W.7

; RULE W.8

; RULE W.9

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(reset)
(run)