;;; Matthew Bennett
;;;  4-28-04
;;;  CSC 412 Ali
;;;  CLIPS Assignment #4 - the Smart Server Threat "Matrix"

;; based upon the professor identification system code
 ;; asks a bunch of questions about the smart server
 ;; in its native habitat and gives some basic information
 ;; about how it should act

;; the original basis for the professor identification
 ;; system came from the animal identification system
 ;; which is available on the CLIPS webpage under
  ;; examples/animal.clp
 ;;I think this is kind of a silly assignment
  ;;so why is it associated with my name??
  ;; oh, well I will do it anyway!

(clear)
;;;***************************
;;;* DEFTEMPLATE DEFINITIONS *
;;;***************************

(deftemplate rule 
   (multislot if)
   (multislot then))

;;;**************************
;;;* INFERENCE ENGINE RULES *
;;;**************************

(defrule propagate-goal ""
   (goal is ?goal)
   (rule (if ?variable $?)
         (then ?goal ? ?value))
   =>
   (assert (goal is ?variable)))

(defrule goal-satified ""
   (declare (salience 30))
   ?f <- (goal is ?goal)
   (variable ?goal ?value)
   (answer ? ?text ?goal)
   =>
   (retract ?f)
   (format t "%s%s%n" ?text ?value))

(defrule remove-rule-no-match ""
   (declare (salience 20))
   (variable ?variable ?value)
   ?f <- (rule (if ?variable ? ~?value $?))
   =>
   (retract ?f))

(defrule modify-rule-match ""
   (declare (salience 20))
   (variable ?variable ?value)
   ?f <- (rule (if ?variable ? ?value and $?rest))
   =>
   (modify ?f (if ?rest)))

(defrule rule-satisfied ""
   (declare (salience 20))
   (variable ?variable ?value)
   ?f <- (rule (if ?variable ? ?value)
               (then ?goal ? ?goal-value))
   =>
   (retract ?f)
   (assert (variable ?goal ?goal-value)))

(defrule ask-question-no-legalvalues ""
   (declare (salience 10))
   (not (legalanswers $?))
   ?f1 <- (goal is ?variable)
   ?f2 <- (question ?variable ? ?text)
   =>
   (retract ?f1 ?f2)
   (format t "%s " ?text)
   (assert (variable ?variable (read))))

(defrule ask-question-legalvalues ""
   (declare (salience 10))
   (legalanswers ? $?answers)
   ?f1 <- (goal is ?variable)
   ?f2 <- (question ?variable ? ?text)
   =>
   (retract ?f1)
   (format t "%s " ?text)
   (printout t ?answers " ")
   (bind ?reply (read))
   (if (member (lowcase ?reply) ?answers) 
     then (assert (variable ?variable ?reply))
          (retract ?f2)
     else (assert (goal is ?variable))))

;;;***************************
;;;* DEFFACTS KNOWLEDGE BASE *
;;;***************************

(deffacts knowledge-base 
 (goal is summary)
 (legalanswers are yes no)

 (rule (if gotroot is yes) 
  (then clients is unlimited))
 (rule (if gotroot is yes) 
  (then ports is low))
 (rule (if gotroot is no) 
  (then clients is limited))
 (rule (if gotroot is no) 
  (then ports is high))
 (question gotroot is "Do you run Googler_Server as root?")

 (rule (if clients is unlimited and cpuloaded is yes)
  (then clients is limited))
 (question cpuloaded is "Is the CPU loaded greater than 50%? (This will limit the # of clients)")

 (rule (if clients is unlimited and memloaded is yes)
  (then clients is limited))
 (question memloaded is "Is the memory loaded greater than 75%? (This will limit the # of clients)") 

 (rule (if clients is unlimited and netloaded is yes)
  (then clients is limited))
 (question netloaded is "Is the network loaded greater than 90% of 10 Mbps ethernet? (This will limit the # of clients)")  

 (rule (if clients is limited and clientloaded is yes)
  (then clientslots is full))
 (rule (if clients is limited and clientloaded is no)
  (then clientslots is notfull))
 (question clientloaded is "Are there 10 clients connected? (Server full)")  

 (rule (if clientslots is full and idlers is yes)
  (then killidlers is true))
 (rule (if clientslots is full and idlers is no)
  (then killidlers is playgames))
 (question idlers is "Are there more than 5 clients idle? (Kill them)")

 (rule (if get2google is yes)
  (then works is true))
 (rule (if get2google is no)
  (then works is false))
 (question get2google is "Can the network get to Google?") 

 (rule (if java is yes)
  (then C4Applet is supported))
 (rule (if java is no)
  (then C4Applet is unsupported))
 (question java is "Do the client browsers support Java2?") 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 (rule (if clients is unlimited and ports is low and clients is unlimited and works is true and C4Applet is supported)
  (then summary is
 "The web server will work on port 80 and has no forseeable practical (only hardware restricted) limits on clients. Google searches are automatic, and the user can be entertained by Connect4 while he/she waits for the search to complete. All is well!"))

 (rule (if clients is nlimited and ports is low and clients is limited and works is true and killIdlers is playgames and C4Applet is supported)
  (then summary is
 "The web server will work on port 80 and has a 10 client limit. Google searches are automatic, and the user can be entertained by Connect4 while he/she waits for the search to complete. All is well!"))

 (rule (if clients is limited and ports is low and clients is limited and works is true and killIdlers is true)
  (then summary is
 "The web server will work on port 80 and has a 10 client limit. Google searches are automatic, and the user will not be entertained by Connect4 to encourage them to die instead of idleing!"))

 (rule (if ports is high and clients is limited and works is true and killidlers is playgames and C4Applet is supported)
  (then summary is
 "The web server will work on port 16000 and has a 10 client limit. Google searches are automatic, and the user can be entertained by Connect4 while he/she waits for the search to complete. All is well!"))

 (rule (if ports is high and clients is limited and works is true and killidlers is true)
  (then summary is
 "The web server will work on port 16000 and has a 10 client limit, but some clients will have to be killed for idling too long. Google searches are automatic, and the users will not be entertained by Connect4 to encourage them to die instead of idleing!"))

 (rule (if works is false)
  (then summary is
 "There is no reason to emply the Googling server if you do not have access to Google from your network!! Go get apache instead. It is much better!"))

 (answer is "Results of analysis: " summary) 
)

(reset)
(run)

