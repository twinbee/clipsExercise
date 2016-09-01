;;;======================================================
;;;   Professor Identification Expert System
;;;
;;; This is an expert system for computer science
;;;  students who forget the name of their professors
;;;  whenever they forget due to thinking about
;;;  so many AI problems.
;;;======================================================
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
 (goal is type.prof)
 (legalanswers are yes no)

 (rule (if accent is yes) 
  (then origin is foreign))
 (rule (if accent is no) 
  (then origin is domestic))
 (question accent is "Does your professor have a funny accent?")

 (rule (if origin is foreign and hw is yes)
  (then homework is heavy))
 (rule (if origin is foreign and hw is no)
  (then homework is light))
 (question hw is "Does your professor give a lot of homework?")

 (rule (if homework is heavy and british is yes)
  (then type.prof is "Cliff Burgess"))
 (rule (if homework is heavy and british is no)
  (then country is notuk))
 (question british is "Is your professor British?")

 (rule (if homework is light and eb is yes)
  (then type.prof is "Adel Ali"))
 (rule (if homework is light and eb is no)
  (then brother is notevil))
 (question eb is "Does your professor have an evil brother?")

 (rule (if brother is notevil and pepper is yes)
  (then type.prof is "Khaled El-Sawi"))
 (rule (if brother is notevil and pepper is no)
  (then type.prof is "Joe Zhang"))
 (question pepper is "Does your professor have an aversion to Dr. Pepper?")

 (rule (if country is notuk and gorillas is yes)
  (then type.prof is "Dia Ali"))
 (rule (if country is notuk and gorillas is no)
  (then type.prof is "Bedduh Murali"))
 (question gorillas is "Does your professor have an abnormal interest in gorillas?")

 (rule (if origin is domestic and excitable is yes)
  (then personality is upbeat))
 (rule (if origin is domestic and excitable is no)
  (then personality is casual))
 (question excitable is "Is your professor exciteable?")

 (rule (if personality is upbeat and coffee is yes)
  (then type.prof is "Brady Rimes"))
 (rule (if personality is upbeat and coffee is no)
  (then type.prof is "Frank Nagurney"))
 (question coffee is "Does your professor drink a lot of coffee?")

 (rule (if personality is casual and cpp is yes)
  (then plang is cplusplus))
 (rule (if personality is casual and cpp is no)
  (then plang is other))
 (question cpp is "Does your professor teach classes primarily in C/C++?")

 (rule (if plang is cplusplus and female is yes)
  (then sex is f))
 (rule (if plang is cplusplus and female is no)
  (then sex is m))
 (question female is "Is your professor a woman?")

 (rule (if plang is other and ada is yes)
  (then type.prof is "Ralph Bisland"))
 (rule (if plang is other and ada is no)
  (then ada-p is false))
 (question ada is "Does your professor loo`ove Ada?")

 (rule (if sex is m and nix is yes)
  (then type.prof is "Ray Seyfarth"))
 (rule (if sex is m and nix is no)
  (then type.prof is "Hugh Garraway"))
 (question nix is "Is your professor a UNIX fanatic?")

 (rule (if sex is f and coast is yes)
  (then type.prof is "Louise Perkins"))
 (rule (if sex is f and coast is no)
  (then type.prof is "Mary Dayne Gregg"))
 (question coast is "Does she work on the Gulf Park Campus?")

 (rule (if ada-p is false and redpencil is yes)
  (then type.prof is "Nancy Howell"))
 (rule (if ada-p is false and redpencil is no)
  (then type.prof is "Maria Cobb"))
 (question redpencil is "Does your professor suggest bringing a red-banded pencil to tests?")

 (answer is "I think your professor is " type.prof)
)

(reset)
(run)

