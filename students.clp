;;; Matthew Bennett
;;;  3-26-04 (yes i started early)
;;;  CSC 412 Ali
;;;  CLIPS Assignment #2 - Student-Teacher problem

;;; For the students use:
;;;  Use Multislot for the name to include First and Last name
;;;  Use Multislot to list the classes they are taking
;;;  Add information whether he/she have taken any of the classes before.
;;;  Just use a slot that acts like a flag to tell whether any of the classes
 ;;;  that the student take is a repeat. DON’T USE MULTISLOT!!!

;;; For the teacher:
;;;  Use Multislot for the name to contain first and last name.
;;;  Use Multislot for the list of classes they are teaching 

(clear)

;;; Function that returns the first item from a multislot variable
(deffunction product ($?phrase) ;return the first product of the phrase
      (bind ?comma (first$ (create$ , ,)))
      (bind $?result x x) ; just to bind the variable with something to start
      (bind ?result (delete-member$ $?result $?result))
      (if (> (length$ $?phrase) 0) then
            (bind ?comp (first$ $?phrase))
            (while (and (neq ?comma ?comp) (> (length$ $?phrase) 0))
                  (bind ?result $?result ?comp)
                  (bind ?phrase (rest$ $?phrase))
                  (bind ?comp (first$ $?phrase))
            )
      )
      $?result
)

;;; Function that delete the first item from a multislot variable
(deffunction product-del ($?phrase) ; delete the first product of the phrase
      (if (> (length$ $?phrase) 0) then
            (bind $?fproduct(product $?phrase))
            (bind ?len (length$ $?fproduct))
            ;(printout t "length: " ?len "   phrase: " ?phrase crlf)
            (bind $?phrase (delete$ $?phrase 1 ?len))
            (if (> (length$ $?phrase) 0) then
                  (bind $?phrase (delete$ $?phrase 1 1))
            )
      )
      $?phrase
)

(deffunction hasDiaClasses ($?classes) (if
  (or
   (member$ csc412 $?classes)   (member$ csc415 $?classes)   (member$ csc512 $?classes)   (member$ csc620 $?classes)
  ) then (bind ?val TRUE)
	else (bind ?val FALSE))?val)(deffunction isRimesClass (?class) (or (eq csc307 ?class) (eq csc317 ?class) (eq csc408 ?class)))(deffunction isAdelClass (?class) (or (eq csc626 ?class) (eq csc330 ?class)))(deffunction DiaGrade  (?history) (if (eq ?history repeat)  then (bind ?val A)  else (bind ?val B))?val)(deffunction RimesGrade  (?history) (if (eq ?history repeat)  then (bind ?val B)  else (bind ?val C))?val)(deffunction AdelGrade  (?history) (if (eq ?history repeat)  then (bind ?val C)  else (bind ?val A) )
?val)

;;;---------------------------------------------------------------------------------

(deftemplate student
	(multislot name)
	(slot id)
	(multislot class)
	(slot status)
  (slot history))

(deftemplate teacher 
	(multislot name)
	(multislot class))

(deffacts factlist
	(teacher (name Brady Rimes)
		(class csc307 csc317 csc408))

	(teacher (name Dia Ali)
		(class csc412 csc415 csc512 csc620))

	(teacher (name Adel Ali)
		(class csc330 csc626))

	(student (name John Doe ) 
		(id 100)
		(class csc412 csc317)
		(status sr)
    (history repeat))

	(student (name Paulus Wahjudi ) 
		(id 100000)
		(class csc512 csc626)
		(status grad)
    (history repeat))

	(student (name Kevin Vaughn )
		(id 2)
		(class csc512 csc626 csc408)
		(status adjunct)
    (history repeat))

	(student (name Poor Freshman ) 
		(id 395892949)
		(class csc317 csc307 csc408)
		(status fr)
    (history repeat))

	(student (name Mr Testcase ) 
		(id 8675309)
		(class csc307 csc330 csc408)
		(status jr)
    (history first))

;;; NOTE ON EVIL GENIUSES ;;;;;;;;;;;;;;;;
;; Evil geniuses never repeat classes! ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	(student (name Evil Mr Testcase ) 
		(id 867309)
		(class csc307 csc330 csc408)
		(status jr)
    (history first))

	(student (name Evil John Doe ) 
		(id 10)
		(class csc412 csc317)
		(status sr)
    (history first))

	(student (name Evil Paulus Wahjudi ) 
		(id 10000)
		(class csc512 csc626)
		(status grad)
    (history first))

	(student (name Evil Kevin Vaughn )
		(id 3)
		(class csc512 csc626 csc408)
		(status adjunct)
    (history first))

	(student (name Evil Poor Freshman ) 
		(id 39589299)
		(class csc317 csc307 csc408)
		(status fr)
    (history first))
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;(defrule grade ; match on all students ?student<-(student (name $?name) (class $?class) (history ?param) )=> (if (hasDiaClasses $?class) ;Dia Ali is so powerful that he can overwrite any grades   then  (loop-for-count (?i 1 (length$ $?class))  do ; for all the classes student is tking   (printout t $?name " in " (nth$ ?i $?class)  ": "    (DiaGrade   ?param) crlf))  else  (loop-for-count (?i 1 (length$ $?class) ) do   (if (isRimesClass (nth$ ?i $?class)) then     (printout t $?name " in " (nth$ ?i $?class)  ": "  (RimesGrade ?param) crlf)   else 
   ;(if (isAdelClass (nth$ ?i $?class)) then     (printout t $?name " in " (nth$ ?i $?class)  ": "  (AdelGrade  ?param) crlf)) )))
;;END ---;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(reset)
(run)
