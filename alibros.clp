;;; Matthew Bennett
;;;  3-28-04 (yes i started early)
;;;  CSC 412 Ali
;;;  CLIPS Assignment #3 - the Amazing Ali brothers(or are they?)

;; takes some data about two guys and determines if they are related.
 ;; if they are related, determines which is the older one

(clear)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Create template of two person
(deftemplate person1
 (slot first)
 (slot last)
 (slot blood) (slot origin) (slot religion) (slot language) (slot work) (slot height) (slot weight) (slot iq) (slot personality) (slot productivity) (slot sport) (slot tickets) (slot car))

(deftemplate person2
 (slot first)
 (slot last)
 (slot blood) (slot origin) (slot religion) (slot language) (slot work) (slot height) (slot weight) (slot iq) (slot personality) (slot productivity) (slot sport) (slot tickets) (slot car))

;;; Create template for the counter 
(deftemplate similar(slot countSimilar(type INTEGER)))
(deftemplate points1(slot ptsPerson1 (type INTEGER)))
(deftemplate points2(slot ptsPerson2 (type INTEGER)))

;;; Flag to tell whether they are related or not
(deftemplate relation(slot isRelated))

;;; Flags to make sure that the rules are not fired again and again
;;; The flags name are identical to the rule name to make it easy to relate
;;; In the real program , you will need more flags for each condition
(deftemplate checkName (slot done))
(deftemplate checkBlood (slot done))
(deftemplate checkOrigin (slot done))
(deftemplate checkReligion (slot done))
(deftemplate checkLanguage (slot done))
(deftemplate checkWork (slot done))
(deftemplate checkHeight (slot done ))
(deftemplate checkWeight (slot done ))
(deftemplate checkIQ (slot done))
(deftemplate checkPersonality (slot done))
(deftemplate checkProductivity (slot done))
(deftemplate checkSport (slot done))
(deftemplate checkTickets (slot done))
(deftemplate checkCar (slot done))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; FACT LIST ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Create the facts and also set the flags to false
(deffacts factlist (person1   (first Adel)
  (last Ali)  (blood O)  (origin Egypt)  (religion Islam)  (language Arabic)  (work Computer)  (height 72)  (weight 160)  (iq 190)  (personality Wise)  (productivity 10)  (sport squash)  (tickets 10)  (car sedan)) (person2   (first Dia)
  (last Ali)  (blood A)  (origin Egypt)  (religion Islam)  (language Arabic)  (work Mathematics)  (height 74)  (weight 180)  (iq 200)  (personality Trouble)  (productivity 10)  (sport aerobics)  (tickets 5)  (car suv))

	(points1 (ptsPerson1 0))
	(points2 (ptsPerson2 0)) 
	(similar (countSimilar 0))
	(checkName (done FALSE))
	(checkBlood (done FALSE))	
	(checkOrigin (done FALSE))
	(checkReligion (done FALSE))
	(checkLanguage (done FALSE))
	(checkWork (done FALSE))
	(checkHeight (done FALSE))
	(checkWeight (done FALSE))
	(checkIQ (done FALSE))
	(checkPersonality (done FALSE))
	(checkProductivity (done FALSE))
	(checkSport (done FALSE))
	(checkTickets (done FALSE))
	(checkCar (done FALSE))

	(relation (isRelated FALSE))
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Rules to check if they are related or not    ;;;;;;
;;; It will printout the similarites             ;;;;;;
;;; and then increment the similarities counter  ;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; The rule to check whether they have the same last name or not.
(defrule checkName
	(person1 (last ?name1))
	(person2 (last ?name1))
	?flag <- (checkName (done FALSE))
	?fact <- (similar(countSimilar ?value))
	=>
	(modify ?fact(countSimilar (+ ?value 1 )))
	;;(printout t "Total match previously : " ?value crlf)
	(printout t "A name match : " $?name1 crlf)
	(modify ?flag(done TRUE)))

;;; The rule to check whether they have the same blood type or not.
(defrule checkBlood
	(person1 (blood ?blood1))
	(person2 (blood ?blood1))
	?flag <- (checkBlood (done FALSE))
	?fact <- (similar(countSimilar ?value))
	=>
	(modify ?fact(countSimilar (+ ?value 1 )))
	;;(printout t "Total match previously : " ?value crlf)
	(printout t "A blood match : " ?blood1 crlf)
	(modify ?flag(done TRUE)))

;;; The rule to check whether they have the same country of origin
(defrule checkOrigin
	(person1 (origin ?origin1))
	(person2 (origin ?origin1))
	?flag <- (checkOrigin (done FALSE))
	?fact <- (similar(countSimilar ?value))
	=>
	(modify ?fact(countSimilar (+ ?value 1 )))
	;;(printout t "Total match previously : " ?value crlf)
	(printout t "An origin match : " ?origin1 crlf)
	(modify ?flag (done TRUE)))

;;; The rule to check whether they have the same religion
(defrule checkReligion
	(person1 (religion ?var1))
	(person2 (religion ?var1))
	?flag <- (checkReligion (done FALSE))
	?fact <- (similar(countSimilar ?value))
	=>
	(modify ?fact(countSimilar (+ ?value 1 )))
	;;(printout t "Total match previously : " ?value crlf)
	(printout t "A religion match : " ?var1 crlf)
	(modify ?flag (done TRUE)))

;;; The rule to check whether they have the same language
(defrule checkLanguage
	(person1 (language ?var1))
	(person2 (language ?var1))
	?flag <- (checkLanguage (done FALSE))
	?fact <- (similar(countSimilar ?value))
	=>
	(modify ?fact(countSimilar (+ ?value 1 )))
	;;(printout t "Total match previously : " ?value crlf)
	(printout t "A language match : " ?var1 crlf)
	(modify ?flag (done TRUE)))

;;; The rule to check whether they have the same field of work
(defrule checkWork
	(person1 (work ?var1))
	(person2 (work ?var1))
	?flag <- (checkWork (done FALSE))
	?fact <- (similar(countSimilar ?value))
	=>
	(modify ?fact(countSimilar (+ ?value 1 )))
	;;(printout t "Total match previously : " ?value crlf)
	(printout t "A work match : " ?var1 crlf)
	(modify ?flag (done TRUE)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Rules to find which one is older
;;; An older person are heavier in body weight

;;; Rule to check their weight
(defrule checkWeight
	(relation (isRelated TRUE))
	(person1 (weight ?weight1) (last ?lname1) (first ?fname1))
	(person2 (weight ?weight2) (last ?lname2) (first ?fname2))
	?flag <- (checkWeight(done FALSE))
	?pt1 <- (points1 (ptsPerson1 ?point1))
	?pt2 <- (points2 (ptsPerson2 ?point2))
	=>	
	(modify ?flag (done TRUE))
	(if (> ?weight1 ?weight2)
		then 
		(printout t ?fname1 " "  ?lname1 " add 1 point from " ?point1 " due to heavier weight" crlf)
		(modify ?pt1 (ptsPerson1(+ ?point1 1))) 
		else
		(printout t ?fname2 " " ?lname2 " add 1 point from " ?point2 " due to heavier weight "crlf)
		(modify ?pt2 (ptsPerson2(+ ?point2 1)))
	))

;;; Rule to check which person has the higher IQ
;;; Person with higher IQ are said to be older , add 1 to the person with higher IQ
(defrule checkIQ
	(relation (isRelated TRUE))
	(person1 (iq ?IQ1) (last ?lname1) (first ?fname1))
	(person2 (iq ?IQ2) (last ?lname2) (first ?fname2))
	?flag <- (checkIQ(done FALSE))
	?pt1 <- (points1 (ptsPerson1 ?point1))
	?pt2 <- (points2 (ptsPerson2 ?point2))
	=>	
	(modify ?flag (done TRUE))
	(if (> ?IQ1 ?IQ2)
		then 
		(printout t ?fname1 " "  ?lname1 " add 1 point from " ?point1 " due to higher IQ " crlf)
		(modify ?pt1 (ptsPerson1(+ ?point1 1))) 
		else
		(printout t ?fname2 " " ?lname2 " add 1 point from " ?point2 " due to higher IQ "crlf)
		(modify ?pt2 (ptsPerson2(+ ?point2 1)))
	))

;;; Check which person has more speeding tickets
;;; since younger person tends to drive faster, they will get more tickets
;;; Subtract 1 to the person with higher number of speeding tickets
(defrule checkTickets
	(relation (isRelated TRUE))
	(person1 (tickets ?speed1) (last ?lname1) (first ?fname1))
	(person2 (tickets ?speed2) (last ?lname2) (first ?fname2))
	?flag <- (checkTickets(done FALSE))
	?pt1 <- (points1 (ptsPerson1 ?point1))
	?pt2 <- (points2 (ptsPerson2 ?point2))
	=>	
	(modify ?flag (done TRUE))
	(if (> ?speed1 ?speed2)
		then 
		(printout t ?fname1 " "  ?lname1 " subtract 1 point from " ?point1 " due to speeding tickets " crlf)
		(modify ?pt1 (ptsPerson1(- ?point1 1))) 
		else
		(printout t ?fname2 " " ?lname2 " subtract 1 point from " ?point2 " due to speeding tickets " crlf)
		(modify ?pt2 (ptsPerson2(- ?point2 1)))
	))
;EXPERIMENTAL PART???????????????????????????????????????????????????????????????????????????????????????????
;;; Check which person is taller
;;; older person should be taller
;;; add 1 to the taller person
(defrule checkHeight
	(relation (isRelated TRUE))
	(person1 (height ?h1) (last ?lname1) (first ?fname1))
	(person2 (height ?h2) (last ?lname2) (first ?fname2))
	?flag <- (checkHeight(done FALSE))
	?pt1 <- (points1 (ptsPerson1 ?point1))
	?pt2 <- (points2 (ptsPerson2 ?point2))
	=>	
	(modify ?flag (done TRUE))
	(if (> ?h1 ?h2)
		then 
		(printout t ?fname1 " "  ?lname1 " add 1 point from " ?point1 " due to being taller " crlf)
		(modify ?pt1 (ptsPerson1(+ ?point1 1))) 
		else
		(printout t ?fname2 " " ?lname2 " add 1 point from " ?point2 " due to being taller " crlf)
		(modify ?pt2 (ptsPerson2(+ ?point2 1)))
	))

;;; Check which person is less productive
;;; older person should be less productive
;;; add 1 to the less productive person
(defrule checkProductivity
	(relation (isRelated TRUE))
	(person1 (productivity ?h1) (last ?lname1) (first ?fname1))
	(person2 (productivity ?h2) (last ?lname2) (first ?fname2))
	?flag <- (checkProductivity(done FALSE))
	?pt1 <- (points1 (ptsPerson1 ?point1))
	?pt2 <- (points2 (ptsPerson2 ?point2))
	=>	
	(modify ?flag (done TRUE))
	(if (> ?h1 ?h2)
		then 
		(printout t ?fname1 " "  ?lname1 " subtract 1 point from " ?point1 " due to being more productive " crlf)
		(modify ?pt1 (ptsPerson1(- ?point1 1))) 
		else
    (if (< ?h1 ?h2)
		 (printout t ?fname2 " " ?lname2 " subtract 1 point from " ?point2 " due to being more productive " crlf)
	 	 (modify ?pt2 (ptsPerson2(- ?point2 1)))
    )
	))

;;; Check if person is wise
;;; older person should be wise
;;; add 1 to the wise person
(defrule checkPersonality
	(relation (isRelated TRUE))
	(person1 (personality Wise) (last ?lname1) (first ?fname1))
	(person2 (personality ?h2) (last ?lname2) (first ?fname2))
	?flag <- (checkPersonality(done FALSE))
	?pt1  <- (points1 (ptsPerson1 ?point1))
	?pt2  <- (points2 (ptsPerson2 ?point2))
	=>	
	(modify ?flag (done TRUE))
	(printout t ?fname1 " "  ?lname1 " add 1 point from " ?point1 " due to being wise " crlf)
	(modify ?pt1 (ptsPerson1(+ ?point1 1))) 
))
(defrule checkPersonality2
	(relation (isRelated TRUE))
	(person1 (personality ?h1) (last ?lname1) (first ?fname1))
	(person2 (personality Wise) (last ?lname2) (first ?fname2))
	?pt1  <- (points1 (ptsPerson1 ?point1))
	?pt2  <- (points2 (ptsPerson2 ?point2))
	=>	
	(printout t ?fname1 " "  ?lname2 " add 1 point from " ?point2 " due to being wise " crlf)
	(modify ?pt2 (ptsPerson1(+ ?point2 1))) 
))

;;; Check if person plays squash
;;; younger person should play squash
;;; sub 1 the squash player person
(defrule checkSport
	(relation (isRelated TRUE))
	(person1 (sport squash) (last ?lname1) (first ?fname1))
	(person2 (sport ?h2) (last ?lname2) (first ?fname2))
	?flag <- (checkSport(done FALSE))
	?pt1  <- (points1 (ptsPerson1 ?point1))
	?pt2  <- (points2 (ptsPerson2 ?point2))
	=>	
	(modify ?flag (done TRUE))
	(printout t ?fname1 " "  ?lname1 " subtract 1 point from " ?point1 " due to playing Squash " crlf)
	(modify ?pt1 (ptsPerson1(- ?point1 1))) 
))
(defrule checkSport2
	(relation (isRelated TRUE))
	(person1 (sport ?h1) (last ?lname1) (first ?fname1))
	(person2 (sport squash) (last ?lname2) (first ?fname2))
	?pt1  <- (points1 (ptsPerson1 ?point1))
	?pt2  <- (points2 (ptsPerson2 ?point2))
	=>	
	(printout t ?fname1 " "  ?lname2 " subtract 1 point from " ?point2 " due to playing Squash " crlf)
	(modify ?pt1 (ptsPerson1(- ?point2 1))) 
))

;;; Check if person drives sporty car
;;; younger person should drive SUV
;;; sub 1 the SUV driver
(defrule checkCar
	(relation (isRelated TRUE))
	(person1 (car suv) (last ?lname1) (first ?fname1))
	(person2 (car ?h2) (last ?lname2) (first ?fname2))
	?flag <- (checkCar(done FALSE))
	?pt1  <- (points1 (ptsPerson1 ?point1))
	?pt2  <- (points2 (ptsPerson2 ?point2))
	=>	
	(modify ?flag (done TRUE))
	(printout t ?fname1 " "  ?lname1 " subtract 1 point from " ?point1 " due to driving a sporty car " crlf)
	(modify ?pt1 (ptsPerson1(- ?point1 1))) 
))
(defrule checkSport2
	(relation (isRelated TRUE))
	(person1 (car ?h1) (last ?lname1) (first ?fname1))
	(person2 (car suv) (last ?lname2) (first ?fname2))
	?flag <- (checkCar(done FALSE))
	?pt1  <- (points1 (ptsPerson1 ?point1))
	?pt2  <- (points2 (ptsPerson2 ?point2))
	=>	
	(modify ?flag (done TRUE))
	(printout t ?fname1 " "  ?lname2 " subtract 1 point from " ?point2 " due to driving a sporty car " crlf)
	(modify ?pt1 (ptsPerson1(- ?point2 1))) 
))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;    tacos and guts part  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ETERNAL ENIGMA #1: ARE THEY RELATED??? ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defrule checkRelated
	?fact <- (relation (isRelated FALSE))
	(similar (countSimilar ?value ))
	=>
	(if (> ?value 3)
	    then
	    (modify ?fact (isRelated TRUE))
          (printout t "They are related!" crlf)
          ;;(printout t "Alert !!!!!! Alert !!!! Now checking which one are older !!!" crlf crlf)
	))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ETERNAL ENIGMA #2: WHO IS OLDER??? ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defrule checkOlder
	(relation (isRelated TRUE))
	(checkTickets (done TRUE))
	(checkIQ (done TRUE))
	(checkWeight (done TRUE))
	(person1 (last ?lname1) (first ?fname1))
	(person2 (last ?lname2) (first ?fname2))
	(points1 (ptsPerson1 ?point1))
	(points2 (ptsPerson2 ?point2))
	=>
	(if (> ?point1 ?point2)
		then 
		(printout t crlf ?fname1 " "  ?lname1 " is the OLDER BROTHER " crlf)	
		else
		(printout t crlf ?fname2 " " ?lname2 " is the OLDER BROTHER" crlf)
	)
	(printout t ?fname1 " "  ?lname1 " total points =  " ?point1 crlf)
	(printout t ?fname1 " "  ?lname1 " total points = " ?point2 crlf)
)

(reset)
(run)
