;;; Matthew Bennett
;;;  3-25-04 (yes i started early)
;;;  CSC 412 Ali
;;;  CLIPS Assignment #1 - Simpsons family problem

;;; List of possible incorrect:
;;; 1. Wrong fact on father
;;; 2. Wrong fact on mother
;;; 3. Wrong fact on son
;;; 4. Wrong fact on both daughter
;;; 5. Wrong fact on one of the daughter
;;; 6. Wrong fact on neighbor

(clear)

(deftemplate family
		 (slot father)
		 (slot mother)
		 (multislot son)
		 (multislot daughter))

(deftemplate neighbour (slot name))
(deftemplate isSimpsons (slot valid))

(deffacts community 
        (family (father Homer)
	  	    (mother	Marge)
	  	    (son   	Bart)
	  	    (daughter Maggie Lisa))
	  (neighbour(name Ned))
)

(defrule isSimpsons 
	(family (father Homer)
		  (mother Marge)
		  (son    Bart)
		  (daughter Lisa Maggie)
	)
	=>
	(assert (isSimpsons (valid TRUE)))
)

(defrule isSimpsons 
	(family (father Homer)
		  (mother Marge)
		  (son    Bart)
		  (daughter Maggie Lisa)
	)
	=>
	(assert (isSimpsons (valid TRUE)))
)

(defrule wrongFather
	(family (father ~Homer)
		  (mother ?mName)
		  (son    ?sName)
		  (daughter ?d1Name ?d2Name)
	)
	=>
	(printout t "Father is wrong" crlf)
	(assert (isSimpsons (valid FALSE)))
)

(defrule wrongMother
	(family (father ?hName)
		  (mother ~Marge)
		  (son    ?sName)
		  (daughter ?d1Name ?d2Name)
	)
	=>
	(printout t "Mother is wrong" crlf)
	(assert (isSimpsons (valid FALSE)))
)

(defrule wrongSon
	(family (father ?hName)
		  (mother ?mName)
		  (son    ~Bart)
		  (daughter ?d1Name ?d2Name)
	)
	=>
	(printout t "Son is wrong" crlf)
	(assert (isSimpsons (valid FALSE)))
)

(defrule wrongYoungerSis
	(family (father ?hName)
		  (mother ?mName)
		  (son    ?sName)
		  (daughter ~Maggie ?d2Name)
	 )
   (family (father ?hName)
		  (mother ?mName)
		  (son    ?sName)
		  (daughter ?d2Name ~Maggie)
  )
	=>
	(printout t "Younger sister is wrong" crlf)
	(assert (isSimpsons (valid FALSE)))
)

(defrule wrongOlderSis
	(family (father ?hName)
		  (mother ?mName)
		  (son    ?sName)
		  (daughter ~Lisa ?d2Name)
	 )
   (family (father ?hName)
		  (mother ?mName)
		  (son    ?sName)
		  (daughter ?d2Name ~Lisa)
  )
	=>
	(printout t "Older sister is wrong" crlf)
	(assert (isSimpsons (valid FALSE)))
)

(defrule wrongFamily
	(isSimpsons (valid FALSE))
	=>
	(printout t "The family is not the Simpsons" crlf)
)

(defrule wrongNeighbour
	(neighbour (name ~Ned))
	(isSimpsons (valid TRUE))
	=>
	(printout t "The family members are correct but the neighbour is wrong" crlf)
)

(defrule wrongNeighbourFamily
	(neighbour (name ~Ned))
	(isSimpsons (valid FALSE))
	=>
	(printout t "The family members are correct but the neighbour is wrong" crlf)
)


(defrule rightNeighbour
	(neighbour (name Ned))
	(isSimpsons (valid TRUE))
	=>
	(printout t "This family is the Simpsons" crlf)
)

(reset)
(run)
