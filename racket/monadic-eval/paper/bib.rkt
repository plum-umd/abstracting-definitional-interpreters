#lang racket/base
(provide (all-defined-out))
(require scriblib/autobib)

(define-cite ~cite citet generate-bibliography #:style number-style)

(define dimvar (author-name "Dimitris" "Vardoulakis"))
(define dvh (author-name "David" "Van Horn"))
(define might (author-name "Matthew" "Might"))
(define darais (author-name "David" "Darais"))
(define pcn (author-name "Phúc C." "Nguyễn"))
(define sth (author-name "Sam" "Tobin-Hochstadt"))
            

(define plt-tr1
  (make-bib #:title    "Reference: Racket"
            #:author   (authors "Matthew Flatt" "PLT")
            #:date     "2010"
            #:location (techrpt-location #:institution "PLT Design Inc."
                                         #:number "PLT-TR-2010-1")
            #;#;#:url      "https://racket-lang.org/tr1/"))
                  
(define aam
  (make-bib #:title "Abstracting Abstract Machines"
            #:author (authors dvh might)
  	    #:date "2010"
    	    #:location (proceedings-location "ACM International Conference on Functional Programming")))

(define aam-jfp
  (make-bib #:title "Systematic Abstraction of Abstract Machines"
            #:author (authors dvh might)
  	    #:date "2012"
    	    #:location (journal-location "Journal of Functional Programming"
					 #:number "4--5"
					 #:volume "22")))


(define reynolds72
  (make-bib #:title "Definitional interpreters for higher-order programming languages"
            #:author (author-name "John" "Reynolds")
	    #:date "1972"
	    #:location (proceedings-location "ACM Annual Conference")))

(define cfa2-lmcs
  (make-bib #:title "CFA2: a Context-Free Approach to Control-Flow Analysis"
            #:author (authors dimvar
			      (author-name "Olin" "Shivers"))
            #:date "2011"
            #:location (journal-location "Logical Methods in Computer Science"
					 #:number "2:3"
					 #:volume "7")))

(define cfa2-diss
  (make-bib #:title "CFA2: Pushdown Flow Analysis for Higher-Order Languages"
            #:author (authors dimvar)
            #:date "2012"
            #:location (dissertation-location #:institution "Northeastern University")))

(define steele-popl94
  (make-bib #:title "Building interpreters by composing monads"
            #:author (author-name "Guy L." "Steele" #:suffix "Jr.")
            #:date "1994"
            #:location (proceedings-location "21st ACM SIGPLAN-SIGACT Symposium on Principles of Programming Languages")))

(define liang-popl95
  (make-bib #:title "Monad transformers and modular interpreters"
            #:author (authors (author-name "Sheng" "Liang")
			      (author-name "Paul" "Hudak")
			      (author-name "Mark" "Jones"))
            #:date "1995"
            #:location (proceedings-location "22nd ACM SIGPLAN-SIGACT Symposium on Principles of Programming Languages")))

(define pdcfa-sfp10
  (make-bib #:title "Pushdown Control-Flow Analysis of Higher-Order Programs"
            #:author (authors (author-name "Christopher" "Earl")
			      (author-name "Matthew" "Might")
			      dvh)
	    #:date "2010"
	    #:location (proceedings-location "Workshop on Scheme and Functional Programming")))

(define ager-tcs05
  (make-bib #:title "A functional correspondence between monadic evaluators and abstract machines for languages with computational effects"
	    #:author (authors (author-name "Mads Sig" "Ager")
			      (author-name "Olivier" "Danvy")
			      (author-name "Jan" "Midtgaard"))
	    #:date "2005"
	    #:location (journal-location "Theoretical Computer Science"
					 #:volume "342"
					 #:number "1")))

(define flatt-pldi98
  (make-bib #:title "Units: Cool Modules for HOT Languages"
            #:author (authors (author-name "Matthew" "Flatt")
			      (author-name "Matthias" "Felleisen"))
	    #:date "1998"
	    #:location (proceedings-location "ACM SIGPLAN 1998 Conference on Programming Language Design and Implementation")))

(define king-76
  (make-bib #:title "Symbolic Execution and Program Testing"
            #:author (authors (author-name "James C." "King"))
            #:date "1976"
            #:location (proceedings-location "Communications of the ACM")))

(define vanhorn-oopsla12
  (make-bib #:title "Higher-order symbolic execution via contracts"
            #:author (authors (author-name "Sam" "Tobin-Hochstadt")
                              dvh)
            #:date "2012"
            #:location (proceedings-location "ACM SIGPLAN International Conference on Object Oriented Programming, Systems, Languages, and Applications")))

(define nguyen-pldi15
  (make-bib #:title "Relatively Complete Counterexamples for Higher-order Programs"
            #:author (authors pcn dvh)
            #:date "2015"
            #:location (proceedings-location "36th ACM SIGPLAN Conference on Programming Language Design and Implementation")))

(define glück-schmidtfest13
  (make-bib #:title "Simulation of two-way pushdown automata revisited"
            #:author (author-name "Robert" "Glück")
            #:location (journal-location "Semantics, Abstract Interpretation, and Reasoning about Programs:
Essays Dedicated to David A. Schmidt on the Occasion of his Sixtieth
Birthday")
	    #:date "2013"))


(define sergey-pldi13
  (make-bib #:title "Monadic Abstract Interpreters"
            #:author (authors (author-name "Ilya" "Sergey")
			      (author-name "Dominique" "Divriese")
			      might
			      (author-name "Jan" "Midtgaard")
			      darais
			      (author-name "Dave" "Clarke")
			      (author-name "Frank" "Piessens"))
	    #:location (proceedings-location "ACM SIGPLAN International Conference on Programming Language Design and Implementation")
	    #:date "2013"))

(define gilray-popl16
  (make-bib #:title "Pushdown Control-Flow Analysis for Free"
	    #:author (authors (author-name "Thomas" "Gilray")
			      (author-name "Steven" "Lyde")
			      (author-name "Michael D." "Adams")
			      might
			      dvh)
	    #:location (proceedings-location "43rd ACM SIGPLAN-SIGACT Symposium on Principles in Programming Languages")
	    #:date "2016"))

(define darais-oopsla15
  (make-bib #:title "Galois Transformers and Modular Abstract Interpreters"
	    #:author (authors darais might dvh)
	    #:location (proceedings-location "ACM SIGPLAN Conference on Object-Oriented Programming, Systems, Languages, and Applications")
	    #:date "2015"))

(define johnson-dls14
  (make-bib #:title "Abstracting Abstract Control"
	    #:author (authors (author-name "J. Ian" "Johnson")
			      dvh)
	    #:location (proceedings-location "10th ACM Symposium on Dynamic Languages")
	    #:date "2014"))

(define tobin-hochstadt-pldi11
  (make-bib #:title "Languages as Libraries"
	    #:author (authors sth
			      (author-name "Vincent" "St-Amour")
			      (author-name "Ryan" "Culpepper")
			      (author-name "Matthew" "Flatt")
			      (author-name "Matthias" "Felleisen"))
	    #:location (proceedings-location "ACM SIGPLAN International Conference on Programming Language Design and Implementation")
	    #:date "2011"))

(define might-popl07
  (make-bib #:title "Logic Flow Analysis of Higher-Order Programs"
            #:author (authors might)
            #:location (proceedings-location "34th Annual ACM SIGPLAN-SIGACT Symposium on Principles of Programming Languages")
	    #:date "2007"))

(define earl-icfp12
  (make-bib #:title "Introspective Pushdown Analysis of Higher-Order Programs"
            #:author (authors (author-name "Christopher" "Earl")
			      (author-name "Ilya" "Sergey")
			      might
			      dvh)
	    #:location (proceedings-location "ACM SIGPLAN International Conference on Functional Programming")
	    #:date "2012"))
			      
(define johnson-jfp14
  (make-bib #:title "Pushdown flow analysis with abstract garbage collection"
	    #:author (authors (author-name "J. Ian" "Johnson")
			      (author-name "Ilya" "Sergey")
			      (author-name "Christopher" "Earl")
			      might
			      dvh)
	    #:location (journal-location "Journal of Functional Programming"
					 #:number "2--3"
					 #:volume "24")
	    #:date "2014"))

(define moggi-89
  (make-bib #:title "An abstract view of programming languages"
            #:author (author-name "Eugenio" "Moggi")
	    #:location (techrpt-location #:institution "Edinburgh University" #:number "ECS-LFCS-90-113")
	    #:date "1989"))

(define rompf-15
  (make-bib #:title "From F to DOT: Type Soundness Proofs with Definitional Interpreters"
            #:author (authors (author-name "Tiark" "Rompf")
	    	     	      (author-name "Nada" "Amin"))
	     #:location (techrpt-location #:institution "arXiv" #:number "1510.05216")
             #:date "2015"))