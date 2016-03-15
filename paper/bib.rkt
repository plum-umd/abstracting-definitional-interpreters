#lang racket/base
(provide (all-defined-out))
(require scriblib/autobib)

(define-cite ~cite citet generate-bibliography #:style number-style)

(define dimvar (author-name "Dimitris" "Vardoulakis"))
(define dvh (author-name "David" "Van Horn"))
(define might (author-name "Matthew" "Might"))

(define aam
  (make-bib #:title "Abstracting Abstract Machines"
            #:author (authors dvh might)
  	    #:date "2010"
    	    #:location (proceedings-location "ACM International Conference on Functional Programming")))

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
            #:location (proceedings-location "ACM international conference on Object oriented programming systems languages and applications")))

(define nguyen-pldi15
  (make-bib #:title "Relatively Complete Counterexamples for Higher-order Programs"
            #:author (authors (author-name "Phúc C." "Nguyễn")
                              (author-name "David" "Van Horn"))
            #:date "2015"
            #:location (proceedings-location "36th ACM SIGPLAN Conference on Programming Language Design and Implementation")))

#| Robert Glück. Simulation of two-way pushdown automata revisited. In
Semantics, Abstract Interpretation, and Reasoning about Programs:
Essays Dedicated to David A. Schmidt on the Occasion of his Sixtieth
Birthday, 2013.  |#
