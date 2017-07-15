#lang scribble/acmart @acmsmall @screen
@require["bib.rkt"]

@title{Abstracting Definitional Interpreters}
@subtitle{Functional Pearl}

@(define (umd-author user name)
  (author name 
	  #:affiliation 
	  (affiliation #:institution 
		       (institution
			#:departments (list "Department of Computer Science")
			"University of Maryland")
		       #:city "College Park"
		       #:state "Maryland")
	  #:email
	  (string-append user "@cs.umd.edu")))

@umd-author["darais"]{David Darais}
@umd-author["labichn"]{Nicholas Labich}
@umd-author["pcn"]{Phúc C. Nguyễn}
@umd-author["dvanhorn"]{David Van Horn}

@setcopyright{rightsretained}
@acmJournal{PACMPL}
@acmYear{2017}
@acmVolume{1}
@acmNumber{ICFP}
@acmArticle{12}
@acmMonth{9}
@acmDOI{10.1145/3110256}
@acmPrice{}

@keywords{interpreters, abstract interpreters}

@CCSXML{

<ccs2012>
 <concept>
 <concept_id>10011007.10010940.10010992.10010998.10011000</concept_id>
 <concept_desc>Software and its engineering~Automated static analysis</concept_desc>
 <concept_significance>500</concept_significance>
 </concept>
 <concept>
 <concept_id>10011007.10011006.10011008.10011009.10011012</concept_id>
 <concept_desc>Software and its engineering~Functional languages</concept_desc>
 <concept_significance>500</concept_significance>
 </concept>
</ccs2012>

}

@ccsdesc[#:number 500]{Software and its engineering~Automated static analysis}
@ccsdesc[#:number 500]{Software and its engineering~Functional languages}

@;acmBadgeL["artifact_available.png"]
@;acmBadgeR["artifact_evaluated-reusable.png"]

@abstract{In this functional pearl, we examine the use of
 definitional interpreters as a basis for abstract
 interpretation of higher-order programming languages. As it
 turns out, definitional interpreters, especially those
 written in monadic style, can provide a nice basis for a
 wide variety of collecting semantics, abstract
 interpretations, symbolic executions, and their
 intermixings.

 But the real insight of this story is a replaying of an
 insight from Reynold's landmark paper, @emph{Definitional
  Interpreters for Higher-Order Programming Languages}, in
 which he observes definitional interpreters enable the
 defined-language to inherit properties of the
 defining-language. We show the same holds true for
 definitional @emph{abstract} interpreters. Remarkably, we
 observe that abstract definitional interpreters can inherit
 the so-called ``pushdown control flow'' property, wherein
 function calls and returns are precisely matched in the
 abstract semantics, simply by virtue of the function call
 mechanism of the defining-language.

 The first approaches to achieve this property for
 higher-order languages appeared within the last ten years,
 and have since been the subject of many papers. These
 approaches start from a state-machine semantics and
 uniformly involve significant technical engineering to
 recover the precision of pushdown control flow. In contrast,
 starting from a definitional interpreter, the pushdown
 control flow property is inherent in the meta-language and
 requires no further technical mechanism to achieve.}

@include-section{01-intro.scrbl}
@include-section{02-aam.scrbl}
@include-section{03-interp.scrbl}
@include-section{04-cache.scrbl}
@include-section{05-reynolds.scrbl}
@include-section{06-widening.scrbl}
@include-section{07-alt-abstraction.scrbl}
@include-section{08-symbolic-execution.scrbl}
@include-section{09-gc.scrbl}
@include-section{10-related-work.scrbl}
@include-section{11-conclusions.scrbl}

@;cite papers in the appendix but not otherwise cited
@void[@~cite{dvanhorn:Neilson:1999}]

@(generate-bib)

