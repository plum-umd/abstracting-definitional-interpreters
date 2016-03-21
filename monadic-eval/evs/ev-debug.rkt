#lang racket/unit
(require racket/match
         "../map.rkt"
         "../signatures.rkt"
         "../unparse.rkt"
         "../transformers.rkt")

(import monad^ mstore^ menv^)
(export ev-debug^)

(define-monad M)

(define (binds e ev0 ev)
  (do ρ ← ask-env
      σ ← get-store
      (let ([xavs (for/list ([(x a) (∈ ρ)] #:when (a . ∈ . σ))
                    (list x a (σ a)))]
            [pbind (λ (xav) (printf "~a ↦ ~a ↦ ~a\n"
                                    (car   xav)
                                    (cadr  xav)
                                    (caddr xav)))])
        (printf "There are ~a active bindings.\n" (length xavs))
        (if (zero? (length xavs)) (step e ev0 ev)
            (let loop ()
              (printf "Look up a particular variable? ")
              (match (string-downcase (read-line))
                ["" 
                 (printf "All active bindings:\n")
                 (for-each pbind xavs)
                 (step e ev0 ev)]
                [x  (if ((string->symbol x) . ∈ . ρ)
                        (begin
                          (let ([a (ρ (string->symbol x))])
                            (printf "~a ↦ ~a ↦ ~a\n" x a (σ a))
                            (loop)))
                        (begin
                          (printf "No such binding exists.\n")
                          (loop)))]))))))

(define (step e ev0 ev [first-call? #f])
  (when first-call?
    (printf "[S]tep to the next subexp?\n")
    (printf "[E]valuate exp?\n")
    (printf "Print live [b]indings?\n"))
  (printf "eval ~a\n" (unparse e))
  (printf "[Step,eval,binds]> ")
  (define inp (read-line))
  (if (eof-object? inp)
      (do ρ ← ask-env (local-env (ρ 'debug 'eval) (ev e)))
      (match (string-downcase
              (if (zero? (string-length inp))
                  ""
                  (substring inp 0 1)))
        [(or "" "s") (do v ← ((ev0 ev) e)
                       (begin (printf "eval ~a\n   ⇒ ~a\n" (unparse e) v)
                              
                              (return v)))]
        ["b"         (binds e ev0 ev)]
        ["e"         (do ρ ← ask-env
                       v ← (local-env (ρ 'debug 'eval) (ev e))
                       (begin (printf "eval ~a\n   ⇒ ~a\n" (unparse e) v)
                              (return v)))]
        [unk
         (printf "Unknown input: ~a\n" unk)
         (step e ev0 ev)])))

(define (((ev-debug ev0) ev) e)
  (do ρ ← ask-env
      (if (not ('debug . ∈ . ρ))
          (local-env (ρ 'debug 'step) (step e ev0 ev #t))
          (match (ρ 'debug)
            ['eval ((ev0 ev) e)]
            ['step (step e ev0 ev)]
            [mgc   (error "unknown magic binding: ~a" mgc)]))))
