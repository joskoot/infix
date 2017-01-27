#lang racket

(provide $ (rename-out ($ infix)) ! √ ^ € Ʃ Π)
(require (only-in racket (sqrt √) (expt ^)))

; ----------------------------------------------------------------------------------------------------
; Definition of operators and related procedures and syntax.
; These definitions can be made within transformer $ too, but as done below check-syntax and
; background expansion show binding arrows. Defined within syntax $ these binding arrows are lost.

(begin-for-syntax
 (require (only-in racket syntax-case with-syntax datum->syntax #%app quote syntax))
 (define operators (syntax->list #'(+ - * / ^ √ € ! quote unquote quasiquote Ʃ Π)))
 (define (operator? stx) (and (identifier? stx) (member stx operators free-identifier=?)))
 (define (var? stx) (and (identifier? stx) (not (member stx operators free-identifier=?))))
 (define (atom? stx) (syntax-case stx () ((x ...) #f) (x (not (operator? #'x)))))
 
 (define-syntax (stx-case stx)
  (syntax-case stx ()
   ((_ stx-expr clause ...)
    (with-syntax
     ((+ (datum->syntax stx '+))
      (- (datum->syntax stx '-))
      (* (datum->syntax stx '*))
      (/ (datum->syntax stx '/))
      (^ (datum->syntax stx '^))
      (√ (datum->syntax stx '√))
      (€ (datum->syntax stx '€))
      (! (datum->syntax stx '!))
      (Ʃ (datum->syntax stx 'Ʃ))
      (Π (datum->syntax stx 'Π))
      (= (datum->syntax stx '=))
      (quote (datum->syntax stx 'quote))
      (unquote (datum->syntax stx 'unquote))
      (quasiquote (datum->syntax stx 'quasiquote)))
   #'(syntax-case stx-expr (+ - * / ^ √ € ! quote unquote quasiquote Ʃ Π =) clause ...))))))

; ----------------------------------------------------------------------------------------------------

(define-syntax ($ infix-stx)

 (define (main)
  (syntax-case infix-stx ()
   ((_) (infix-error "missing expr" infix-stx))
   ((_ x ...) (parse-expr #'(x ...)))
   (_ (infix-error "incorrect use of syntax infix" infix-stx))))
 
 (define (parse-expr stx)
  (let-values (((pos-terms neg-terms) (parse-terms stx #f #'() #'())))
   (syntax-case (list pos-terms neg-terms) ()
    (((     ) (y    )) #'(- y))
    (((     ) (y ...)) #'(- (+ y ...)))
    (((x    ) (     )) #'x)
    (((x    ) (y ...)) #'(- x y ...))
    (((x ...) (     )) #'(+ x ...))
    (((x ...) (y ...)) #'(- (+ x ...) y ...)))))
 
 (define (parse-terms stx -? pos-terms neg-terms)
  (let-values (((term -? rest) (parse-term stx -?)))
   (let-values
    (((pos-terms neg-terms)
      (if -?
       (values pos-terms #`(#,@neg-terms #,term))
       (values #`(#,@pos-terms #,term) neg-terms))))
    (stx-case rest
     (() (values pos-terms neg-terms))
     ((x) (operator? #'x) (infix-error "missing element" #'x))
     ((+ x ...) (parse-terms #'(x ...) #f pos-terms neg-terms))
     ((- x ...) (parse-terms #'(x ...) #t pos-terms neg-terms))
     ((x y ...) (infix-error "missing operator" #'x))))))
 
 (define (parse-term stx -?)
  (stx-case stx
   ((x) (operator? #'x) (infix-error "missing element" #'x))
   ((+ x ...) (parse-term #'(x ...) -?))
   ((- x ...) (parse-term #'(x ...) (not -?)))
   (_
    (let-values (((numerators denominators -? rest) (parse-factors stx -?)))
     (syntax-case (list numerators denominators) ()
      (((     ) (y    )) (values #'(/ y) -? rest))
      (((     ) (y ...)) (values #'(/ (* y ...)) -? rest))
      (((x    ) (     )) (values #'x -? rest))
      (((x    ) (y ...)) (values #'(/ x y ...) -? rest))
      (((x ...) (     )) (values #'(* x ...) -? rest))
      (((x ...) (y ...)) (values #'(/ (* x ...) y ...) -? rest)))))))
 
 (define (parse-factors stx -?)
  (define (parse-factors stx -? /? numerators denominators)
   (let-values (((factor -? /? rest) (parse-factor stx -? /?)))
    (let-values
     (((numerators denominators)
        (if /?
         (values numerators #`(#,@denominators #,factor))
         (values #`(#,@numerators #,factor) denominators))))
     (stx-case rest
      (() (values numerators denominators -? rest))
      ((x) (operator? #'x) (infix-error "missing element" #'x))
      ((* x ...) (parse-factors #'(x ...) -? /? numerators denominators))
      ((/ x ...) (parse-factors #'(x ...) -? (not /?) numerators denominators))
      (_ (values numerators denominators -? rest))))))
  (parse-factors stx -? #f #'() #'()))
 
 (define (parse-factor stx -? /?)
  (stx-case stx
   (() (infix-error "missing factor" stx))
   ((x) (operator? #'x) (infix-error "missing element" #'x))
   ((* x ...) (parse-factor #'(x ...) -? /?))
   ((/ x ...) (parse-factor #'(x ...) -? (not /?)))
   ((+ x ...) (parse-factor #'(x ...) -? /?))
   ((- x ...) (parse-factor #'(x ...) (not -?) /?))
   (_ (let-values (((expt rest) (parse-expt stx))) (values expt -? /? rest)))))
 
 (define (parse-expt stx)
  (let-values (((base rest) (parse-element stx)))
   (let loop ((stx rest) (elements #`(#,base)))
    (stx-case stx
     ((x) (operator? #'x) (infix-error "missing exponent" #'x))
     ((^ x ...)
      (let-values (((element rest) (parse-element #'(x ...))))
       (loop rest #`(#,@elements #,element))))
     (_ (values (do-expt elements) stx))))))
 
 (define (do-expt stx)
  (syntax-case stx ()
   ((x) #'x)
   ((x y ...) #`(^ x #,(do-expt #'(y ...))))))
 
 (define (parse-element stx)
  (define (parse-e stx -? /?)
   (stx-case stx
    (('x . rest) (do! #''x #'rest -? /?))
    ((`x . rest) (do! #'`x #'rest -? /?))
    (((x ...) . rest) (do! (parse-expr #'(x ...)) #'rest -? /?))
    ((Ʃ (var = x ...) . rest)
     (let ((range (parse-args #'(x ...))))
      (let-values (((body rest) (parse-element #'rest)))
       (values #`(for-sum var #,range #,body) rest))))
    ((Π (var = x ...) . rest)
     (let ((range (parse-args #'(x ...))))
      (let-values (((body rest) (parse-element #'rest)))
       (values #`(for-prod var #,range #,body) rest))))
    ((€ x (arg ...) y ...) (do! #`(x #,@(parse-args #'(arg ...))) #'(y ...) -? /?))
    ((€ x y ...) (do! #'x #'(y ...) -? /?))
    ((fun (arg ...) . rest) (var? #'fun) (do! #`(fun #,@(parse-args #'(arg ...))) #'rest -? /?))
    ((atom . rest) (atom? #'atom) (do! #'atom #'rest -? /?))
    ((x) (operator? #'x) (infix-error "missing element" (car (syntax->list stx))))
    ((+ x ...) (parse-e #'(x ...) -? /?))
    ((- x ...) (parse-e #'(x ...) (not -?) /?))
    ((* x ...) (parse-e #'(x ...) -? /?))
    ((/ x ...) (parse-e #'(x ...) -? (not /?)))
    ((√ x ...)
     (let-values (((√arg rest) (parse-element #'(x ...))))
      (values (do-element #`(√ #,√arg) -? /?) rest)))
    ((^ x ...) (infix-error "incorrect ^" (car (syntax->list stx))))
    ((x y ...) (infix-error "unrecognized element" #'x))))
  (parse-e stx #f #f))
 
 (define (do! arg rest -? /?)
  (stx-case rest
   ((! . rest) (do! #`(! #,arg) #'rest -? /?))
   (_ (values (do-element arg -? /?) rest))))
 
 (define (do-element stx -? /?)
  (syntax-case (list -? /?) ()
   ((#f #f) stx)
   ((#f #t) #`(/ #,stx))
   ((#t #f) #`(- #,stx))
   ((#t #t) #`(- (/ #,stx)))))
 
 (define (parse-args stx)
  (define (parse-args arg stx)
   (stx-case stx
    (() (if (null? (syntax-e arg)) #'() #`(#,(parse-expr arg))))
    ((, x y ...)
     (if (null? (syntax-e arg)) (infix-error "missing argument" #'x)
      #`(#,(parse-expr arg) #,@(parse-args #'() #'(x y ...)))))
    ((x y ...) (parse-args #`(#,@arg x) #'(y ...)))))
  (parse-args #'() stx))
 
 (define infix-error
  (case-lambda
   ((msg            ) (raise-syntax-error 'infix msg infix-stx))
   ((msg stx        ) (raise-syntax-error 'infix msg infix-stx stx))
   ((msg stx sub-stx) (raise-syntax-error 'infix msg stx sub-stx))))
 
 (main))

; ----------------------------------------------------------------------------------------------------
; Auxiliary definitions.

(define-syntax (for-sum stx)
 (syntax-case stx ()
  ((_ var range body) #'(for-sum/prod + 0 var range body "Ʃ"))))

(define-syntax (for-prod stx)
 (syntax-case stx ()
  ((_ var range body) #'(for-sum/prod * 1 var range body "Π"))))

(define-syntax (for-sum/prod stx)
 (syntax-case stx ()
  ((_ fun init var (from to step) body type)
 #'(let ((f from) (t to) (s step))
    (when (zero? s)
     (error 'infix "~a loop with zero step (from=~s, to=~s)" type f t))
    (when (or (= (+ f s) f) (= (- t s) t) (infinite? (/ (- t f) s)))
     (error 'infix "infinite ~a loop with from=~s, to=~s, step=~s" type f t s))
    (let ((<> (if (positive? s) >= <=)))
     (let loop ((accum init) (var f))
      (if (<> var t) accum
       (loop (fun accum body) (+ var s)))))))
  ((_ fun init var (from to) body type)
 #'(for-sum/prod fun init var (from to 1) body type))))

(define-syntax (define-dummy-syntax stx)
 (syntax-case stx ()
  ((_ s)
 #'(define-syntax (s dummy-stx)
    (syntax-case dummy-stx ()
     ((x y (... ...)) (raise-syntax-error 's "valid in ($ infix-expr) only" #'x))
     (_               (raise-syntax-error 's "valid in ($ infix-expr) only" dummy-stx)))))))

(define-dummy-syntax €)
(define-dummy-syntax Ʃ)
(define-dummy-syntax Π)

(define (! n)
 (unless (exact-nonnegative-integer? n) (raise-argument-error '! "exact-nonnegative-integer?" n))
 (let ! ((n n) (f 1)) (if (zero? n) f (! (sub1 n) (* n f)))))

; The end ============================================================================================
