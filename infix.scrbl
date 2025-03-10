#lang scribble/manual

@(require scribble/eval "infix.rkt" (for-label "infix.rkt" racket))
@(require (only-in scribble/eval interaction))
@title{Embedding infix notation in Racket}
@author{Jacob J. A. Koot}
@;@(defmodule "infix.rkt" #:packages ())
@(defmodule infix/infix #:packages ()) 

@(define => (string (integer->char 8658)))

@section{Syntaxes infix and $}

Syntaxes @racket[$] and @racket[infix]
allow infix notation to be embedded in Racket programs.
@racket[$] and @racket[infix] are synonyms of each other in the sense of
@racket[free-identifier=?].

@defform[#:kind "syntax" ($ infix-expr)
         #:grammar
         ([infix-expr addition]
          [addition term
           (code:line addition + term)
           (code:line addition - term)]
          [term factor
           (code:line factor * term)
           (code:line factor / term)]
          [factor power]
          [power element (code:line element ^ power)]
          [element token (infix-expr)
           (code:line + element)
           (code:line - element)
           (code:line * element)
           (code:line / element)
           (code:line √ element)
           (code:line element !)
           (code:line
            Ʃ(id = infix-expr @#,(litchar ",")infix-expr [@#,(litchar ",")infix-expr]) element)
           (code:line
            Π(id = infix-expr @#,(litchar ",")infix-expr [@#,(litchar ",")infix-expr]) element)]
          [token
           (code:line atom)
           (code:line id)
           (code:line id arglist
                      (code:comment @#,t{@=> (id ($ arg) ...); function or macro call}))
           (code:line € sexpr (code:comment @#,t{=> sexpr}))
           (code:line € sexpr arglist
                      (code:comment @#,t{@=> (sexpr ($ arg) ...); function or macro call}))
           (code:line @#,(litchar "'")sexpr
                      (code:comment @#,t{@=> 'sexpr ; not transformed}))
           (code:line @#,(litchar "`")sexpr
                      (code:comment @#,t{@=> @litchar{`sexpr} ; not transformed, although})
                      (code:comment @#,t{@racket[$] can be called in @racket[unquote]d parts.}))]
          [arglist () (ne-arglist)]
          [ne-arglist arg (code:line ne-arglist @#,(litchar ",") arg)]
          [arg infix-expr])
         #:contracts(
          [id
(code:line @#,(litchar "id not") @#,racket[free-identifier=?] @#,(litchar "with an operator"))]
          [sexpr
           (code:line @#,(litchar "symbolic expression"))]
          [atom
           (code:line @#,(litchar "symbolic expression, but not an id nor a list"))])]{
Transforms infix-expr to a corresponding Racket prefix-expr that computes
the value of the infix-expr. For example: }

@racket[($ 1 + 2 * 3)] @=> @racket[(+ 1 (* 2 3))] → @racket[7].

@elem['lsquo]@elem[@=>]@elem['rsquo] means expansion and @elem['lsquo]→@elem['rsquo] evaluation.

@defform[#:kind "syntax" (infix infix-expr)]{
Same as @racket[$] which is described immediately above here.}

@subsection{Additional syntaxes and functions.}

@defform[#:kind "syntax" #:id € €]{
Inhibits infix→prefix transformation.
For infix-exprs only. Syntax error when used outside an infix-expr.
In the sexpr of @elem['ldquo]@racket[€ sexpr]@elem['rdquo] calls to @racket[$] are expanded.}

@defform[#:kind "syntax" #:id Ʃ Ʃ]{
Summation. For infix-exprs only. Syntax error when used outside an infix-expr.}

@defform[#:kind "syntax" #:id Π Π]{
Product.
For infix-exprs only. Syntax error when used outside an infix-expr.}

@defproc[(! (n exact-nonnegative-integer?)) exact-positive-integer?]{Factorial.}

@defproc[(^ (base number?) (exponent number?)) number?]{
Exponentiation. @racket[^] is @racket[free-identifier=?] with @racket[expt].}

@defproc[(√ (x number?)) number?]{
Square root. @racket[√] is @racket[free-identifier=?] with @racket[sqrt].}

@section{Operators}

The following operators can be used:

@tabular[#:sep @hspace[3] #:row-properties '(bottom-border ())
(list
 (list @bold{operator} @bold{usage} @bold{position} @bold{function})
 (list @elem[@code{+}] "monadic" "prefix" "no operation")
 (list @elem[@code{-}] "monadic" "prefix" "flip sign")
 (list @elem[@code{*}] "monadic" "prefix" "no operation")
 (list @elem[@code{/}] "monadic" "prefix" "flip between numerator and denominator")
 (list @elem[@code{√}] "monadic" "prefix" "square root")
 (list @elem[@code{!}] "monadic" "postfix" "factorial")
 (list @elem[@code{€}] "monadic" "prefix" "inhibit transformation")
 (list @elem[@code{+}] "diadic" "infix" "addition, associative")
 (list @elem[@code{-}] "diadic" "infix" "subtraction")
 (list "" 'cont 'cont @elem["left associative: " @code{1 - 2 - 3 = (1 - 2) - 3}])
 (list @elem[@code{*}] "diadic" "infix" "multiplication, associative")
 (list @elem[@code{/}] "diadic" "infix" "division")
 (list "" 'cont 'cont @elem["right associative: " @code{1 / 2 / 3 = 1 / (2 / 3)}])
 (list @elem[@code{^}] "diadic" "infix" "exponentiation")
 (list "" 'cont 'cont @elem["right associative: " @code{1 ^ 2 ^ 3 = 1 ^ (2 ^ 3)}])
 (list @litchar{'} "monadic" "prefix" "quotation, no transformation")
 (list @litchar{`} "monadic" "prefix" "quasiquotation")
 (list "" 'cont 'cont @elem["in unquoted parts " @racket[$] " can be called, though"])
 (list @elem[@code{Ʃ}] "special" 'cont "indexed summation")
 (list @elem[@code{Π}] "special" 'cont "indexed product")
 (list @litchar{,} 'cont 'cont "separator in arglists"))]
 
In order to avoid name collisions between operators and variables the operators of an infix-expr
are recognized with @racket[free-identifier=?]. For example:

@racket[($ 2 expt 3)] is recognized the same as @racket[($ 2 ^ 3)]

provided @racket[expt] and @racket[^]
have their original bindings as exported from @racket[racket] cq @racket[infix.rkt].

In the following example @racket[plus] is @racket[free-identifier=?] with racket's @racket[+].
In the @racket[let]-form the binding of @racket[+] is shaddowed.
Here @code{+} is a variable and @racket[plus] an operator.

@interaction[
(require "infix.rkt" (only-in racket (+ plus)))
(let ((+ 1)) ($ + plus 2))]

Notice that the following does not work:

@interaction[
(require "infix.rkt")
(code:line (define plus +) (code:comment "plus is not identifier=? with +."))
(let ((+ 1)) ($ + plus 2))]

@subsection{Precedence rules}

From high to low the precedence of operators is:

@tabular[#:sep @hspace[3]
 (list
  (list
   @elem[@code{(infix-expr)}]
   "precedence is overruled by parentheses")
  (list
   @elem[@code{!}]
   'cont)
  (list
   @elem[@code{+ - * / √ €} @litchar{ ' ` } @code{Ʃ Π}]
   @elem["where "@code{+ - * /} " are used monadically"])
  (list
   @elem[@code{^}]
   'cont)
  (list
   @elem[@code{*}]
   "where used diadically")
  (list
   @elem[@code{/}]
   "where used diadically")
  (list
   @elem[@code{+ -}]
   "where used diadically"))]

An infix-expr consists of terms. Every term, the first one excepted,
must begin with a sign. The first one can have a sign, though.
There is no need to give diadic @racket[+] and diadic
@racket[-] distinct precedences. In fact an infix-expr is parsed as a sequence of terms.
Positive terms come in the plus, the negative ones in the minus.
This makes the diadic minus sign right associative.

In a term diadic @racket[*] has precedence over diadic @racket[/].
A factor is in the numerator cq denominator if
within its term it is preceeded by an even cq odd number of @racket[/]
(at the nesting level of the term)
Operator @racket[*], both monadic and diadic, leaves the factor in its place
(numerator or denominator).
@racket[/], both monadic and diadic, is a switch between numerator and denominator.
This makes division right associative.
Within a term, diadic @racket[*] and diadic @racket[/] are not treated in the
same way as the diadic @racket[+] and diadic @racket[-] in an addition.
The distinction is explained by the following examples:

@code{($   a + b - c + d)} @=> @code{(- (+ a b d) c)}@linebreak[]
@code{($ - a + b - c + d)} @=> @code{(- (+ b d) a c)}@linebreak[]
@code{($   a * b / c * d)} @=> @code{(/ (* a b) c d)}@linebreak[]
@code{($ / a * b / c * d)} @=> @code{(/ (* c d) a b)}@linebreak[]

@subsection{Indexed summation and multiplication}

The summation

@code{Ʃ(id = from-expr, to-expr, step-expr) body-element}

is expanded to a simplified version of:

@racketblock[
(for/sum ((id (in-range from-expr to-expr step-expr)))
 ($ body-element))]

and the product

@code{Π(id = from-expr, to-expr, step-expr) body-element}

to a simplified form of:

@racketblock[
(for/product ((id (in-range from-expr to-expr step-expr)))
 ($ body-element))]

@racket[Ʃ] and @racket[Π] forms are expanded to tail recursive loops.
If no cycles at all are made through the loop, @racket[Ʃ] returns 0 and @racket[Π] returns 1.
From, to and step are inspected before the loop is initiated.
If it is clear that an infinite loop would be produced, an error is raised.
(This may happen when the @code{step} is zero or at least one of the values @code{from}, @code{to}
and @code{step} is inexact and the absolute value of @code{from/step} or @code{to/step}
is of the order @code{1e300} or greater)

Examples:

@interaction[
(require "infix.rkt")
(define (factorial n) ($ Π(k = 2, n + 1) k))
($ factorial(0))
($ factorial(5))
($ factorial(2 + 3))]

The following example shows detection of an infinite loop.
@racket[(write k)] has been included in order to show that the infiniteness of
the loop is detected before the loop is initiated. Nothing is written

@interaction[
(require "infix.rkt")
($ Ʃ(k = 0, 1e200, 1e-200) €(begin (write k) k))]

@section{White space and delimiters}

In the above grammar a space in the right hand side of a rule indicates that
white space is required if it is not adjacent to a delimiter.
Additional white space is allowed before
and after every delimiter.
Comma’s (@code{unquote}) are used as separator in arglists.@(linebreak)
For example:

@code{($ a(b,c,d + e))} and @code{($ a(b(unquote c)(unquote d) + e))}

are transformed to

@code{(a b c (+ d e))}.

Notice the spaces and @racket[*] in for example:

@code{($ √(b ^ 2 - 4 * a * c))}

the spaces and @racket[*] cannot be omitted:

@code{($ √(b^2-4ac))} @=> @code{(sqrt b^2-4ac)}

because @racket[$] accepts every id
(not @racket[free-identifier=?] with an operator) as a variable.
Hence @code{b^2-4ac} is parsed as one singe id.
Operator @racket[*] cannot be omitted.
Allowing @racket[*] to be omitted would introduce ambiguity, for example:

@racket[($ f (x + y))] @=> @racket[(f (+ x y))] because parsed as a function or syntax call.
@linebreak[]
@racket[($ f * (x + y))] @=> @racket[(* f (+ x y))] because parsed as a multiplication.

@section{Nested $ calls}

In @racket[($ € sexpr)] the sexpr can contain calls to macro @racket[$].
These calls are transformed. For example:

@racket[($ € ($ 1 + 2))] @=> @racket[($ 1 + 2)] @=> @racket[(+ 1 2)] → @racket[3]

In @racket[($ `sexpr)] the @racket[sexpr] may contain @racket[,($ inner-expr)].
If the unquote is at appropriate nesting level,
@racket[($ inner-expr)] is transformed. For example:

@racket[($ `(1 ,($ 2 + 3)))] @=> @racket[(list 1 ($ 2 + 3))] @=> @racket[(list 1 (+ 2 3))] →
@racket[(1 5)]

@section{Square brackets and curly braces}

Depending on the values of the parameters
@racket[read-square-bracket-as-paren] and @racket[read-curly-brace-as-paren]
parentheses can be written as square brackets and/or curly braces too,
provided they match well.

@section{Method}

@racket[$] is designed particularly for arithmetic expressions with the above operators.
@racket[$] can be used for other expressions too, but it is not useful for this purpose.
The infix notation is transformed into the prefix notation of Racket, for example:

@code{($ √(b ^ 2 - 4 * a * c))} @=> @code{(sqrt (- (expt b 2) (* 4 a c)))}

Given the infix-expr, @racket[$] finds all terms and their signs.
The infix-expr is transformed to:

@racket[(- (+ pos-term pos-term ...) neg-term neg-term ...)]

where possible simplified to:

@racketblock[(+ pos-term pos-term pos-term ...)
pos-term
(- pos-term neg-term neg-term ...)
(- (+ neg-term neg-term neg-term ...))
(- neg-term)]

A term is parsed by finding the factors of the numerator and the denominator.
A factor is in the numerator c.q. the denominator if,
within and at the parentheses nesting level of the term it belongs to,
the number of preceding / is even c.q. odd.
This makes division right associative.
The term is transformed to:

@racket[(/ (* numerator numerator ...) denominator denominator ...)]

where possible simplified to:

@racketblock[(* numerator numerator numerator ...)
numerator
(/ numerator denominator denominator ...)
(/ (* denominator denominator denominator ...))
(/ denominator)]

All parsing is done with the powerfull matching capabilities of @racket[syntax-case].

@section{Provided functions and syntaxes}

@tabular[#:sep @hspace[3]
 (list
  (list @elem[@racket[infix] " " @racket[$]]
        @elem["two synonyms of the infix macro ("@racket[free-identifier=?] ")"])
  (list @elem[@racket[√]] @elem[@racket[free-identifier=?] " with " @racket[sqrt]"."])
  (list @elem[@racket[^]] @elem[@racket[free-identifier=?] " with " @racket[expt]"."])
  (list @code{Ʃ Π} "Syntaxes that can be used in infix-exprs only.")
  (list @racket[€] "Syntax that can be used in infix-exprs only.")
  (list "" "Inhibits infix→prefix transformation,")
  (list "" @elem["although " @racket[$] " may escape from this inhibition. For example:"])
  (list "" @elem[@racket[($ € ($ infix-expr))] " does the same as " @racket[($ infix-expr)]"."]))]
    
Syntax @racket[$] accepts all ids @racket[free-identifier=?] with
@racket[+ - * / √ ^ € Ʃ Π quote unquote quasiquote] as operators. Notice that Racket reads
@bold{@litchar{'}}@code{sexpr} as @code{(quote sexpr)},
@bold{@litchar{`}}@code{sexpr} as @code{(quasiquote sexpr)} and
@bold{@litchar{,}}@code{sexpr} as @code{(unquote sexpr)}.

@section{Constant propagation}

Racket’s compiler propagates constants, particularly in arithmetic expressions.
For example:

@racket[(let ((b 2)) ($ √(2 + b)))] @=> @racket[(let ((b 2)) (sqrt (+ 2 b)))]

which is compiled to @racket[2].
Therefore macro @racket[$] does not have to bother about propagation of constants.
