#lang scribble/manual

@(require scribble/eval (for-label (except-in racket set - + -> or and => = *) "forge.rkt"))

@title{Forge: A Quick Guide}

@(define forge-eval (make-base-eval))
@interaction-eval[#:eval forge-eval
                  (require "forge.rkt")]
@(define garbage-eval (make-base-eval))
@interaction-eval[#:eval garbage-eval
                  (require "forge.rkt")]
@(define garbage-eval2 (make-base-eval))
@interaction-eval[#:eval garbage-eval2
                  (require "forge.rkt")]
@(define garbage-eval3 (make-base-eval))
@interaction-eval[#:eval garbage-eval3
                  (require "forge.rkt")]
@(define garbage-eval4 (make-base-eval))
@interaction-eval[#:eval garbage-eval4
                  (require "forge.rkt")]
@(define garbage-eval5 (make-base-eval))
@interaction-eval[#:eval garbage-eval5
                  (require "forge.rkt")]
@defmodulelang["forge.rkt"]

Welcome to Forge!  Forge is a language built for teaching formal methods and model checking.  Forge is very much a work in progress, and is notably missing both integers and type hierarchy for sigs - rest assured, these features, and many more, are on their way!

Forge extends the @hyperlink["https://docs.racket-lang.org/ocelot/"]{Ocelot} package to create first-order relational logic models.  Forge differs from Ocelot in that it provides:

@itemlist[@item{Alloy-like semantics for universe declaration (i.e. implicitly defining a universe of discourse in terms of sigs and bounds)}
          @item{A built-in visualizer and evaluator}
          @item{Support for KodKod integers (coming soon)}
          @item{Built-in semantics for state (coming soon)}
          @item{Additional teaching tools like language levels and different interaction modes (coming soon)}]

@local-table-of-contents[]

@section{The Language}

To begin working with forge, make sure to begin your file with the line:

@codeblock|{#lang s-exp forge}|

@subsection{Atoms, Sigs, and Relations}

Forge is a tool that is used to build models in relational logic.  As such, it generates instances that are made of atoms, which represent particulars in a universe of discourse, and relations, which are sets of tuples of atoms.  The forge language lets us specify what sorts of atoms and what relations exist in the universe of our model.

The primary construct in forge is the sig.  A sig represents a type of atom that can appear in the universe.

To declare a sig, use the declare-sig or declare-one-sig form:

@defform*[((declare-sig name) (declare-sig name ((relation field-sig ...) ...)))]{Adds a sig called @racket[name] to the universe.  If any relations are specified, adds them to the spec.  The type of a declared relation is (name -> field-sig0 -> field-sig1 ...).}

For example, we might use forge to model a simple directed graph.  To do this, we'd specify a sig to represent vertices of the graph and a relation to represent edges.

@examples[#:eval forge-eval
          (declare-sig vertex ((edges vertex)))
          vertex
          edges]

This declaration adds two relations to the spec - the arity 1 relation @racket[vertex], which contains exactly the atoms of type vertex in the universe, and the arity 2 relation @racket[edges], which contains some number of pairs of vertices, or is empty.

We could also envision a more complicated example with more relations or relations of higher arity, like a spec to model a graph of references stored on the heap that changes over time.  One way to approach modeling such a scenario could be to write:

@examples[#:eval forge-eval
          (declare-sig heap-cell)
          (declare-sig state ((references heap-cell heap-cell) (allocated heap-cell)))
          sigs]


@defthing[sigs list?]{A list of all the declared sigs in a spec.}

@defform*[((declare-one-sig name) (declare-one-sig name ((relation field-sig ...) ...)))]{Like @racket[declare-sig], but also implicitly bounds the sig such that each instance contains exactly one atom of type @racket[name].}


@subsection{The Run Statement}

After we've defined the sigs and relations that make up our spec, we can run it!

@defform*[((run name) (run name constraint) (run name ((sig lower upper) ...)) (run name constraint ((sig lower upper) ...)))]

Calling @racket[run] will automatically launch the visualizer in a web browser.  Calling run multiple times will launch multiple instances of the visualizer as browser tabs.

Each run statement has a name - this is so we can include multiple run statements in a spec and differentiate them.  Names should be unique strings.

@examples[#:eval garbage-eval
          (declare-sig vertex ((edges vertex))) (run "My first spec")]

Optionally, it is often useful to specify bounds or constraints with a run statement.

@subsubsection{Run with Bounds}

By default, a universe of discourse contains four potential atoms of any given sig, so in any given instance there are zero to four atoms of each type (other than one-sigs, which have exactly one atom).  Often, there is cause to have a different upper and lower bound on a sig.  These bounds can be specified at the run statement.

@examples[#:eval garbage-eval2
          (declare-sig vertex ((edges vertex))) (run "My first bounded spec" ((vertex 3 5)))]

This run statement finds instances of a graph with no fewer than three and no more than five vertices.  Any sigs for which a bound is not specified use the default bound.

@examples[#:eval garbage-eval3
          (declare-sig cow) (declare-sig horse) (declare-sig sheep) (run "Barnyard" ((cow 3 3) (horse 2 5)))]

This run statement finds instances with exactly 3 cows, between 2 and 5 horses, and between 0 and 4 sheep.

@subsubsection{Run with Constraints}

Run statements can also be called with constraints.

@examples[#:eval garbage-eval4
          (declare-sig vertex ((edges vertex))) (run "Directed Acyclic Graph" (no (& iden (^ edges))) ((vertex 3 5)))]

The above run statement requires that the edges relation is acyclic.  For more information on how to write constraints, see the section on Constraints.

@subsection{Constraints}

In addition to defining what sigs and relations can exist in a model, we need to be able to assert thins about properties of a model.  Forge provides a language for specifying such constraints in terms of expressions and formulas.

@subsubsection{Expressions}

Forge uses Ocelot's language for representing expressions and formulas.

Expressions represent symbolic relations.  

@defproc[(+ [r0 expr?] [r expr?] ...) expr?]{Produces the union of the given relations.}

@defproc[(& [r0 expr?] [r expr?] ...) expr?]{Produces the intersection of the given relations.}

@defproc[(- [r0 expr?] [r expr?] ...) expr?]{Produces the difference of the given relations.}

@defproc[(-> [r0 expr?] [r expr?] ...) expr?]{Produces the cartesian product of the given relations.}'

@defproc[(join [r0 expr?] [r1 expr?]) expr?]{Produces the relational join of the given relations.  The output relation has arity = (arity of r0) + (arity of r1) - 2, and contains tuples created by appending members of r0 and r1 where the last element of the member of r0 matches the first element of r1.  So, if x contains (a b) and (c b) and y contains (b d) and (e f), @racket[(join x y)] contains (a d) and (c d)}

@defproc[(~ [r expr?]) expr?]{Produces the transpose of the given relation.}

@defproc[(^ [r expr?]) expr?]{Produces the transitive closure of the given relation.}

@defproc[(* [r expr?]) expr?]{Produces the reflexive transitive closure of the given relation.}

@defthing[none expr?]{An empty relation.}

@defthing[iden expr?]{An arity 2 relation containing every atom in the universe related to itself.  So, for each atom x, iden containx the tuple (x x).}

@defthing[univ expr?]{An arity 1 relation containing every atom in the universe as a singleton.}

@defform[(set ([id domain] ...) formula)]{A relation with arity equal to the number of [id domain] declarations.  Each domain is an arity 1 relaion.  A tuple (x1 ... xn) is in the relation if and only if x1 is in the first domain, x2 is in the second, etc. and formula evaluates to true when x1 ... xn are bound to id1 ... idn.}

@subsubsection{Formulas}

Formulas evaluate to true of false, and are used to place constraints on relations.

@defproc[(in [r0 expr?] [r1 expr?]) formula?]{Produces a formula that's true iff r0 is a subset of r1}

@defproc[(= [r0 expr?] [r1 expr?]) formula?]{Produces a formula that's true iff r0 and r1 contain exactly the same tuples}

@defproc[(and [f0 formula?] [f1 formula?] ...) formula?]{Produces a formula that's true iff all the arguments are true}

@defproc[(or [f0 formula?] [f1 formula?] ...) formula?]{Produces a formula that's true iff at least one argument is true}

@defproc[(=> [f0 formula?] [f1 formula?]) formula?]{Produces a formula that's true iff f0 implies f1}

@defproc[(! [f formula?]) formula?]{Produces a formula that's true iff f is false}

@defproc[(some [r expr?]) formula?]{Produces a formula that's true iff r is non-empty}

@defproc[(no [r expr?]) formula?]{Produces a formula that's true iff r is empty}

@defproc[(one [r expr?]) formula?]{Produces a formula that's true iff r has exactly one element}

@defproc[(lone [r expr?]) formula?]{Produces a formula that's true iff r has zero or one elements}

@subsubsection{Quantifiers}

Quantifiers are a type of formula that check each member of a specified set for some property.

@defform[(all ([id domain] ...) body-formula)]{Where each domain is an arity 1-relation and body-formula is a formula. Produces a formula that's true iff, for every binding of each id to an element of its domain, body-formula is true.}

@defform[(some ([id domain] ...) body-formula)]{Where each domain is an arity 1-relation and body-formula is a formula. Produces a formula that's true iff, for at least one binding of each id to an element of its domain, body-formula is true.}

@defform[(one ([id domain] ...) body-formula)]{Where each domain is an arity 1-relation and body-formula is a formula. Produces a formula that's true iff, for exactly one binding of each id to an element of its domain, body-formula is true.}

@defform[(lone ([id domain] ...) body-formula)]{Where each domain is an arity 1-relation and body-formula is a formula. Produces a formula that's true iff, for at most one binding of each id to an element of its domain, body-formula is true.}

@subsubsection{Facts}

@defform[(fact formula)]{Requires that formula be true in every instance generated by every @racket[run] statement called after this fact declaration.}

@subsubsection{Predicates}

@defform*[((pred name formula) (pred (name vars ...) formula))]{Binds name to formula but doesn't implicitly require it to be true.  If the second form is used, binds name to a function that takes arguments vars ... evaluates formula with each argument bound the the apropriate variable.}

@subsection{Putting it all together}

Here's an example of a simple Forge spec we might write to define an undirected tree.

@examples[#:eval garbage-eval5
          (declare-sig vertex ((edges vertex)))
          (fact (= edges (~ edges)))
          (fact (in (-> vertex vertex) (* edges)))
          (fact (all ([v1 vertex] [v2 vertex])
                     (=> (in (+ (-> v1 v2) (-> v2 v1)) edges) (! (in (+ (-> v1 v2) (-> v2 v1)) (^ (- edges (+ (-> v1 v2) (-> v2 v1)))))))))
          (run "Undirected Tree" ((vertex 0 5)))]