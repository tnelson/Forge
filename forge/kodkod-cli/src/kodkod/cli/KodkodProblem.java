/*
 * Kodkod -- Copyright (c) 2005-present, Emina Torlak
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
 * THE SOFTWARE.
 */
package kodkod.cli;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.logging.Handler;
import java.util.logging.Level;
import java.util.logging.Logger;

import kodkod.ast.Formula;
import kodkod.ast.Relation;
import kodkod.engine.Evaluator;
import kodkod.engine.IncrementalSolver;
import kodkod.engine.Solution;
import kodkod.engine.Solver;
// import kodkod.engine.bddlab.BDDSolverFactory; Removed for Pardinus
import kodkod.engine.config.Options;
import kodkod.engine.satlab.SATFactory;
import kodkod.engine.ucore.RCEStrategy;
import kodkod.instance.*;
import kodkod.util.ints.IntSet;
import kodkod.util.ints.Ints;

import org.parboiled.errors.ActionException;

/**
 * Provides a set of actions invoked by the {@link KodkodParser Kodkod parser} to specify
 * and solve relational satisfiability problems.  A {@link KodkodProblem} instance represents either
 * a complete or an incremental specification of a Kodkod problem.
 *
 * <p>A complete specification consists of {@link Options}, {@link Bounds}, a set of
 * asserted {@link Formula formulas}, and a problem {@link DefEnv definition environment}.
 * It represents a stand-alone problem description in which each asserted formulas refers to
 * the relations bound the problem's own {@link Bounds} instance.  Complete specifications
 * are solved with a {@link Solver standard solver}.</p>
 *
 * <p>An incremental specification also consists of {@link Options}, {@link Bounds}, a set of
 * asserted {@link Formula formulas}, and a problem {@link DefEnv definition environment}.
 * But unlike a complete specification, an incremental specification is composed of a sequence
 * of partial specifications.  The first specification in this sequence is a stand-alone problem
 * description. But each subsequent specification may assert formulas that refer to
 * relations that are not bound in its own {@link Bounds} instance, as long as they appear in previously
 * specified {@link Bounds}.</p>
 *
 * <p>Incrementally specified problems are solved with an {@link IncrementalSolver incremental solver}, and
 * the sequence of partial specification describing such a problem must satisfy the requirements
 * imposed by the incremental solving API. For example, the {@link Options},
 * {@link #declareUniverse(int) universe} and {@link #declareInts(List) integer bounds} can only
 * be configured/declared in the initial specification; they may not be extended or modified in
 * any way by subsequent partial specifications.  The {@link IncrementalSolver} documentation
 * describes the remaining restrictions in detail. </p>
 *
 * <p>All specification action methods return <code>true</code> on success and
 * throw an  {@link ActionException action exception} on failure. References to a problem
 * specification {@code p} should be discarded by client code after a reference to a new specification
 * has been obtained from {@code p} by calling {@link #extend()} or {@link #clear()}.  </p>
 *
 * @specfield prev: lone {@link KodkodProblem}
 * @specfield options: {@link Options}
 * @specfield bounds: lone {@link Bounds}
 * @specfield asserts: set {@link Formula} 	// top-level formulas
 * @specfield env: {@link DefEnv} 			// definitions for this problem
 * @specfield maxSolutions: long			// maximum number of solutions that may be produced for this problem
 *
 * @invariant one this.*prev.options && one this.*prev.bounds.universe
 * @invariant some this.prev => no this.intBound && (no this.relations & this.^prev.bounds.relations) && this.partial() && this.incremental()
 * @invariant all p: this.^prev | p.incremental()
 * @invariant (this.*prev.asserts.*components & Relation) in this.*prev.bounds.relations
 * @invariant this.*prev.bounds.relations = this.env['r']
 *
 * @author Emina Torlak
 */
 public abstract class KodkodProblem {
	private long buildTimeMillis = -1, coreTimeMillis = -1, maxSolutions = 1;
	private Options options;
	private final DefEnv env;
	private final List<Formula> asserts;
	private Bounds bounds;

	/**
	 * Creates a partial problem with the given state.
	 * @requires prev.incremental()
	 * @ensures this.bounds' = bounds && no this.asserts' && this.env' = env && this.options' = options && this.maxSolutions' = maxSolutions
	 */
	private KodkodProblem(DefEnv env, Bounds bounds, Options options, long maxSolutions) {
		this.env = env;
		this.bounds = bounds;
		this.options = options;
        // System.out.println("HERE");
        // this.options.setSymmetryBreaking(20);
        // System.out.println("HERE2");
		this.asserts = new ArrayList<>();
		this.maxSolutions = maxSolutions;
	}

	/**
	 * Creates a problem with {@link KodkodFactory#baseOptions()}.
	 * @ensures this(new DefEnv(), null, KodkodFactory.baseOptions(), 1)
	 */
	private KodkodProblem() {
		this(new DefEnv(), null, KodkodFactory.baseOptions(), 1);
	}

	/**
	 * Returns an empty {@link KodkodProblem} that can be used to
	 * construct a complete, stand-alone specification of a new relational
	 * satisfiability problem. The specification will be initialized with
	 * {@link KodkodFactory#baseOptions()}.
	 * with the given options.
	 * @return some p: {@link KodkodProblem} |
	 * 			no p.prev && !p.incremental() && p.options = KodkodFactory.baseOptions() &&
	 * 			no p.bounds && no p.asserts && p.env = new DefEnv() && p.maxSolutions = 1
	 */
	public static KodkodProblem complete() { return new KodkodProblem.Complete(); }

	/**
	 * Returns an empty {@link KodkodProblem} that can be used to
	 * construct the first (stand-alone) component of an incremental
	 * specification of a new relational  satisfiability problem.
	 * The specification will be initialized with
	 * {@link KodkodFactory#baseOptions()}.
	 * @return some p: {@link KodkodProblem} |
	 * 			no p.prev && p.incremental() && p.options = KodkodFactory.baseOptions() &&
	 * 			no p.bounds && no p.asserts && p.env = new DefEnv() && p.maxSolutions = 1
	 */
	public static KodkodProblem incremental() { return new KodkodProblem.Incremental(); }

	/**
	 * Returns a new Stepper problem!
	 */
	public static KodkodProblem stepper() { return new KodkodProblem.Stepper(); }

	/**
	 * Returns an empty partial {@link KodkodProblem} instance that can be used to
	 * extend the incremental specification composed of the sequence of
	 * problems ending with this {@link KodkodProblem}.  Throws {@link ActionException}
	 * if this is not an {@link #incremental() incremental} specification.
	 * @requires some this.bounds && this.incremental()
	 * @return some p: {@link KodkodProblem} |
	 * 			p.prev = this && p.incremental() && p.options = this.options &&
	 * 			p.bounds = new Bounds(this.bounds.universe) && no p.asserts &&
	 * 			no p.env.parent && p.env.defs = this.env.defs &&
	 * 			p.maxSolutions = this.maxSolutions
	 */
	public abstract KodkodProblem extend();

	/**
	 * Returns an empty {@link KodkodProblem} that can be used to construct
	 * a stand-alone specification of a new relational satisfiability problem.
	 * The returned instance is like this problem in the following sense:  it is
	 * incremental iff this is incremental; it is initialized with {@code this.options};
	 * and it has {@code this.maxSolutions} as its limit on the maximum number of solutions.
	 * @return some p: {@link KodkodProblem} |
	 * 			no p.prev && (p.incremental() iff prototype.incremental()) &&
	 * 			p.options = prototype.options && p.maxSolutions = prototype.maxSolutions &&
	 * 			no p.bounds && no p.asserts && p.env = new DefEnv()
	 */
	public abstract KodkodProblem clear();

	/**
	 * Solves this and outputs the resulting solution(s) to the given {@code out}.
	 * The number of produced solutions is bound above by {@code this.maxSolutions}.
	 * After solving the problem, this method returns a new empty {@link KodkodProblem}
	 * that can be used either to extend the specification comprising this problem with
	 * additional constraints, if this is an incremental {@link KodkodProblem}, or to
	 * construct a new complete specification otherwise.
	 * @requires some this.problem.bounds
	 * @ensures some S: set SOLUTIONS(Formula.and(this.*prev.asserts), this.*prev.bounds, this.options) |
	 * 				(all s: S | out.writeSolution(s, this) ) &&  0 < #S <= this.maxSolutions
	 * @return !this.isIncremental() => this.clear() else this.extend()
	 * @throws ActionException any solving pre-conditions are violated
	 **/
	public abstract KodkodProblem solve(KodkodOutput out);

	// public final KodkodProblem solveNext(KodkodOutput out){
	// 	if (!KodkodServer.lastModelAvailable){
	// 		out.writeCore(null, null);
	// 	} else if (!(KodkodServer.lastModel.hasNext())){
	// 		out.writeCore(null, null);
	// 	} else {
	// 		Solution sol = KodkodServer.lastModel.next();
	// 		if (sol.sat()){
	// 			out.writeInstance(sol.instance(), KodkodServer.lastRDefs);
	// 		} else {
	// 			out.writeCore(null, null);
	// 		}
	// 	}
	// 	return clear();
	// }

	/**
	 * Returns the problem definitions environment.  The relation ('r') register in the
	 * returned environment should not be modified by client code, other than through
	 * the {@link #declareRelation(String, TupleSet, TupleSet)} method.  The variable ('v')
	 * register of the problem environment is unmodifiable.
	 * @return this.env
	 */
	public final DefEnv env() { return env; }

	/**
	 * Returns {@code this.bounds}.  The bounds should not be modified by client
	 * code during the lifetime of this {@link KodkodProblem} except through the {@link #declareInts(List)} or
	 * {@link #declareRelation(int, TupleSet, TupleSet)} methods.
	 * @return this.bounds
	 */
	public final Bounds bounds() { return bounds; }

	/**
	 * Returns the union of all bounds in {@code this.*prev}.
	 * @return let prevs = this.*prev.bounds |
	 * 			some b: Bounds | b.universe = prevs.universe && b.intBound = prevs.intBound &&
	 *            b.lowerBound = prevs.lowerBound && b.upperBound = prevs.upperBound
	 */
	public Bounds allBounds() { return bounds; }

	/**
	 * Returns {@code this.options}.  The options should not be modified by client
	 * code during the lifetime of this {@link KodkodProblem} except through the {@link #configureOptions(Options)} method.
	 * @return this.options
	 */
	public final Options options() { return options; }

	/**
	 * Returns the conjunction of {@code this.asserts}.
	 * @return Formula.and(this.asserts)
	 */
	public final Formula asserts() { return Formula.and(asserts); }

	/**
	 * Returns the time in milliseconds that had elapsed between the calls to
	 * {@link #startBuild()} and {@link #endBuild()} on this instance.
	 * If the calls were not made, or if they were not made as specified by {@link #startBuild()} and
	 * {@link #endBuild()}, the value returned by this method is undefined.
	 * @return problem building (parsing) time, in milliseconds
	 */
	public final long buildTimeMillis() { return buildTimeMillis; }

	/**
	 * Returns the time in milliseconds taken to minimize the unsat core during
	 * the last call to {@link #solve(KodkodOutput)} on this instance. If no
	 * call to solve was made, or if it was made and no core minimization was
	 * performed, the value returned by this method is undefined.
	 * @return core extraction and minimization time, in milliseconds
	 */
	public final long coreTimeMillis() { return coreTimeMillis; }

	/**
	 * Returns true iff this is a (partial or stand-alone) component
	 * of an incremental specification.
	 * @return true iff this is a (partial or stand-alone) component
	 * of an incremental specification.
	 */
	public abstract boolean isIncremental();

	/**
	 * Returns true iff this is a partial specification.
	 * @return some this.prev
	 */
	public abstract boolean isPartial() ;

	/**
	 * Returns true iff this is a Stepper specification (solved or unsolved).
	 * @return some this.prev
	 */
	public boolean isStepper(){
		return false;
	}

	/**
	 * Returns true iff this is a solved Stepper specification.
	 * @return some this.prev
	 */
	public boolean isSolved(){
		return false;
	}

	/**
	 * An action used to inform this problem instance that its state
	 * is about to be populated.  This method assumes, but does not check,
	 * that no other action methods have yet been called on this problem instance.
	 * This method should be called exactly once, prior to all other actions
	 * being performed.
	 * @return true
	 */
	final boolean startBuild() {
		buildTimeMillis = System.currentTimeMillis();
		return true;
	}

	/**
	 * An action used to inform a this problem instance that its state
	 * has been populated.  This method assumes, but does not check,
	 * that no other action methods will ever be called on this problem instance.
	 * This method should be called exactly once, after all other actions have
	 * been performed.
	 * @return true
	 */
	final boolean endBuild() {
		buildTimeMillis = System.currentTimeMillis() - buildTimeMillis;
		return true;
	}

	/**
	 * Returns {@code this.maxSolutions}.
	 * @return this.maxSolutions
	 */
	public final long maxSolutions() { return maxSolutions; }

	/**
	 * Sets {@code this.maxSolutions} to the given value.
	 * @requires maxSolutions > 0
	 * @return true
	 */
	final boolean setMaxSolutions(long maxSolutions) {
		if (maxSolutions < 1)
			throw new ActionException("Expected maxSolutions > 0, given " + maxSolutions);
		this.maxSolutions = maxSolutions;
		return true;
	}

	/**
	 * Sets the logging level of {@code this.options.reporter.logger} to the specified value.
	 * Calling this method on a partial {@link KodkodProblem} will result in an {@link ActionException}.
	 * @requires no this.prev
	 * @ensures this.options.reporter.logger.level' = level
	 */
	boolean setVerbosity(Level level) {
		final Logger logger = ((KodkodReporter)options.reporter()).logger();
		logger.setLevel(level);
		for(Handler h : logger.getHandlers()) {
			h.setLevel(level);
		}
		return true;
	}

	/**
	 * Sets {@code this.options.bitwidth} to the specified value. Calling this
	 * method on a partial {@link KodkodProblem} will result in an {@link ActionException}.
	 * @requires no this.prev
	 * @ensures this.options.bitwidth' = bitwidth
	 */
	boolean setBitwidth(int bitwidth) {
		try {
			options.setBitwidth(bitwidth);
		} catch (IllegalArgumentException ex) {
			throw new ActionException(ex.getMessage(), ex); // wrap
		}
		return true;
	}

	boolean setSB(int sb) {
		try {
			options.setSymmetryBreaking(sb);
		} catch (IllegalArgumentException ex) {
			throw new ActionException(ex.getMessage(), ex); // wrap
		}
		return true;
	}

	boolean setSkolemDepth(int sd) {
		try {
			options.setSkolemDepth(sd);
		} catch (IllegalArgumentException ex) {
			throw new ActionException(ex.getMessage(), ex); // wrap
		}
		return true;
	}
	boolean setCoreGranularity(int gran) {
		try {
			options.setCoreGranularity(gran);
		} catch (IllegalArgumentException ex) {
			throw new ActionException(ex.getMessage(), ex); // wrap
		}
		return true;
	}
	boolean setLogTranslation(int lt) {
		try {
			options.setLogTranslation(lt);
		} catch (IllegalArgumentException ex) {
			throw new ActionException(ex.getMessage(), ex); // wrap
		}
		return true;
	}


	/**
	 * Sets {@code this.options.solver} to the specified solver factory.
	 * Calling this method on a partial {@link KodkodProblem} will result in an {@link ActionException}.
	 * An {@link ActionException} will also be thrown if this method is called
	 * with a non-incremental {@link SATFactory} on an incremental problem specification.
	 * @requires no this.prev
	 * @ensures this.options.solver' = solver
	 * Changed for Pardinus
	 */
	boolean setSolver(SATFactory solver) {
		if (SATFactory.available(solver)) {
			options.setSolver(solver);
			return true;
		} else {
			throw new ActionException(solver.toString() + " is not available on this system. Searched " + System.getProperty("java.library.path"));
		}
	}

	/**
	 * Same as {@link #setBddSolver(BDDSolverFactory, boolean)} with the default parameter of false
	 * for distinctPathsOnly.
	 */ 
	// boolean setBddSolver(BDDSolverFactory solver) {
	// 	return setBddSolver(solver, false);
	// } Removed for Pardinus

	/**
	 * Sets {@code this.options.solver} to use the specified bdd solver and tells whether
	 * to generate one solution per distinct path in the bdd or for all solutions.
	 * @param solver The solver to use
	 * @param distinctPathsOnly whether to generate only one solution per path through the bdd.
	 * @return true
	 */
	// boolean setBddSolver(BDDSolverFactory solver, boolean distinctPathsOnly) {
	// 	options.setBddSolver(solver, distinctPathsOnly);
	// 	return true;
	// } Removed for Pardinus

	/**
	 * Modifies {@code this.options} so as to enable or disable minimal core extraction.
	 * Calling this method on a partial {@link KodkodProblem} will result in an {@link ActionException}.
	 * An {@link ActionException} will also be thrown if this method is called with <code>true</code> on
	 * an incremental problem specification.
	 * @requires no this.prev
	 * @ensures enable =>
	 * 				(this.options.setLogTranslation(1) && this.options.setCoreGranularity(0) &&
	 * 				 this.ooptions.setSatSolver(SATFactory.MiniSatProver)) else
	 *              (this.options.setLogTranslation(0) && this.options.setCoreGranularity(0))
	 **/
	// Just set solver, LT, and CG separately.
	@Deprecated
	boolean setCoreExtraction(boolean enable) {
		try {
			if (enable) {
				if (!SATFactory.available(SATFactory.MiniSatProver)) {
					throw new ActionException("Cannot enable core extraction since no proof-logging solver is available on this system.");
				}
				options.setLogTranslation(1);
				options.setCoreGranularity(0);
				// Changed for Pardinus
				options.setSolver(SATFactory.MiniSatProver);
			} else {
				options.setLogTranslation(0);
				options.setCoreGranularity(0);
			}
		} catch (IllegalArgumentException ex) {
			throw new ActionException(ex.getMessage(), ex); // wrap
		}
		return true;
	}


	/**
	 * Sets {@code this.bounds} to an empty {@link Bounds} over a freshly
	 * created universe of the given size.  The created universe consists of
	 * {@link Integer integers} in the range {@code [0..size)}.  Calling this
	 * method after bounds have already been defined will result in an {@link ActionException}.
	 * @requires no this.prev
	 * @requires no this.bounds
	 * @ensures this.bounds'.universe.atoms = [0..size)<:iden
	 * @ensures no this.bounds'.intBound && no this.bounds'.lowerBound && no this.bounds'.upperBound
	 */
	final boolean declareUniverse(int size) {
		if (bounds != null)
			throw new ActionException("Universe already defined.");
		try {
			final List<Integer> atoms = new ArrayList<Integer>(size);
			for(int i = 0; i < size; i++)
				atoms.add(i);
			final Universe universe = new Universe(atoms);
			bounds = new Bounds(universe);
		} catch (IllegalArgumentException ex) {
			throw new ActionException(ex.getMessage(), ex); // wrap
		}
		return true;
	}

	/**
	 * Sets the interpretation of integer atoms in {@code this.bounds} by
	 * performing {@code this.bounds.boundExactly(ints.get(i), ints.get(i+1))} on
	 * each even {@code i} in {@code [0..ints.size())}.  Assumes that no integers
	 * have been already bound in  {@code this.bounds}.  Calling this
	 * method after ints have already been defined, or calling it on a partial {@link KodkodProblem},
	 * will result in an {@link ActionException}.
	 * @requires no this.prev
	 * @requires some this.bounds
	 * @requires no bounds.ints()
	 * @requires ints.size() % 2 = 0
	 * @requires all i: [0..ints.size()) | i % 2 = 1 => ints.get(i) in bounds.universe.atoms[int]
	 * @ensures all i: [0..ints.size()) | i % 2 = 0 => this.bounds.boundExactly(ints.get(i), this.bounds.universe.factory.setOf(ints.get(i+1))
	 */
	boolean declareInts(List<Integer> ints) {
		if (!bounds.intBounds().isEmpty())
			throw new ActionException("Integer bounds already defined.");
		try {
			final Universe univ = bounds.universe();
			final TupleFactory f = univ.factory();
			final IntSet intAtomIndices = Ints.bestSet(univ.size());
			for(int idx = 0, size = ints.size(); idx < size; idx += 2) {
				final int i = ints.get(idx);
				final Integer atom = ints.get(idx+1);

				final TupleSet ibound = bounds.exactBound(i);
				if (ibound != null) {
					throw new ActionException("Cannot bind the integer " + i + "to atom " + atom +
							" since it is already bound to atom " + ibound.indexView().min() +".");
				}
				if (!intAtomIndices.add(univ.index(atom)))
					throw new ActionException("Cannot bind the integer " + i + " as atom " + atom +
							" since a different integer is already bound to this atom.");

				bounds.boundExactly(i, f.setOf(atom));
			}
		} catch (IllegalArgumentException ex) {
			throw new ActionException(ex.getMessage(), ex); // wrap
		}

		return true;
	}

	/**
	 * Creates a set of relations with the given indices, adds them to {@code this.env.defs['r']},
	 * and bounds them in {@code this.bounds} using the given lower/upper bounds.
	 * @requires no this.env.defs['r'].def[idxs[int]]
	 * @requires #idxs[int] = idxs.size()
	 * @requires lower.arity = upper.arity
	 * @requires some this.bounds
	 * @ensures let R = this.bounds.relations' - this.bounds.relations |
	 * 			  #R = idxs.size() &&
	 * 			  (all idx: idxs[int] | some r: R |
	 * 				r.name.equals("r"+idx)) &&
	 *            	r.arity() = lower.arity  &&
	 *            	this.bounds.lowerBound'[r] = lower && this.bounds.upperBound'[r] = upper &&
	 *            	this.env.def('r', idx, r))
	 */
	final boolean declareRelations(List<Integer> idxs, TupleSet lower, TupleSet upper) {
		try {
			for(Integer idx : idxs) {
				final Relation r = Relation.nary("r"+idx, lower.arity());
				env.def('r', idx, r);
				bounds.bound(r, lower, upper);
				declaredRelation(r, lower, upper);
			}
		} catch (RuntimeException ex) {
			throw new ActionException(ex.getMessage(), ex); // wrap
		}
		return true;
	}

	/**
	 * Hook for subclasses to intercept relation creation calls.
	 * It is called by {@linkplain #declaredRelation(Relation, TupleSet, TupleSet)} whenever it adds a new
	 * relation to {@code this.bounds}. Does nothing
	 * by default.
	 */
	void declaredRelation(Relation r, TupleSet lower, TupleSet upper) { }

	/**
	 * Adds the given formula to {@code this.asserts}.
	 * @return this.asserts.add(formula)
	 */
	final boolean assertFormula(Formula formula) {
		return asserts.add(formula);
	}

	/**
	 *
	 */
	boolean evaluate(kodkod.ast.Expression expression) {
		throw new ActionException("Can only evaluate for stepper problems.");
	}

	/**
     *
	 */
	boolean evaluate(kodkod.ast.IntExpression expression) {
		throw new ActionException("Can only evaluate for stepper problems.");
	}

	/**
	 *
	 */
	boolean evaluate(Formula formula) {
		throw new ActionException("Can only evaluate for stepper problems.");
	}

	/**
	 * Prints the given solution for this problem to {@code out}, after first minimizing its core, if any.
	 * @requires sol in SOLUTIONS(Formula.and(this.*prev.asserts), this.*prev.bounds, this.options)
	 * @ensures some sol.proof => sol.proof.minimize(new RCEStrategy(sol.proof.log))
	 * @ensures out.print(sol, this)
	 * */
	final void write(KodkodOutput out, Solution sol) {
		if (sol.proof() != null) { // core extraction was enabled
			coreTimeMillis = System.currentTimeMillis();
			sol.proof().minimize(new RCEStrategy(sol.proof().log()));
			coreTimeMillis = System.currentTimeMillis() - coreTimeMillis;
		}
		out.writeSolution(sol, this);
	}

	/**
	 * Prints at most {@code this.maxSolutions} solutions from the given iterator to {@code out}.
	 * The first solution is always printed, using {@link #print(Solution)}.  If the first solution is SAT, then only
	 * subsequent SAT solutions are printed; the last, unsatisfiable, solution is discarded.
	 * @requires (sols.first).*(sols.next) in SOLUTIONS(Formula.and(this.*prev.asserts), this.*prev.bounds, this.options)
	 * @ensures some last: (sols.first).*(sols.next) |
	 * 				let S = last.*~(sols.next) | 0 < #S <= this.maxSolutions && all s: S | this.output(out, sol)
	 **/
	final void write(KodkodOutput out, Iterator<Solution> sols) {
		Solution sol = sols.next();
		write(out, sol); // print the first solution
		if (sol.sat()) { // there has to be at least one more, possibly unsat, solution after the first sat solution
			long atMost = maxSolutions;
			while(--atMost > 0 && (sol = sols.next()).sat()) { // don't print the last unsat solution
				write(out, sol);
			}
		}
	}

    final void writeUnsat(KodkodOutput out, Solution sol){
        out.writeUnsat(sol, this);
    }


	// TODO: allow multiple Stepper problems.
	// possibly by having Solve() return a new Stepper? or maybe after clear...


	// AH. realization. the other problem classes have good reason to return new objects;
	// they need to clear the bounds and asserts! However, for Stepper, we only need to return
	// new objects when we transition to a solved stepper. A solved stepper can return itself.
	private static final class Stepper extends KodkodProblem {
		private final Solver solver;
		private boolean issolved = false;
		private Solution lastSol;
        private int iteration = -1;
        private boolean unsat = false;
        private Evaluator evaluator = null;

		// Used to print new solutions from the first solved model.
		private Iterator<Solution> solutions;

		Stepper() {
			this.solver = new Solver(super.options);
                // System.err.println(super.options);
			super.maxSolutions = -1;	// maxSolutions has no meaning for Steppers.
		}

		// makes a solved stepper with the given solutions.
		private Stepper(Stepper prototype, Iterator<Solution> solutions){
			super(prototype.env(), null, null, -1);
			assert (solutions != null);
			this.solver = null;
			this.issolved = true;
			this.solutions = solutions;
            assert (this.iteration == -1);
		}

		public boolean isIncremental() { return false; }
		public boolean isPartial() { return false; }
		public boolean isStepper() { return true; }
		public boolean isSolved() { return issolved	; }

		public KodkodProblem extend() { throw new ActionException("Cannot extend a non-incremental specification."); }
		public KodkodProblem clear() {
			if (solver!=null)
				solver.free();
			return new Stepper();
		}

		public KodkodProblem solve(KodkodOutput out) {
			if (isSolved()){
                assert (this.iteration >= 0);
                this.iteration++;

                if (this.unsat){
                    assert(lastSol != null);
                    writeUnsat(out, lastSol);
                    return this;
                }

				if (solutions.hasNext()){
					Solution sol = solutions.next();

                    // If our first solution is also our last, then the spec
                    // is unsatisfiable, and we say so.
                    if ((this.iteration == 0) && !(solutions.hasNext())){
                        this.unsat = true;
                        lastSol = sol;
                        writeUnsat(out, lastSol);
                        return this;
                    } else {
    					write(out, sol);
    					lastSol = sol;
    					evaluator = new Evaluator(sol.instance()); // TODO: add options
    					return this;
                    }
				}

                // If we finished our list of solutions, we just keep repeating the last solutions
                // found, which will be no-more-instances.
                else {
					assert(lastSol != null);
					write(out, lastSol);
					return this;
				}
			}

			try {
                Iterator<Solution> solved = solver.solveAll(asserts(), bounds());
				return new Stepper(this, solved).solve(out);
			} catch (RuntimeException ex){
				ex.printStackTrace();
				throw new ActionException(ex.getMessage(), ex);
			}
		}

		public boolean evaluate(kodkod.ast.Expression expression) {
			TupleSet ts = evaluator.evaluate(expression);
			StringBuilder str = new StringBuilder();
			str.append("{");
			for(Tuple t : ts) {
				str.append("(");
				str.append(t.atomIndex(0));
				for(int idx = 1; idx < ts.arity(); idx++) {
					str.append(" ").append(t.atomIndex(idx));
				}
				str.append(")");
			}
			str.append("}");
			System.out.println("(evaluated :expression " + str + ")");

			return true;
		}
		public boolean evaluate(kodkod.ast.IntExpression expression) {
			System.out.println("(evaluated :int-expression " + evaluator.evaluate(expression) + ")");
			return true;
		}
		public boolean evaluate(Formula formula) {
			System.out.println("(evaluated :formula " + evaluator.evaluate(formula) + ")");
			return true;
		}
	}

	/**
	 * Implements a complete specification of a Kodkod problem.
	 * @author Emina Torlak
	 */
	private static final class Complete extends KodkodProblem {
		private final Solver solver;

		Complete() {  this.solver = new Solver(super.options); }
		Complete(Complete prototype) {
			super(new DefEnv(), null, prototype.options(), prototype.maxSolutions());
			this.solver = prototype.solver;
		}
		public boolean isIncremental() { return false; }
		public boolean isPartial() { return false; }
		public boolean isStepper() { return false; }

		public KodkodProblem extend() { throw new ActionException("Cannot extend a non-incremental specification."); }
		public KodkodProblem clear() { return new Complete(this) ; }

		public KodkodProblem solve(KodkodOutput out) {
			try {
				if (maxSolutions()==1) {
					write(out, solver.solve(asserts(), bounds()));
				} else {
					write(out, solver.solveAll(asserts(), bounds()));
				}
				return clear();
			} catch (RuntimeException ex) {
				throw new ActionException(ex.getMessage(), ex);
			}
		}
	}

	/**
	 * Enforces options configuration restrictions for incremental specifications.
	 * @author Emina Torlak
	 */
	private static class Incremental extends KodkodProblem {
		IncrementalSolver solver = null;

		Incremental() { }
		Incremental(DefEnv env, Bounds bounds, Options options, long maxSolutions, IncrementalSolver solver) {
			super(env, bounds, options, maxSolutions);
			this.solver = solver;
		}

		public final boolean isIncremental() { return true; }
		public boolean isPartial() { return false; }
		public boolean isStepper() { return false; }

		public final KodkodProblem extend() { return new Partial(this); }

		public final KodkodProblem clear() {
			if (solver!=null)
				solver.free();
			return new Incremental(new DefEnv(), null, options(), maxSolutions(), null) ;
		}

		public final KodkodProblem solve(KodkodOutput out) {
			try {
				if (solver==null)
					solver = IncrementalSolver.solver(options());
				write(out, solver.solve(asserts(), bounds()));
				KodkodServer.lastModel = null;
				return extend();
			} catch (RuntimeException ex) {
				throw new ActionException(ex.getMessage(), ex);
			}
		}

		boolean setCoreExtraction(boolean enable) {
			if (enable)
				throw new ActionException("Minimal unsat core extraction is not provided for incremental problems.");
			return super.setCoreExtraction(enable);
		}

		// Changed for Pardinus
		boolean setSolver(SATFactory solver) {
			if (!solver.incremental())
				throw new ActionException("Cannot use a non-incremental SAT solver ("+solver+") for incremental solving.");
			return super.setSolver(solver);
		}
	}

//	/**
//	 * Disables methods not supported for partial problem specifications.
//	 * @author Emina Torlak
//	 */
//	private static final class Partial extends KodkodProblem.Incremental {
//		//@SuppressWarnings("unchecked")
//		Partial(KodkodProblem.Incremental prev) {
//			super(prev.env(), new Bounds(prev.bounds().universe()), prev.options(), prev.maxSolutions(), prev.solver);
//		}
//		public final boolean isPartial() { return true; }
//		private final String cannot(String msg) {
//			return "Cannot " + msg + " of an incremental problem.  Use (clear) to start specifying a new problem.";
//		}
//		boolean setBitwidth(int bitwidth) { throw new ActionException(cannot("re-configure bitwidth")); }
//		boolean setSatSolver(SATFactory solver) { throw new ActionException(cannot("re-configure the solver")); }
//		boolean setCoreExtraction(boolean enable) { throw new ActionException(cannot("re-configure the core extraction behavior")); }
//		boolean declareInts(List<Integer> ints) { throw new ActionException(cannot("re-declare integer atoms in the universe of")); }
//	}

	/**
	 * Disables methods not supported for partial problem specifications.
	 * @author Emina Torlak
	 */
	private static final class Partial extends KodkodProblem.Incremental {
		private final Bounds complete;
		Partial(KodkodProblem.Incremental prev) {
			super(prev.env(), new Bounds(prev.bounds().universe()), prev.options(), prev.maxSolutions(), prev.solver);
			this.complete = prev.allBounds();
		}

		public final boolean isPartial() { return true; }
		public boolean isStepper() { return false; }

		public final Bounds allBounds() { return complete; }

		void declaredRelation(Relation r, TupleSet lower, TupleSet upper) { complete.bound(r, lower, upper); }

		private final String cannot(String msg) {
			return "Cannot " + msg + " of an incremental problem.  Use (clear) to start specifying a new problem.";
		}
		boolean setBitwidth(int bitwidth) { throw new ActionException(cannot("re-configure bitwidth")); }
		// Changed for Pardinus
		boolean setSolver(SATFactory solver) { throw new ActionException(cannot("re-configure the solver")); }
		boolean setCoreExtraction(boolean enable) { throw new ActionException(cannot("re-configure the core extraction behavior")); }
		boolean declareInts(List<Integer> ints) { throw new ActionException(cannot("re-declare integer atoms in the universe of")); }
	}
}
