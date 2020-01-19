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

import java.util.Collections;
import java.util.LinkedHashSet;
import java.util.Set;
import java.util.logging.Logger;

import kodkod.ast.Formula;
import kodkod.ast.Node;
import kodkod.ast.Relation;
import kodkod.engine.Proof;
import kodkod.engine.Solution;
import kodkod.engine.Statistics;
import kodkod.instance.Instance;
import kodkod.instance.Tuple;
import kodkod.instance.TupleSet;

/**
 * An implementation of the  {@link KodkodOutput} interface that writes solutions provided to it
 * to standard output, in the form of s-expressions.  Given a satisfiable
 * solution {@code s}, an instance of this class  will produce output of the form {@code (sat :model ([ri {(int+)*}]*))},
 * where each {@code ri} is a relation in {@code s.instance}, and {@code {(int+)*}} is an
 * s-expression representation of the tupleset {@code s.instance.tuples(ri)}.
 * Each tupleset is represented as a list of tuples, and each tuple is represented
 * as a (space-separated) list of atom indices comprising that tuple.
 *
 * <p>
 * Given an unsatisfiable solution {@code s} to a problem {@code p}, this implementation will produce output of the
 * form {@code (unsat :core (fi+))}, if a core is available, or of the form {@code (unsat)}, otherwise.
 * Each {@code fi} is a formula identifier in {@code p.env.defs['f']} such that the formula
 * {@code p.env.defs['f'][fi]} is in {@code s.proof.highLevelCore().values()}.  If there are some formulas
 * that are in the core but are not defined in {@code p.env.defs['f']}, a warning message will be printed to
 * a specified {@link Logger logger}.
 * </p>
 *
 * <p>
 * For both sat and unsat solutions, a {@link StandardKodkodOutput} instance
 * prints all statistics as info messages to its {@link Logger logger}.
 * The format of the info messages is unspecified.
 * </p>
 *
 * @specfield logger: {@link Logger}
 */
public final class StandardKodkodOutput implements KodkodOutput {
	private final Logger logger;

	/**
	 * Creates an instance of {@link StandardKodkodOutput}.
	 * @ensures this.logger' = logger
	 */
	StandardKodkodOutput(Logger logger) {  this.logger = logger; }

	/**
	 * Creates an instance of {@link StandardKodkodOutput} that will use the
	 * {@link Logger#getGlobal() global logger}.
	 * @ensures this.logger' = Logger.getGlobal()
	 */
	StandardKodkodOutput() {  this(Logger.getGlobal()); }

    public void writeUnsat(Solution sol, KodkodProblem problem) {
        writeCore(sol.proof(), (Defs<Formula>) problem.env().defs('f'));
        writeStats(problem, sol);
    }

	/**
	 * {@inheritDoc}
	 * @see kodkod.cli.KodkodOutput#writeSolution(kodkod.engine.Solution, kodkod.cli.KodkodProblem)
	 */
	@SuppressWarnings("unchecked")
	public void writeSolution(Solution sol, KodkodProblem problem) {
		if (sol.sat()) 	writeInstance(sol.instance(), (Defs<Relation>) problem.env().defs('r'));
		else			System.out.println("(no-more-instances)");
		writeStats(problem, sol);
	}



	/**
	 * Writes the instance s-expression to standard out.
	 * @requires all r: defs.def[int] | inst.tuples(r) != null
	 **/
	public void writeInstance(Instance inst, Defs<Relation> defs) {
		final StringBuilder str = new StringBuilder();
		str.append("(sat :model (");
		for (int i = 0, max = defs.maxIndex(); i <= max; i++) {
			final Relation r = defs.use(i);
			if (r==null) continue;
			final TupleSet ts = inst.tuples(r);
			assert ts != null;
			str.append("[").append(r).append(" {");
			final int arity = ts.arity();
			for(Tuple t : ts) {
				str.append("(");
				str.append(t.atomIndex(0));
				for(int idx = 1; idx < arity; idx++) {
					str.append(" ").append(t.atomIndex(idx));
				}
				str.append(")");
			}
			str.append("}]");
		}
		str.append("))");
		System.out.println(str);
	}

	/**
	 * Writes the core s-expression to standard out.
	 * @requires proof.highLevelCore().values() in defs.def[int]
	 **/
	public void writeCore(Proof proof, Defs<Formula> defs) {
		final StringBuilder str = new StringBuilder();
		str.append("(unsat");
		final Set<Node> core;
		if (proof == null) {
			core = Collections.emptySet();
		} else {
			core = new LinkedHashSet<Node>(proof.highLevelCore().values());
			str.append(" :core (");
			for (int i = 0, max = defs.maxIndex(); i <= max; i++) {
				if (core.remove(defs.use(i)))
					str.append('f').append(i).append(" ");
			}
			if (str.charAt(str.length()-1)==' ')
				str.deleteCharAt(str.length()-1);
			str.append(")");
		}
		str.append(")");
		System.out.println(str.toString());
		if (!core.isEmpty()) {
			Logger.getGlobal().warning(	"Returned minimal core is missing " + core.size() +
										" formulas for which no definitions of the form (fi ...) were provided.\n");
		}
	}

	/**
	 * Logs the given solution and problem statistics to {@code this.logger} as info messages.
	 */
	void writeStats(KodkodProblem problem, Solution sol) {
		final Statistics stats = sol.stats();
		writeStat("problem size",
				String.format("variables = %s, clauses = %s, state = %s bits",
				stats.variables(), stats.clauses(), stats.primaryVariables()));

		final long pt = problem.buildTimeMillis(), ct = problem.coreTimeMillis(),
				   tt = stats.translationTime(), st = stats.solvingTime();

		if (ct >= 0)
			writeStat("solving time (ms)",
					String.format("total = %s, parsing = %s, translation = %s, SAT = %s, core = %s",
					pt + tt + st + ct, pt, tt, st, ct));
		else
			writeStat("solving time (ms)",
					String.format("total = %s, parsing = %s, translation = %s, SAT = %s",
					pt + tt + st, pt, tt, st));
	}

	/**
	 * Logs the given statistic and its value to {@code this.logger} as an info message.
	 */
	void writeStat(String stat, Object value) {
		logger.info(String.format("%s: %s\n", stat, value));
	}

//	static final void printMemInfo() {
//    	Runtime runtime = Runtime.getRuntime();
//        NumberFormat format = NumberFormat.getInstance();
//        StringBuilder sb = new StringBuilder();
//        long maxMemory = runtime.maxMemory();
//        long allocatedMemory = runtime.totalMemory();
//        long freeMemory = runtime.freeMemory();
//        final long mb = 1048576;
//        sb.append("free memory (MB): ");
//        sb.append(format.format(freeMemory / mb));
//        sb.append("\n");
//        sb.append("allocated memory (MB): ");
//        sb.append(format.format(allocatedMemory / mb));
//        sb.append("\n");
//        sb.append("max memory (MB): ");
//        sb.append(format.format(maxMemory / mb));
//        sb.append("\n");
//        sb.append("total free memory (MB): ");
//        sb.append(format.format((freeMemory + (maxMemory - allocatedMemory)) / mb));
//        sb.append("\n");
//        System.out.println(sb);
//    }
}
