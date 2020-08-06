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


import java.util.List;
import java.util.Map.Entry;
import java.util.Set;
import java.util.logging.Logger;

import kodkod.ast.Decl;
import kodkod.ast.Formula;
import kodkod.ast.Relation;
import kodkod.engine.bool.BooleanFormula;
import kodkod.engine.config.Reporter;
import kodkod.instance.Bounds;
import kodkod.instance.Tuple;
import kodkod.util.ints.IntSet;

/**
 * An implementation of the reporter interface that prints messages
 * to a {@link Logger logger}.
 * @author Emina Torlak
 */
public final class KodkodReporter implements Reporter {
	private final Logger logger;

	/*
	 * Added for Pardinus.
	 */
	public void reportConfigs(int configs, int primaryVars, int vars, int clauses) {};
	
	/*
	 * Added for Pardinus.
	 */
	public void warning(String warning) {};

	/*
 	 * Added for Pardinus.
	 */
	public void debug(String debug) {};

	/*
 	 * Added for Pardinus.
	 */
	public void reportLex(List<Entry<Relation, Tuple>> original,
			List<Entry<Relation, Tuple>> permuted) {};

	/**
	 * Constructs a new instance of the LoggerReporter that logs its messages
	 * to the given logger. 
	 */
	public KodkodReporter(Logger logger) { 
		this.logger = logger;
	}
	
	/**
	 * Constructs a new instance of the LoggerReporter that logs its messages
	 * to the {@link Logger#getGlobal() global logger}.
	 */
	public KodkodReporter() { this(Logger.getGlobal()); }
	
	/**
	 * Returns the logger used by this reporter.
	 * @return this.logger.
	 */
	public Logger logger() { return logger; }

	/**
	 * {@inheritDoc}
	 * @see kodkod.engine.config.Reporter#detectingSymmetries(kodkod.instance.Bounds)
	 */
	public void detectingSymmetries(Bounds bounds){
		logger.fine("Detecting symmetries.\n");
	}
	
	/**
	 * {@inheritDoc}
	 * @see kodkod.engine.config.Reporter#detectedSymmetries(java.util.Set)
	 */
	public void detectedSymmetries(Set<IntSet> parts) {
		logger.finer("Detected " + parts.size() + " equivalence classes of atoms.\n");
	}
	
	/**
	 * @see kodkod.engine.config.Reporter#optimizingBoundsAndFormula()
	 */
	public void optimizingBoundsAndFormula() {
		logger.fine("Optimizing bounds and formula (breaking predicate symmetries, inlining, skolemizing).\n");
	}
	
	/**
	 * {@inheritDoc}
	 * @see kodkod.engine.config.Reporter#skolemizing(kodkod.ast.Decl, kodkod.ast.Relation, java.util.List)
	 */
	public void skolemizing(Decl decl, Relation skolem, List<Decl> context) {
		logger.finer("Skolemizing " + decl + ": skolem relation=" + skolem + ", arity=" + skolem.arity() + ".\n");
	}
	
	/**
	 * @see kodkod.engine.config.Reporter#translatingToBoolean(kodkod.ast.Formula, kodkod.instance.Bounds)
	 */
	public void translatingToBoolean(Formula formula, Bounds bounds) {
		logger.fine("Translating to a boolean circuit.\n");
	}

	/**
	 * @see kodkod.engine.config.Reporter#generatingSBP()
	 */
	public void generatingSBP() {
		logger.fine("Generating lex-leader symmetry breaking predicate.\n");
	}
	
	/**
	 * @see kodkod.engine.config.Reporter#translatingToCNF(kodkod.engine.bool.BooleanFormula)
	 */
	public void translatingToCNF(BooleanFormula circuit) {
		logger.fine("Translating to cnf.\n");
	}
	
	/**
	 * @see kodkod.engine.config.Reporter#solvingCNF(int, int, int)
	 */
	public void solvingCNF(int primaryVars, int vars, int clauses) {
		logger.fine("Solving p cnf " + vars + " " + clauses + ".\n");
	}
	
	/**
	 * @see java.lang.Object#toString()
	 */
	public String toString() {
		return "KodkodReporter";
	}
}
