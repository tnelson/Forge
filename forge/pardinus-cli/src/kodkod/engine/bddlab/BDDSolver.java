package kodkod.engine.bddlab;

import kodkod.engine.fol2sat.BooleanTranslation;

import java.util.Iterator;

/**
 * Interface that all bdd-based solvers should implement. Provides
 * methods for constructing a bdd for the given formula, getting solutions,
 * and checking satisfiability.
 * @param <B> The bdd class the solver uses.
 * @author Mark Lavrentyev.
 */
public interface BDDSolver extends Iterator<BDDSolution.Partial> {

    /**
     * Sets the boolean formula to solve to the given formula. This may
     * be used multiple times, provided {@link #done()} is called in
     * between sucessive calls to this method.
     * @param formula The formula for the bdd solver to solve.
     */
    void setFormula(BooleanTranslation formula);

    /**
     * Constructs the bdd from the solver's formula, which is part of the
     * boolean translation.
     * @return true if the resulting bdd is satisfiable, and false when unsat.
     */
    boolean construct();

    /**
     * Closes the solver and frees all memory. Any other method calls
     * after this may have unspecified behavior.
     */
    void done();

    /**
     * Tells whether the bdd solver is ready to return solutions through the iterator.
     * If false, calls to next() and hasNext() may throw errors.
     * @return true when the bdd solution iterator is ready to use.
     */
    boolean isReady();

    /**
     * Checks whether the current bdd is satisfiable.
     * @return true if there is some satisfying assignment for this bdd.
     */
    boolean isSat();

    /**
     * Gets the name of the solver.
     * @return The name of the solver.
     */
    String toString();
}
