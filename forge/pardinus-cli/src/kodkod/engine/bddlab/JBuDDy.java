package kodkod.engine.bddlab;

import kodkod.engine.bool.*;
import kodkod.engine.fol2sat.BooleanTranslation;
import net.sf.javabdd.BDD;
import net.sf.javabdd.BDDFactory;
import net.sf.javabdd.BuDDyFactory;

import java.io.FileNotFoundException;
import java.io.PrintStream;
import java.io.PrintWriter;
import java.io.UnsupportedEncodingException;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Set;


/**
 * Java wrapper for the JavaBDD BuDDy wrapper.
 * @author Mark Lavrentyev
 */
final class JBuDDy implements BDDSolver {
    private BooleanTranslation translation;
    private BDDFactory factory;
    private BDD bdd;
    private Iterator<byte[]> pathIterator;

    public JBuDDy() {}

    /**
     * See {@link BDDSolver#setFormula(BooleanTranslation)}.
     * @param formula The formula for the bdd solver to solve.
     */
    @Override
    public void setFormula(BooleanTranslation booleanTranslation) {
        this.translation = booleanTranslation;

        // supress printing temporarily
        PrintStream sysOut = System.out;
        System.setOut(System.err);

        this.factory = BDDFactory.init(100000, 10000);
        this.factory.setVarNum(translation.getNumVars());

        // un-suppress printing again
        System.setOut(sysOut);

        // start with empty bdd which is trivially true
        this.bdd = factory.one();
    }

    /**
     * Constructs the BDD from the boolean formula. Returns true if
     * the formula is sat and false if it's unsat. Also exports the dot graph of the BDD as
     * a png file.
     * @return true when sat, false when unsat
     */
    @Override
    public boolean construct() {
        // supress printing temporarily
        PrintStream sysOut = System.out;
        System.setOut(System.err);

        assert factory.isInitialized();
        bdd = translation.getFormula().accept(new BDDConstructor(), new Object());
        pathIterator = bdd.allsat().iterator();

        exportDotGraph("bdd-graph.gv");

        // un-suppress printing again
        System.setOut(sysOut);

        return isSat();
    }

    /**
     * Exports a dot graph of the bdd to the given file.
     * @param filename The file to write the dot graph to.
     */
    private void exportDotGraph(String filename) {
        PrintStream sysOutBefore = System.out;

        try {
            PrintStream fileStream = new PrintStream(filename);
            System.setOut(fileStream);
            bdd.printDot();
            fileStream.flush();
            fileStream.close();
        } catch (FileNotFoundException e) {
            System.out.flush();
            System.out.close();
        }

        System.setOut(sysOutBefore);
    }

    /**
     * Frees any memory being used by BuDDy and puts it back into its original state.
     * Behavior after calling this is unspecified.
     */
    @Override
    public void done() {
        factory.done();
    }

    /**
     * Tells whether the solver is ready to iterate over the solutions.
     * Returns false if the iterator has not yet been initialized (which
     * happens when the bdd is constructed).
     * @return True when the solution iterator is ready to use.
     */
    @Override
    public boolean isReady() {
        return pathIterator != null;
    }

    /**
     * Checks whether the current bdd is satisfiable i.e. it's not the false node.
     * @return true if the bdd is satisfiable and false otherwise.
     */
    @Override
    public boolean isSat() {
        return !bdd.isZero();
    }

    /**
     * Tells whether there is another distinct path to the true node in the bdd.
     * Does not count new solutions where only don't-care variables change.
     * @return true if there is another solution.
     */
    @Override
    public boolean hasNext() {
        return pathIterator.hasNext();
    }

    /**
     * Gets the next solution along a different path in the bdd (i.e. not counting
     * new solutions where only don't-care variables change).
     * @return A new solution along a new path in the bdd.
     */
    @Override
    public BDDSolution.Partial next() {
        byte[] assignment = pathIterator.next();
        Set<Integer> trueVars = new HashSet<>();
        Set<Integer> falseVars = new HashSet<>();
        Set<Integer> dontCareVars = new HashSet<>();

        for (int i = 0; i < assignment.length; i++) {
            if (assignment[i] == 1) {
                trueVars.add(i);
            } else if (assignment[i] == 0) {
                falseVars.add(i);
            } else if (assignment[i] == -1) {
                dontCareVars.add(i);
            }
        }

        return new BDDSolution.Partial(trueVars, falseVars, dontCareVars);
    }

    /**
     * Returns name of the solver - JBuDDy
     * @return "JBuDDy"
     */
    @Override
    public String toString() {
        return "JBuDDy";
    }

    /**
     * Constructor to build BDD based on the formula it's given. Implements the
     * BooleanVisitor.
     * @author Mark Lavrentyev
     */
    private class BDDConstructor implements BooleanVisitor<BDD, Object> {
        int nextVar = 0;

        /**
         * Return a bdd representing the given multigate boolean formula. Recursively
         * builds the bdd for each of the sub-formulas and combines them based on the
         * multigate operator.
         * @param multigate The multigate boolean formula to build a bdd for.
         * @return The bdd for the given multigate formula.
         */
        @Override
        public BDD visit(MultiGate multigate, Object arg) {
            BDD[] branchBDDs = new BDD[multigate.size()];
            for (int i = 0; i < multigate.size(); i++) {
                branchBDDs[i] = multigate.input(i).accept(this, arg);

                if (branchBDDs[i] == null) {
                    return null;
                }
            }

            if (multigate.op().equals(Operator.AND)) {
                BDD combinedBDD = factory.one();
                for (BDD branchBDD : branchBDDs) {
                    combinedBDD = combinedBDD.and(branchBDD);
                }
                return combinedBDD;

            } else if (multigate.op().equals(Operator.OR)) {
                BDD combinedBDD = factory.zero();
                for (BDD branchBDD : branchBDDs) {
                    combinedBDD = combinedBDD.or(branchBDD);
                }
                return combinedBDD;

            } else {
                return null;
            }
        }

        /**
         * Return a bdd representing an if-then-else (ite) gate. Recursively builds
         * the bdd for each of the three sub-branches and combines them.
         * @param ite The ite boolean formula to build a bdd for.
         * @return The bdd for the ite boolean formula.
         */
        @Override
        public BDD visit(ITEGate ite, Object arg) {
            BDD ifSubBDD = ite.input(0).accept(this, arg);
            BDD thenSubBDD = ite.input(1).accept(this, arg);
            BDD elseSubBDD = ite.input(2).accept(this, arg);

            if (ifSubBDD == null || thenSubBDD == null || elseSubBDD == null) {
                return null;
            }

            return ifSubBDD.ite(thenSubBDD, elseSubBDD);
        }

        /**
         * Return a bdd representing the negation formula passed in.
         * @param negation The not-gate formula to represent with a bdd.
         * @return A bdd representing the not-ed formula.
         */
        @Override
        public BDD visit(NotGate negation, Object arg) {
            BDD subformulaBDD = negation.input(0).accept(this, arg);

            if (subformulaBDD == null) {
                return null;
            }

            return subformulaBDD.not();
        }

        /**
         * Return a bdd representing just the given boolean variable.
         * @param variable The variable to represent.
         * @return The bdd representing the given variable.
         */
        @Override
        public BDD visit(BooleanVariable variable, Object arg) {
            if (translation.isVarMapped(variable)) {
                return factory.ithVar(translation.getVarMap(variable));
            } else {
                translation.setVarMap(variable, nextVar);
                return factory.ithVar(nextVar++);
            }
        }
    }
}