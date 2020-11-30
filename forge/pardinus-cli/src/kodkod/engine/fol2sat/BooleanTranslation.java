package kodkod.engine.fol2sat;

import kodkod.ast.Relation;
import kodkod.engine.bddlab.BDDSolution;
import kodkod.engine.bddlab.BDDSolver;
import kodkod.engine.bool.*;
import kodkod.engine.config.Options;
import kodkod.instance.Bounds;
import kodkod.instance.Instance;
import kodkod.instance.TupleFactory;
import kodkod.instance.TupleSet;
import kodkod.util.collections.BiMap;
import kodkod.util.ints.IndexedEntry;
import kodkod.util.ints.IntIterator;
import kodkod.util.ints.IntSet;
import kodkod.util.ints.Ints;

import java.util.HashSet;
import java.util.Map;
import java.util.Set;

public class BooleanTranslation {
    private final Bounds bounds;
    private final Options options;
    private final Map<Relation, IntSet> primaryVarUsage;
    private final int maxPrimaryVar;

    private TranslationLog log;

    private final BiMap<BooleanVariable, Integer> varMap = new BiMap<>();
    private final BooleanFormula formula;
    private final BDDSolver solver;

    /**
     * Create the boolean translation object from a formula, bounds, options, and vars. Initializes
     * the bdd solver based on the solver type the options object specifies.
     */
    BooleanTranslation(BooleanFormula formula, Bounds bounds, Options options, Map<Relation, IntSet> varUsage, int maxPrimaryVar) {
        assert options.solverType() == Options.SolverType.BDD;

        this.formula = formula;
        this.bounds = bounds;
        this.options = options;
        this.solver = options.bddSolver().instance();
        solver.setFormula(this);
        this.primaryVarUsage = varUsage;
        this.maxPrimaryVar = maxPrimaryVar;
    }

    /**
     * Same as {@link #BooleanTranslation(BooleanFormula, Bounds, Options, Map, int)} except it also
     * takes the translation log and stores it.
     * @param log The log of the translation process.
     */
    BooleanTranslation(BooleanFormula formula, Bounds bounds, Options options, Map<Relation, IntSet> varUsage, int maxPrimaryVar, TranslationLog log) {
        this(formula, bounds, options, varUsage, maxPrimaryVar);
        this.log = log;
    }

    /**
     * See {@link Translation#primaryVariables(Relation)}. Does the same thing, just for a bdd-based
     * translation.
     */
    public IntSet primaryVariables(Relation relation) {
        final IntSet vars = primaryVarUsage.get(relation);
        return vars==null ? Ints.EMPTY_SET : vars;
    }

    /**
     * Gets the number of variables that need to be encoded into the bdd to solve.
     */
    public int getNumVars() {
        return formula.accept(new VarGetter(), new Object()).size();
    }

    /**
     * Gets the boolean formula in this translation.
     * @return the translated boolean formula.
     */
    public BooleanFormula getFormula() {
        return formula;
    }

    /**
     * Gets the solver variable associated with the given boolean variable.
     * @param var the formula variable.
     * @return the solver variable.
     */
    public int getVarMap(BooleanVariable var) {
        return varMap.getV(var);
    }

    /**
     * Gets the formula variable associated with the given solver variable.
     * @param var the solver variable.
     * @return the formula variable.
     */
    public BooleanVariable getVarMap(int var) {
        return varMap.getK(var);
    }

    /**
     * Adds the variable mapping between var and solverVar.
     * @param var The boolean variable used in the formula.
     * @param solverVar The variable label used in the solver.
     */
    public void setVarMap(BooleanVariable var, int solverVar) {
        varMap.put(var, solverVar);
    }

    /**
     * Check if the given boolean variable has already been mapped to a solver variable.
     * @param var The boolean variable to check if it's been mapped already.
     */
    public boolean isVarMapped(BooleanVariable var) {
        return varMap.containsK(var);
    }

    /**
     * Gets the current bdd solver in use for this problem.
     * @return The bdd solver in use for this problem.
     */
    public BDDSolver solver() {
        return solver;
    }

    /**
     * Gets the options used in this translation.
     * @return The options used in this translation
     */
    public Options options() {
        return options;
    }

    /**
     * Gets the instance associated with the given total solution to the boolean problem.
     * @param totalSolution The total assignment of variables that satisfies the formula.
     * @return The instance corresponding to this solution.
     */
    public final Instance interpret(BDDSolution.Total totalSolution) {
        // build instance from the boolean solution
        final Instance instance = new Instance(bounds.universe());
        final TupleFactory f = bounds.universe().factory();

        for(IndexedEntry<TupleSet> entry : bounds.intBounds()) {
            instance.add(entry.index(), entry.value());
        }

        for(Relation r : bounds.relations()) {
            TupleSet lower = bounds.lowerBound(r);
            IntSet indices = Ints.bestSet(lower.capacity());
            indices.addAll(lower.indexView());
            IntSet vars = primaryVariables(r);

            if (!vars.isEmpty()) {
                int lit = vars.min();
                for(IntIterator iter = bounds.upperBound(r).indexView().iterator(); iter.hasNext();) {
                    final int index = iter.next();
                    if (!indices.contains(index) && totalSolution.valueOfBool(getVarMap(new BooleanVariable(lit++)))) {
                        indices.add(index);
                    }
                }
            }
            instance.add(r, f.setOf(r.arity(), indices));
        }

        return instance;
    }

    /**
     * Gets the first solution from the bdd solver and interprets it into an instance, similar
     * to how {@link Translation#interpret()} works.
     * @requires solver.isReady() = true
     * @requires solver.isSat() = true
     * @return the instance that corresponds to the first solution from the solver.
     */
    public final Instance interpret() {
        // get a solution
        if (!solver.isReady()) {
            throw new IllegalStateException("BDD solver must be ready before getting solution.");
        } else if (!solver.isSat()) {
            throw new IllegalStateException("BDD solver must be SAT to get a solution.");
        }
        BDDSolution.Total totalSolution = solver.next().next();
        return interpret(totalSolution);
    }

    /**
     * Traverses a boolean formula and returns the set of variables present in the formula.
     * @author Mark Lavrentyev
     */
    private class VarGetter implements BooleanVisitor<Set<BooleanVariable>, Object> {
        @Override
        public Set<BooleanVariable> visit(MultiGate multigate, Object arg) {
            return visitGate(multigate, arg);
        }

        @Override
        public Set<BooleanVariable> visit(ITEGate ite, Object arg) {
            return visitGate(ite, arg);
        }

        /**
         * Returns all the variables in the sub-formula of this negation.
         */
        @Override
        public Set<BooleanVariable> visit(NotGate negation, Object arg) {
            return visitGate(negation, arg);
        }

        /**
         * Returns the singleton containing just the variable.
         */
        @Override
        public Set<BooleanVariable> visit(BooleanVariable variable, Object arg) {
            Set<BooleanVariable> singleton = new HashSet<>();
            singleton.add(variable);
            return singleton;
        }

        private Set<BooleanVariable> visitGate(BooleanFormula formula, Object arg) {
            Set<BooleanVariable> vars = new HashSet<>();
            for (int i = 0; i < formula.size(); i++) {
                vars.addAll(formula.input(i).accept(this, arg));
            }
            return vars;
        }
    }
}
