package kodkod.engine.bddlab;


import java.util.*;

/**
 * Container for holding a variable assignment from a BDD path. Comes in two flavors -
 * partial and total solutions.
 * @author Mark Lavrentyev
 */
public abstract class BDDSolution {
    private final Set<Integer> trueVars;
    private final Set<Integer> falseVars;

    BDDSolution(Collection<Integer> tVars, Collection<Integer> fVars) {
        this.trueVars = new HashSet<>(tVars);
        this.falseVars = new HashSet<>(fVars);
    }

    /**
     * Getter for true variables in this solution.
     * @return the set of true variables in this solution.
     */
    public Set<Integer> getTrueVars() {
        return new HashSet<>(trueVars);
    }

    /**
     * Getter for the false variables in this solution.
     * @return The set of false variables in this solution.
     */
    public Set<Integer> getFalseVars() {
        return new HashSet<>(falseVars);
    }

    /**
     * Defines the interface for getting the value assigned to a variable in a solution.
     * @param var The variable to get the value of.
     * @return The value of the variable.
     */
    public abstract VarAssignment valueOf(int var);

    /**
     * Represents a partial solution of a problem, where some variables are indicated to be
     * don't-cares i.e. any assignment of these variables yields a proper total solution.
     * @author Mark lavrentyev
     */
    public static class Partial extends BDDSolution implements Iterator<Total> {
        private final List<Integer> dontCareVars;
        private long iteratorIdx = 0;

        /**
         * Constructor for creating a partial instance with variables that are true in this solution,
         * variables that are false in this solution, and variables whose assignment doesn't matter
         * in this solution (i.e. both true and false yields a solution).
         * @param trueVars vars that are assigned to true in this solution.
         * @param falseVars vars that are assigned to false in this solution.
         * @param dontCareVars vars whose assignment doesn't matter in this solution.
         */
        Partial(Collection<Integer> trueVars, Collection<Integer> falseVars, Collection<Integer> dontCareVars) {
            super(trueVars, falseVars);
            this.dontCareVars = new ArrayList<>(dontCareVars);
        }

        /**
         * Gets the dont care vars in this partial instance.
         * @return The set of variables whose assignment doesn't matter in this partial solution.
         */
        public Set<Integer> getDontCareVars() {
            return new HashSet<>(dontCareVars);
        }

        /**
         * Overrides {@link BDDSolution#valueOf(int)}. May return any of TRUE, FALSE, or DONTCARE.
         */
        @Override
        public VarAssignment valueOf(int var) {
            if (getTrueVars().contains(var)) {
                return VarAssignment.TRUE;
            } else if (getFalseVars().contains(var)) {
                return VarAssignment.FALSE;
            } else if (getDontCareVars().contains(var)) {
                return VarAssignment.DONTCARE;
            } else {
                throw new IllegalArgumentException("Tried to get value of variable " + var + ", which is not present in the solution.");
            }
        }

        /**
         * Tells whether there is another assignment of don't-care variables
         * that would yield a new total solution.
         * @return true if there is another distinct assignment of don't-care vars.
         */
        @Override
        public boolean hasNext() {
            return (Math.log(iteratorIdx + 1) / Math.log(2)) <= dontCareVars.size();
        }

        /**
         * Gets the next total solution that doesn't have any don't cares and has
         * an assignment to every don't care variable. The new solution is guaranteed
         * to be distinct from all previous ones.
         * @return A new bdd solution distinct from all previous ones, with all
         * don't-care variables assigned to either true or false.
         */
        @Override
        public Total next() {
            if (this.hasNext()) {
                Set<Integer> newTrueVars = new HashSet<>(getTrueVars());
                Set<Integer> newFalseVars = new HashSet<>(getFalseVars());

                for (int i = 0; i < dontCareVars.size(); i++) {
                    boolean dontCareAssn = ((iteratorIdx >> i) & 1) != 0;
                    if (dontCareAssn) {
                        newTrueVars.add(dontCareVars.get(i));
                    } else {
                        newFalseVars.add(dontCareVars.get(i));
                    }
                }

                this.iteratorIdx++;
                return new Total(newTrueVars, newFalseVars);
            } else {
                throw new NoSuchElementException("No more distinct don't-care assignments for this path");
            }
        }

        /**
         * Returns a total solution where the true and false variables in this partial solution
         * are kept and all the don't-care variables are randomly assigned to either true or false.
         * @return a total solution in this parital solution family with don't-cares randomly assigned.
         */
        public Total getRandomTotal() {
            Set<Integer> newTrueVars = new HashSet<>(getTrueVars());
            Set<Integer> newFalseVars = new HashSet<>(getFalseVars());

            Random rand = new Random();
            for (int i = 0; i < dontCareVars.size(); i++) {
                if (rand.nextBoolean()) {
                    newTrueVars.add(dontCareVars.get(i));
                } else {
                    newFalseVars.add(dontCareVars.get(i));
                }
            }

            return new Total(newTrueVars, newFalseVars);
        }

        /**
         * Get the corresponding total solution where all don't-cares are set to false.
         */
        public Total getFalseTotal() {
            Set<Integer> newTrueVars = new HashSet<>(getTrueVars());
            Set<Integer> newFalseVars = new HashSet<>(getFalseVars());

            newFalseVars.addAll(dontCareVars);
            return new Total(newTrueVars, newFalseVars);
        }

        /**
         * Get the corresponding total solution where all don't-cares are set to true.
         */
        public Total getTrueTotal() {
            Set<Integer> newTrueVars = new HashSet<>(getTrueVars());
            Set<Integer> newFalseVars = new HashSet<>(getFalseVars());

            newTrueVars.addAll(dontCareVars);
            return new Total(newTrueVars, newFalseVars);
        }
    }

    /**
     * Represents a total solution of a problem, where all variables have an assignment
     * and there are no don't-care variables.
     * @author Mark Lavrentyev
     */
    public static class Total extends BDDSolution {
        /**
         * Creates a total instance where every variable has an assignment.
         * @param trueVars vars that are true in this total assignment.
         * @param falseVars vars that are false in this total assignment.
         */
        Total(Collection<Integer> trueVars, Collection<Integer> falseVars) {
            super(trueVars, falseVars);
        }

        /**
         * Overrides {@link BDDSolution#valueOf(int)}.
         * @ensures value returned either TRUE or FALSE (never DONTCARE)
         * @throws IllegalArgumentException if the variable isn't in the solution at all.
         */
        @Override
        public VarAssignment valueOf(int var) {
            if (getTrueVars().contains(var)) {
                return VarAssignment.TRUE;
            } else if (getFalseVars().contains(var)) {
                return VarAssignment.FALSE;
            } else {
                throw new IllegalArgumentException("Tried to get value of variable " + var + ", which is not present in the solution.");
            }
        }

        /**
         * Boolean version of value of that can be used for total solutions.
         * @param var the variable to check
         * @return true if the variable is assigned to true, false if it's assigned to false.
         * @throws IllegalArgumentException if the variable isn't in the solution at all.
         */
        public boolean valueOfBool(int var) {
            return valueOf(var) == VarAssignment.TRUE;
        }
    }

    /**
     * Defines the three possible variable assignments in a solution: true, false, and don't-care.
     * @author Mark Lavrentyev
     */
    public enum VarAssignment {
        TRUE,
        FALSE,
        DONTCARE
    }
}
