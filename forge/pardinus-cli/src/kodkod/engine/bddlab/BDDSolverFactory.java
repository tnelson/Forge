package kodkod.engine.bddlab;


/**
 * Factory class for building bdd solvers such as BuDDy.
 * @author Mark Lavrentyev
 */
public abstract class BDDSolverFactory {

    public static final BDDSolverFactory JBuDDy = new BDDSolverFactory() {
        @Override
        public BDDSolver instance() {
            return new JBuDDy();
        }

        @Override
        public String toString() {
            return "JBuDDy";
        }
    };

    public abstract BDDSolver instance();
}
