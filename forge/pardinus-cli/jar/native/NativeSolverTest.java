
import kodkod.engine.satlab.SATFactory;
import kodkod.engine.satlab.SATProver;
import kodkod.engine.satlab.SATSolver;

public class NativeSolverTest {
    static int failures = 0;

    static void fail(String name, String why) {
        System.out.println(String.format("  %-14s FAIL: %s", name, why));
        failures++;
    }

    static void check(String name, SATFactory factory) {
        if (!SATFactory.available(factory)) {
            fail(name, "not available (the native library did not load or failed a trivial solve)");
            return;
        }

        SATSolver sat = factory.instance();
        try {
            sat.addVariables(2);
            sat.addClause(new int[] {1, 2});
            sat.addClause(new int[] {-1, 2});
            sat.addClause(new int[] {1, -2});
            if (!sat.solve()) {
                fail(name, "a satisfiable formula was reported unsatisfiable");
                return;
            }
            if (!sat.valueOf(1) || !sat.valueOf(2)) {
                fail(name, "wrong model for a satisfiable formula");
                return;
            }
        } finally {
            sat.free();
        }

        SATSolver unsat = factory.instance();
        try {
            unsat.addVariables(2);
            unsat.addClause(new int[] {1, 2});
            unsat.addClause(new int[] {-1, 2});
            unsat.addClause(new int[] {1, -2});
            unsat.addClause(new int[] {-1, -2});
            if (unsat.solve()) {
                fail(name, "an unsatisfiable formula was reported satisfiable");
                return;
            }
            if (factory.prover()) {
                if (unsat instanceof SATProver prover) {
                    if (prover.proof().core().size() == 0) {
                        fail(name, "the proof of unsatisfiability has an empty core");
                        return;
                    }
                } else {
                    fail(name, "a prover factory did not produce a SATProver");
                    return;
                }
            }
        } finally {
            unsat.free();
        }

        System.out.println(String.format("  %-14s ok (loads; sat model correct; unsat detected%s)",
                name, factory.prover() ? "; unsat core non-empty" : ""));
    }

    public static void main(String[] args) {
        check("MiniSat", SATFactory.MiniSat);
        check("MiniSatProver", SATFactory.MiniSatProver);
        check("Glucose", SATFactory.Glucose);
        check("Lingeling", SATFactory.Lingeling);
        System.out.println(failures == 0 ? "all solvers passed" : failures + " solver(s) failed");
        System.exit(failures == 0 ? 0 : 1);
    }
}
