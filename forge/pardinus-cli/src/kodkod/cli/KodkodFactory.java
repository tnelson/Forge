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
import java.util.logging.Level;
import java.util.logging.Logger;

import kodkod.ast.Decl;
import kodkod.ast.Decls;
import kodkod.ast.Expression;
import kodkod.ast.Formula;
import kodkod.ast.IntExpression;
import kodkod.ast.Node;
import kodkod.ast.Relation;
import kodkod.ast.Variable;
import kodkod.ast.operator.ExprCastOperator;
import kodkod.ast.operator.ExprCompOperator;
import kodkod.ast.operator.ExprOperator;
import kodkod.ast.operator.FormulaOperator;
import kodkod.ast.operator.IntOperator;
import kodkod.ast.operator.Multiplicity;
import kodkod.engine.config.Options;
import kodkod.instance.Bounds;
import kodkod.instance.Tuple;
import kodkod.instance.TupleFactory;
import kodkod.instance.TupleSet;

import org.parboiled.errors.ActionException;

/**
 * Provides a set of methods that wrap Kodkod's {@link Node AST}, {@link Bounds bounds},
 * and {@link Options options} construction  methods.  The wrappers intercept all
 * {@link RuntimeException runtime exceptions} thrown by the wrapped methods,
 * and re-throw them as {@link ActionException action exceptions} for better error reports.
 *
 * @author Emina Torlak
 */
public final class KodkodFactory {
	private KodkodFactory() {}

	//-------------------------------------------------------------------------
	//  Option configuration, verbosity and output management
	//-------------------------------------------------------------------------
	/**
	 * Returns a fresh options object with all default options, except for
	 * the reporter, which is set to a {@link KodkodReporter} that logs its messages
	 * to the {@link Logger#getGlobal() global logger}.  Classes in this package
	 * should use this method rather than Options constructor to create new options instances.
	 * @return some o: Options | o.reporter.logger = Logger.getGlobal() && o.reporter.logger.level = Level.SEVERE
	 */
	public static final Options baseOptions() {
		Options opts = new Options();
		final KodkodReporter r = new KodkodReporter();
		r.logger().setLevel(Level.SEVERE);
		opts.setReporter(r);
        opts.setSymmetryBreaking(0);
		return opts;
	}

	/**
	 * Translates a Kodkod verbosity parameter into a corresponding logging {@link Level}.
	 * Verbosity values [0..Integer.MAX_VALUE] correspond to logger levels as follows:
	 * 0 = no messages; 1 = only error/severe messages;  2 = warning messages; 3 = info messages;
	 * 4/5/6 = fine/finer/finest messages; and >6 = all messages.
	 * @requires verbosity >= 0
	 * @return {@link Level} correponding to the given verbosity
	 */
	public static final Level level(int verbosity) {
		switch(verbosity) {
		case 0 : return Level.OFF;
		case 1 : return Level.SEVERE;
		case 2 : return Level.WARNING;
		case 3 : return Level.INFO;
		case 4 : return Level.FINE;
		case 5 : return Level.FINER;
		case 6 : return Level.FINEST;
		default: return Level.ALL;
		}
	}

	//-------------------------------------------------------------------------
	//  Tupleset construction
	//-------------------------------------------------------------------------
	/**
	 * Returns the exact bound for the given relation, as specified by the given bounds.
	 * @requires r in bounds.relations
	 * @requires bounds.lowerBound[r] = bounds.upperBound[r]
	 * @return bounds.lowerBound[r]
	 */
	public static final TupleSet valueOf(Relation r, Bounds bounds) {
		final TupleSet rval = bounds.lowerBound(r);
		if (!rval.equals(bounds.upperBound(r)))
			throw new ActionException("Cannot get the value of a non-constant relation " + r.name() + ".");
		return rval;
	}

	/**
	 * Returns the tupleset representation of the given constant expression with respect to the given bounds.
	 * @requires c in UNIV + NONE + INTS + IDEN
	 * @return the tupleset representation of the given constant expression with respect to the given bounds.
	 */
	public static final TupleSet valueOf(Expression c, Bounds bounds) {
		final TupleFactory f = bounds.universe().factory();
		if (c==Expression.UNIV) {
			return f.allOf(1);
		} else if (c==Expression.NONE) {
			return f.noneOf(1);
		} else if (c==Expression.INTS) {
			final TupleSet ints = f.noneOf(1);
			for(TupleSet ts : bounds.intBounds().values()) {
				ints.addAll(ts);
			}
			return ints;
		} else if (c==Expression.IDEN) {
			final TupleSet ts = f.noneOf(2);
			for(Object atom : f.universe()) {
				ts.add(f.tuple(atom,atom));
			}
			return ts;
		} else {
			throw new ActionException("Unrecognized relational constant: " + c);
		}
	}

	/**
	 * Composes the given tupleset with the provided list of tuplesets according
	 * to the semantics of the given operator.  The operator must be one of {@link ExprOperator#UNION},
	 * {@link ExprOperator#INTERSECTION}, {@link ExprOperator#DIFFERENCE}, or
	 * {@link ExprOperator#PRODUCT}, describing the corresponding
	 * bulk operation on {@link TupleSet tuple sets}.
	 * @return first op rest.get(0) op ... op rest.get(rest.size()-1)
	 */
	public static final TupleSet compose(ExprOperator op, TupleSet first, List<TupleSet> rest) {
		try {
			final TupleFactory f = first.universe().factory();
			TupleSet out = f.noneOf(first.arity());
			out.addAll(first);
			switch(op) {
			case PRODUCT : 		for(TupleSet ts : rest) { out = out.product(ts); }; break;
			case UNION : 		for(TupleSet ts : rest) { out.addAll(ts); }; break;
			case INTERSECTION :	for(TupleSet ts : rest) { out.retainAll(ts); }; break;
			case DIFFERENCE :	for(TupleSet ts : rest) { out.removeAll(ts); }; break;
			default:            throw new ActionException("Cannot compose tuple sets using " + op + ".");
			}
			return out;
		} catch (RuntimeException ex) {
			throw new ActionException(ex.getMessage(), ex); // wrap
		}
	}

	/**
	 * Creates a new tupleset containing the union of the given tuplesets and returns it.
	 * @return first op second
	 */
	public static final TupleSet union(TupleSet first,  TupleSet  second) {
		try {
			final TupleFactory f = first.universe().factory();
			TupleSet out = f.noneOf(first.arity());
			out.addAll(first);
			out.addAll(second);
			return out;
		} catch (RuntimeException ex) {
			throw new ActionException(ex.getMessage(), ex); // wrap
		}
	}

	/**
	 * Returns a freshly created tupleset that consists of all tuples with indices between the two tuples.
	 * @return this.bounds.universe.factory().range(start, end)
	 */
	public static final TupleSet range(Tuple start, Tuple end) {
		try {
			return start.universe().factory().range(start, end);
		} catch (RuntimeException ex) {
			throw new ActionException(ex.getMessage(), ex); // wrap
		}
	}

	/**
	 * Returns a freshly created tupleset that consists of all tuples
	 * in the region defined by the two tuples.
	 * @return this.bounds.universe.factory().area(start, end)
	 */
	public static final TupleSet area(Tuple start, Tuple end) {
		try {
			return start.universe().factory().area(start, end);
		} catch (RuntimeException ex) {
			throw new ActionException(ex.getMessage(), ex); // wrap
		}
	}

	/**
	 * Returns a fresh modifiable tupleset containing the given tuple.
	 * @ensures this.bounds.universe.factory().setOf(t)
	 */
	public static final TupleSet setOf(Tuple t) {
		try {
			return t.universe().factory().setOf(t);
		} catch (RuntimeException ex) {
			throw new ActionException(ex.getMessage(), ex); // wrap
		}
	}

	/**
	 * Adds the given tuple to the provided tupleset.
	 * @ensures ts.add(t)
	 */
	public static final boolean add(TupleSet ts, Tuple t) {
		try {
			ts.add(t);
		} catch (RuntimeException ex) {
			throw new ActionException(ex.getMessage(), ex); // wrap
		}
		return true;
	}

	/**
	 * Returns a tuple consisting  of the atoms specified in the list, constructed
	 * using the provided tuple factory.
	 * @requires !atoms.isEmpty()
	 * @requires all a: atoms.elts[int] | a in factory.universe.atoms[int]
	 * @return this.bounds.universe.factory().tuple(atoms)
	 **/
	public static final Tuple tuple(TupleFactory factory, List<?> atoms) {
		try {
			return factory.tuple(atoms);
		} catch (RuntimeException ex) {
			throw new ActionException(ex.getMessage(), ex); // wrap
		}
	}

	//-------------------------------------------------------------------------
    //  Quantified variable declarations
    //-------------------------------------------------------------------------
	/**
	 * Returns a declaration of a quantified variable {@code name} with the
	 * given multiplicity and expression.
	 * @return Variable.nary(name, expr.arity()).declare(mult, expr)
	 */
	public static final Decl declareVariable(String name, Multiplicity mult, Expression expr) {
		try {
			//System.out.println(mult);
			//System.out.println(expr);
			return Variable.nary(name, expr.arity()).declare(mult, expr);
		} catch (RuntimeException ex) {
			throw new ActionException(ex.getMessage(), ex); // wrap
		}
	}

	//-------------------------------------------------------------------------
    //  Formula construction
    //-------------------------------------------------------------------------
	/**
	 * Composes the formulas in the list according to the semantics of the given operator,
	 * and returns the result.  Application of binary operators to more than two arguments
	 * results in a left-associative application of the operator to the arguments.
	 * @return Formula.compose(op, args)
	 */
	public static final Formula compose(FormulaOperator op, List<Formula> args) {
		try {
			if (op.nary()) {
				return Formula.compose(op, args);
			} else {
				Formula out = args.get(0);
				for(int i = 1, size = args.size(); i < size; i++) {
					out = out.compose(op, args.get(i));
				}
				return out;
			}
		} catch (RuntimeException ex) {
			throw new ActionException(ex.getMessage(), ex); // wrap
		}
	}
	/**
	 * Compares the expressions in the list according to the semantics of the given operator,
	 * and returns the result.
	 * @return Expression.compare(op, left, right);
	 */
	public static final Formula compare(ExprCompOperator op, Expression left, Expression right) {
		try {
			return left.compare(op, right);
		} catch (RuntimeException ex) {
			throw new ActionException(ex.getMessage(), ex); // wrap
		}
	}
	/**
	 * Returns the acyliclity predicate over the given relation.
	 * @return r.acyclic()
	 */
	public static final Formula acyclic(Relation r) {
		try {
			return r.acyclic();
		} catch (RuntimeException ex) {
			throw new ActionException(ex.getMessage(), ex); // wrap
		}
	}
	/**
	 * Returns the total order predicate over the given relations.
	 * @return ord.totalOrder(ordered, first, last)
	 */
	public static final Formula totalOrder(Relation ord, Relation ordered, Relation first, Relation last) {
		try {
			return ord.totalOrder(ordered, first, last);
		} catch (RuntimeException ex) {
			throw new ActionException(ex.getMessage(), ex); // wrap
		}
	}

	//-------------------------------------------------------------------------
    //  Expression construction
    //-------------------------------------------------------------------------
	/**
	 * Composes the expressions in the list according to the semantics of the given operator,
	 * and returns the result.  Application of binary operators to more than two arguments
	 * results in a left-associative application of the operator to the arguments.
	 * @requires args.size() > 0
	 * @requires op.unary() => args.size()=1
	 * @return op.nary() => Expression.compose(op, args) else
	 * 		   op.binary() => LEFT_ASSOCIATE(op, args) else
	 *         op.unary() => args.get(0).apply(op)
	 */
	public static final Expression compose(ExprOperator op, List<Expression> args) {
		try {
			if (op.nary()) {
				return Expression.compose(op, args);
			} else if (op.binary()) {
				Expression out = args.get(0);
				for(int i = 1, size = args.size(); i < size; i++) {
					out = out.compose(op, args.get(i));
				}
				return out;
			} else {
				assert op.unary() && args.size()==1;
				return args.get(0).apply(op);
			}
		} catch (RuntimeException ex) {
			throw new ActionException(ex.getMessage(), ex); // wrap
		}
	}
	/**
	 * Returns the set comprehension over the given formula and declarations.
	 * @ensures formula.comprehension(decls)
	 */
	public static final Expression comprehension(Decls decls, Formula formula) {
		try {
			return formula.comprehension(decls);
		} catch (RuntimeException ex) {
			throw new ActionException(ex.getMessage(), ex); // wrap
		}
	}
	/**
	 * Returns the if-then-else over the given formula and branch expressions.
	 * @ensures formula.thenElse(thenExpr, elseExpr)
	 */
	public static final Expression ite(Formula formula, Expression thenExpr, Expression elseExpr) {
		try {
			return formula.thenElse(thenExpr, elseExpr);
		} catch (RuntimeException ex) {
			throw new ActionException(ex.getMessage(), ex); // wrap
		}
	}

	//-------------------------------------------------------------------------
    //  IntExpression (bitvector expression) construction
    //-------------------------------------------------------------------------
	/**
	 * Composes the int expressions in the list according to the semantics of the given operator.
	 * Application of binary operators to more than two arguments
	 * results in a left-associative application of the operator to the arguments.
	 * @requires op.unary() => args.size()=1
	 * @return op.nary() => IntExpression.compose(op, args) else
	 * 		   op.binary() => LEFT_ASSOCIATE(op, args) else
	 *         op.unary() => args.get(0).apply(op)
	 */
	public static final IntExpression compose(IntOperator op, List<IntExpression> args) {
		try {
			if (op.nary()) {
				return IntExpression.compose(op, args);
			} else if (op.binary()) {
				IntExpression out = args.get(0);
				for(int i = 1, size = args.size(); i < size; i++) {
					out = out.compose(op, args.get(i));
				}
				return out;
			} else {
				assert op.unary() && args.size()==1;
				return args.get(0).apply(op);
			}
		} catch (RuntimeException ex) {
			throw new ActionException(ex.getMessage(), ex); // wrap
		}
	}

	/**
	 * Returns the expression-to-bitvector conversion specified by the given operator.
	 * @ensures expr.apply(op)
	 */
	public static final IntExpression cast(ExprCastOperator op, Expression expr) {
		try {
			return expr.apply(op);
		} catch (RuntimeException ex) {
			throw new ActionException(ex.getMessage(), ex); // wrap
		}
	}

	/**
	 * Returns the quantified sum expression over the given decls.
	 * @ensures expr.sum(decls)
	 */
	public static final IntExpression sum(Decls decls, IntExpression expr) {
		try {
			return expr.sum(decls);
		} catch (RuntimeException ex) {
			throw new ActionException(ex.getMessage(), ex); // wrap
		}
	}
}
