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

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

import java.util.ArrayList;
import java.util.Collection;
import java.util.EnumSet;
import java.util.List;

import kodkod.ast.Formula;
import kodkod.ast.Relation;
import kodkod.ast.Variable;
import kodkod.ast.operator.ExprCompOperator;
import kodkod.ast.operator.FormulaOperator;
import kodkod.ast.operator.IntCompOperator;
import kodkod.ast.operator.Multiplicity;
import kodkod.ast.operator.Quantifier;

import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.Parameterized;
import org.junit.runners.Parameterized.Parameters;
import org.parboiled.Parboiled;
import org.parboiled.support.ParsingResult;

/**
 * Tests for the {@link KodkodParser#Constraint()} rules.
 * @author Emina Torlak
 */
@RunWith(Parameterized.class)
public class ConstraintRuleTest extends AbstractParserTest {
	private static final Relation[] r = {
		Relation.unary("r0"), Relation.unary("r1"),
		Relation.binary("r2"), Relation.binary("r3"),
		Relation.ternary("r4"), Relation.ternary("r5")
	};
	private final String input;
	private final ExpectedError expectedError;
	private final Formula expectedFormula;

	public ConstraintRuleTest(String input, ExpectedError expectedError, Formula expectedExpr) {
		this.input = input;
		this.expectedError = expectedError;
		this.expectedFormula = expectedExpr;
//		System.out.println(input);
	}

	@Before
	public void setUp() throws Exception {
		final KodkodParser parser = Parboiled.createParser(KodkodParser.class);
		setUp(parser, parser.Sequence(parser.DeclareUniverse(),parser.DeclareInts(),parser.OneOrMore(parser.DeclareRelation())));
		parse("(univ 10)" +
			  "(ints [(1 0) (2 1) (4 2) (-8 3)])" +
			  "(r0 [{} :: ints])" +
			  "(r1 [{} :: univ])" +
			  "(r2 [(-> none none) :: (-> ints univ)])" +
			  "(r3 [(-> none none) :: (-> univ ints)])" +
			  "(r4 [(-> none none none) :: (-> ints ints ints)])" +
			  "(r5 [(-> none none none) :: (-> univ ints univ)])");
		setUp(parser, parser.Sequence(parser.OneOrMore(parser.Constraint()), KodkodParser.EOI));
	}

	@Parameters
	public static Collection<Object[]> inputs() {
		final Collection<Object[]> ret = new ArrayList<Object[]>();
		badSyntax(ret);
		badSemantics(ret);
		validLeaf(ret);
		validComparisonAndMult(ret);		// this is broken.
		validNary(ret);
		validRelationPredicates(ret);
		validLet(ret);
		validQuantifier(ret);
		return ret;
	}

	private static void badSyntax(Collection<Object[]> ret) {
		ret.add(new Object[] { "()", ExpectedError.PARSE, null });
		for(Multiplicity op : Multiplicity.values()) {
			ret.add(new Object[] { "("+op.toString()+")", ExpectedError.PARSE, null });
			ret.add(new Object[] { "("+op.toString()+" 2)", ExpectedError.PARSE, null });
			ret.add(new Object[] { "("+op.toString()+" true)", ExpectedError.PARSE, null });
			ret.add(new Object[] { "("+op.toString()+" r0 r1)", ExpectedError.PARSE, null });
		}
		for(ExprCompOperator op : ExprCompOperator.values()) {
			ret.add(new Object[] { "("+op.toString()+")", ExpectedError.PARSE, null });
			ret.add(new Object[] { "("+op.toString()+" 2)", ExpectedError.PARSE, null });
			ret.add(new Object[] { "("+op.toString()+" univ true)", ExpectedError.PARSE, null });
			ret.add(new Object[] { "("+op.toString()+" r0 3)", ExpectedError.PARSE, null });
			ret.add(new Object[] { "("+op.toString()+" 3 r0)", ExpectedError.PARSE, null });
		}
		for(IntCompOperator op : IntCompOperator.values()) {
			ret.add(new Object[] { "("+op.toString()+")", ExpectedError.PARSE, null });
			ret.add(new Object[] { "("+op.toString()+" 2)", ExpectedError.PARSE, null });
			ret.add(new Object[] { "("+op.toString()+" 0 true)", ExpectedError.PARSE, null });
			ret.add(new Object[] { "("+op.toString()+" r0 3)", ExpectedError.PARSE, null });
			ret.add(new Object[] { "("+op.toString()+" 3 r0)", ExpectedError.PARSE, null });
		}
		ret.add(new Object[] { "(!)", ExpectedError.PARSE, null });
		ret.add(new Object[] { "(! univ)", ExpectedError.PARSE, null });
		ret.add(new Object[] { "(! 10)", ExpectedError.PARSE, null });
		for(FormulaOperator op : FormulaOperator.values()) {
			ret.add(new Object[] { "("+op.toString()+")", ExpectedError.PARSE, null });
			ret.add(new Object[] { "("+op.toString()+" 2)", ExpectedError.PARSE, null });
			ret.add(new Object[] { "("+op.toString()+" true 3)", ExpectedError.PARSE, null });
			ret.add(new Object[] { "("+op.toString()+" false r0)", ExpectedError.PARSE, null });
		}
		for(Quantifier quant : Quantifier.values()) {
			ret.add(new Object[] { "("+quant.toString()+")", ExpectedError.PARSE, null });
			ret.add(new Object[] { "("+quant.toString()+" ())", ExpectedError.PARSE, null });
			ret.add(new Object[] { "("+quant.toString()+" ()())", ExpectedError.PARSE, null });
			ret.add(new Object[] { "("+quant.toString()+" ([v0 : r0]))", ExpectedError.PARSE, null });
			ret.add(new Object[] { "("+quant.toString()+" ([v0 : r0])())", ExpectedError.PARSE, null });
			ret.add(new Object[] { "("+quant.toString()+" ([v0 : r0]) univ)", ExpectedError.PARSE, null });
			ret.add(new Object[] { "("+quant.toString()+" ([v0 : r0]) 2)", ExpectedError.PARSE, null });
		}
		ret.add(new Object[] { "(let)", ExpectedError.PARSE, null });
		ret.add(new Object[] { "(let () ())", ExpectedError.PARSE, null });
		ret.add(new Object[] { "(let ([g univ]) true)", ExpectedError.PARSE, null });
		ret.add(new Object[] { "(let ([e0 univ]) 2)", ExpectedError.PARSE, null });
		ret.add(new Object[] { "(let ([e0 univ]) e0)", ExpectedError.PARSE, null });
		ret.add(new Object[] { "(let ([f0 univ]) true)", ExpectedError.PARSE, null });
		ret.add(new Object[] { "(let ([i0 univ]) true)", ExpectedError.PARSE, null });
		ret.add(new Object[] { "(acyclic)", ExpectedError.PARSE, null });
		ret.add(new Object[] { "(acyclic 1)", ExpectedError.PARSE, null });
		ret.add(new Object[] { "(acyclic true)", ExpectedError.PARSE, null });
		ret.add(new Object[] { "(acyclic univ)", ExpectedError.PARSE, null });
		ret.add(new Object[] { "(total-order)", ExpectedError.PARSE, null });
		ret.add(new Object[] { "(total-order 1)", ExpectedError.PARSE, null });
		ret.add(new Object[] { "(total-order true)", ExpectedError.PARSE, null });
		ret.add(new Object[] { "(total-order univ)", ExpectedError.PARSE, null });
		ret.add(new Object[] { "(total-order r0 r1)", ExpectedError.PARSE, null });
		ret.add(new Object[] { "(total-order r0 r1 r2)", ExpectedError.PARSE, null });
	}

	private static void badSemantics(Collection<Object[]> ret) {
		for(ExprCompOperator op : ExprCompOperator.values()) {
			ret.add(new Object[] { "("+op.toString()+" univ iden)", ExpectedError.ACTION, null });
		}
		for(Quantifier quant : Quantifier.values()) {
			ret.add(new Object[] { "("+quant.toString()+" ([v2 :lone r2]) true)", ExpectedError.ACTION, null });
			ret.add(new Object[] { "("+quant.toString()+" ([v2 :one r2]) true)", ExpectedError.ACTION, null });
			ret.add(new Object[] { "("+quant.toString()+" ([v2 : r2]) true)", ExpectedError.ACTION, null });
			ret.add(new Object[] { "("+quant.toString()+" ([v2 : r0]) (some v3))", ExpectedError.ACTION, null });
		}
		ret.add(new Object[] { "(acyclic r0)", ExpectedError.ACTION, null });
		ret.add(new Object[] { "(acyclic r4)", ExpectedError.ACTION, null });
		ret.add(new Object[] { "(total-order r0 r1 r2 r3)", ExpectedError.ACTION, null });
		ret.add(new Object[] { "(total-order r2 r0 r1 r5)", ExpectedError.ACTION, null });
	}

	private static void validLeaf(Collection<Object[]> ret) {
		ret.add(new Object[] { "true", ExpectedError.NONE, Formula.TRUE  });
		ret.add(new Object[] { "false", ExpectedError.NONE, Formula.FALSE  });
	}

	private static void validComparisonAndMult(Collection<Object[]> ret) {
		for(int i = 0; i < r.length; i += 2) {

			// this has problems. what are they?
			for(ExprCompOperator op : ExprCompOperator.values()) {
				ret.add(new Object[] { "("+op.toString()+" " + r[i] + " " + r[i+1] +")", ExpectedError.NONE, r[i].compare(op, r[i+1]) });
			}
			for(IntCompOperator op : IntCompOperator.values()) {
				ret.add(new Object[] { "("+op.toString()+" (#" + r[i] + ") (#" + r[i+1] +"))", ExpectedError.NONE, r[i].count().compare(op, r[i+1].count()) });
			}
			for(Multiplicity mult : EnumSet.complementOf(EnumSet.of(Multiplicity.SET))) {
				ret.add(new Object[] { "("+mult.toString()+" " + r[i] +")", ExpectedError.NONE, r[i].apply(mult) });
			}
		}
	}

	private static void validNary(Collection<Object[]> ret) {
		for(FormulaOperator op : FormulaOperator.values()) {
			ret.add(new Object[] { "("+op.toString()+" (lone r5))", ExpectedError.NONE, r[5].lone() });
			ret.add(new Object[] { "("+op.toString()+" (lone r5) (no r2))", ExpectedError.NONE, r[5].lone().compose(op, r[2].no()) });
			ret.add(new Object[] { "(!("+op.toString()+" (lone r5)))", ExpectedError.NONE, r[5].lone().not() });
		}
		final Collection<Object[]> subs = new ArrayList<>();
		validComparisonAndMult(subs);
		final StringBuilder subIn = new StringBuilder();
		final List<Formula> subExpected = new ArrayList<>(subs.size());
		for(Object[] test : subs) {
			subIn.append((String)test[0]);
			subIn.append(" ");
			subExpected.add((Formula)test[2]);
		}
		for(FormulaOperator op : FormulaOperator.values()) {
			if (op.nary()) {
				ret.add(new Object[] { "(" + op.toString() + " " + subIn.toString() + ")", ExpectedError.NONE, Formula.compose(op, subExpected) });
			} else {
				Formula out = subExpected.get(0);
				for(int i = 0, size = subExpected.size(); i < size; i++) {
					out = out.compose(op, subExpected.get(i));
				}
			}
		}
	}

	private static void validRelationPredicates(Collection<Object[]> ret) {
		ret.add(new Object[] { "(acyclic r2)", ExpectedError.NONE, r[2].acyclic() });
		ret.add(new Object[] { "(total-order r2 r0 r1 r1)", ExpectedError.NONE, r[2].totalOrder(r[0], r[1], r[1]) });
	}

	private static void validLet(Collection<Object[]> ret) {
		for(Relation leaf : r) {
			ret.add(new Object[] { "(let ([f10 (some "+leaf+")]) f10)", ExpectedError.NONE, leaf.some() });
		}
		ret.add(new Object[] { "(let ([e5 r5][e0 r0]) (<=> (one e0) (no e5)))", ExpectedError.NONE, r[0].one().iff(r[5].no()) });
		ret.add(new Object[] { "(let ([f5 (no r5)][f0 (one r0)]) (<=> f0 f5))", ExpectedError.NONE, r[0].one().iff(r[5].no())});
		ret.add(new Object[] { "(let ([f5 (no r5)][f0 (<=> (one r0) f5)]) f0)", ExpectedError.NONE, r[0].one().iff(r[5].no()) });
		ret.add(new Object[] { "(let ([f5 (no r5)][f0 (one r0)]) (let ([f5 (lone r5)]) (<=> f0 f5)))", ExpectedError.NONE, r[0].one().iff(r[5].lone()) });

	}

	private static void validQuantifier(Collection<Object[]> ret) {
		final Variable v10 = Variable.unary("v10"), v5 = Variable.unary("v5"), v6 = Variable.unary("v6");
		for(Quantifier quant : Quantifier.values()) {
			ret.add(new Object[] { "("+quant+" ([v10 : r1]) true)", ExpectedError.NONE, Formula.TRUE.quantify(quant, v10.oneOf(r[1])) });
			ret.add(new Object[] { "("+quant+" ([v10 :one r1]) (in v10 r0))", ExpectedError.NONE, v10.in(r[0]).quantify(quant, v10.oneOf(r[1])) });
			ret.add(new Object[] { "("+quant+" ([v5 :one r0][v6 : (. v5 r2)]) (= v5 v6))", ExpectedError.NONE,
					v5.eq(v6).quantify(quant, v5.oneOf(r[0]).and(v6.oneOf(v5.join(r[2])))) });
			ret.add(new Object[] { "("+quant+" ([v5 :one r0][v6 : {([v10 : (. v5 r2)]) (one v10)}]) (= v5 v6))", ExpectedError.NONE,
					v5.eq(v6).quantify(quant, v5.oneOf(r[0]).and(v6.oneOf(v10.one().comprehension(v10.oneOf(v5.join(r[2])))))) });
			ret.add(new Object[] { "("+quant+" ([v5 :one r0][v6 : {([v6 : (. v5 r2)]) (no v6)}]) (= v5 v6))", ExpectedError.NONE,
					v5.eq(v6).quantify(quant, v5.oneOf(r[0]).and(v6.oneOf(v6.no().comprehension(v6.oneOf(v5.join(r[2])))))) });
		}
	}

	@Test
	public void test() {
		final ParsingResult<?> result = checkExpectedErrors(expectedError, parse(input));
		if (expectedError != ExpectedError.NONE) return;
		assertEquals(1, result.valueStack.size());
		assertTrue(result.valueStack.peek() instanceof Formula);
		final Formula actual = (Formula) result.valueStack.pop();
		assertEquals(expectedFormula.toString(), actual.toString());
		assertEquals(-1, globals().maxIndex('e'));
		assertEquals(-1, globals().maxIndex('f'));
		assertEquals(-1, globals().maxIndex('i'));
		assertEquals(-1, globals().maxIndex('v'));
	}

}
