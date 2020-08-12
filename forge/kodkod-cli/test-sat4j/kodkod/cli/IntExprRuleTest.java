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

import static kodkod.ast.IntConstant.constant;
import static kodkod.ast.operator.IntOperator.MINUS;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

import java.util.ArrayList;
import java.util.Collection;

import kodkod.ast.Formula;
import kodkod.ast.IntConstant;
import kodkod.ast.IntExpression;
import kodkod.ast.Relation;
import kodkod.ast.Variable;
import kodkod.ast.operator.IntOperator;

import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.Parameterized;
import org.junit.runners.Parameterized.Parameters;
import org.parboiled.Parboiled;
import org.parboiled.support.ParsingResult;

/**
 * Tests for the {@link KodkodParser#IntExpr()} rules.
 * @author Emina Torlak
 */
@RunWith(Parameterized.class)
public class IntExprRuleTest extends AbstractParserTest {
	private static final IntConstant max = constant(Integer.MAX_VALUE);
	private static final IntConstant min = constant(Integer.MIN_VALUE);
	private static final Relation[] r = {
		Relation.unary("r0"), Relation.unary("r1"),
		Relation.binary("r2"), Relation.binary("r3"),
		Relation.ternary("r4"), Relation.ternary("r5")
	};
	private final String input;
	private final ExpectedError expectedError;
	private final IntExpression expectedExpr;

	public IntExprRuleTest(String input, ExpectedError expectedError, IntExpression expectedExpr) {
		this.input = input;
		this.expectedError = expectedError;
		this.expectedExpr = expectedExpr;
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
		setUp(parser, parser.Sequence(parser.OneOrMore(parser.IntExpr()), KodkodParser.EOI));
	}

	@Parameters
	public static Collection<Object[]> inputs() {
		final Collection<Object[]> ret = new ArrayList<Object[]>();
		badSyntax(ret);
		badSemantics(ret);
		validLeaf(ret);
		validUnaryAndCast(ret);
		validNary(ret);
		validIf(ret);
		validLet(ret);
		validSum(ret);
		return ret;
	}

	private static void badSyntax(Collection<Object[]> ret) {
		ret.add(new Object[] { "()", ExpectedError.PARSE, null });
		for(IntOperator op : IntOperator.values()) {
			ret.add(new Object[] { "("+op.toString()+")", ExpectedError.PARSE, null });
			ret.add(new Object[] { "("+op.toString()+" none)", ExpectedError.PARSE, null });
			ret.add(new Object[] { "("+op.toString()+" true)", ExpectedError.PARSE, null });
			if (op.unary() && op!=IntOperator.NEG) {
				ret.add(new Object[] { "("+op.toString()+" 1 2)", ExpectedError.PARSE, null });
			}
		}
		ret.add(new Object[] { "(#)", ExpectedError.PARSE, null });
		ret.add(new Object[] { "(sum)", ExpectedError.PARSE, null });
		ret.add(new Object[] { "(sum true)", ExpectedError.PARSE, null });
		ret.add(new Object[] { "(ite)", ExpectedError.PARSE, null });
		ret.add(new Object[] { "(ite false)", ExpectedError.PARSE, null });
		ret.add(new Object[] { "(ite true univ)", ExpectedError.PARSE, null });
		ret.add(new Object[] { "(ite true 1 univ)", ExpectedError.PARSE, null });
		ret.add(new Object[] { "(let)", ExpectedError.PARSE, null });
		ret.add(new Object[] { "(let () ())", ExpectedError.PARSE, null });
		ret.add(new Object[] { "(let ([g univ]) 2)", ExpectedError.PARSE, null });
		ret.add(new Object[] { "(let ([i0 3]))", ExpectedError.PARSE, null });
		ret.add(new Object[] { "(let ([i0 ints]) 2)", ExpectedError.PARSE, null });
		ret.add(new Object[] { "(let ([e0 univ]) true)", ExpectedError.PARSE, null });
		ret.add(new Object[] { "(let ([f0 true]) none)", ExpectedError.PARSE, null });
		ret.add(new Object[] { "(sum () ())", ExpectedError.PARSE, null });
		ret.add(new Object[] { "(sum ([v0 : r0]))", ExpectedError.PARSE, null });
		ret.add(new Object[] { "(sum ([v0 : r0]) v0)", ExpectedError.PARSE, null });
		ret.add(new Object[] { "(sum ([v0 : r0]) true)", ExpectedError.PARSE, null });
		ret.add(new Object[] { "(sum ([v0 :no r0]) 3)", ExpectedError.PARSE, null });
	}

	private static void badSemantics(Collection<Object[]> ret) {
		ret.add(new Object[] { "(sum r2)", ExpectedError.ACTION, null });
		ret.add(new Object[] { "(sum r4)", ExpectedError.ACTION, null });
		ret.add(new Object[] { "(sum ([v0 :set r0]) (# v0))", ExpectedError.ACTION, null });
		ret.add(new Object[] { "(sum ([v0 :some r1]) (# v0))", ExpectedError.ACTION, null });
		ret.add(new Object[] { "(sum ([v0 :lone r0]) (# v0))", ExpectedError.ACTION, null });
		ret.add(new Object[] { "(sum ([v0 :set r2]) (# v0))", ExpectedError.ACTION, null });
		ret.add(new Object[] { "(sum ([v0 : r2]) (# v0))", ExpectedError.ACTION, null });
		ret.add(new Object[] { "(sum ([v0 : r0]) (# v11))", ExpectedError.ACTION, null });
		ret.add(new Object[] { "(sum ([v0 : r0][v0 : r1]) (# v10))", ExpectedError.ACTION, null });
		ret.add(new Object[] { "(let ([i0 0]) i5)", ExpectedError.ACTION, null });
		ret.add(new Object[] { "(let ([i0 0][i0 1]) i0)", ExpectedError.ACTION, null });
	}

	private static void validLeaf(Collection<Object[]> ret) {
		ret.add(new Object[] { "0", ExpectedError.NONE, IntConstant.constant(0) });
		ret.add(new Object[] { String.valueOf(Integer.MAX_VALUE), ExpectedError.NONE, max });
		ret.add(new Object[] { String.valueOf(Integer.MIN_VALUE), ExpectedError.NONE, min });
	}

	private static void validUnaryAndCast(Collection<Object[]> ret) {
		for(IntOperator op : IntOperator.values()) {
			if (op.unary()) {
				ret.add(new Object[] { "( "+op.toString()+ " "+ Integer.MAX_VALUE +" )", ExpectedError.NONE, max.apply(op) });
			}
		}
		ret.add(new Object[] { "(# r4)", ExpectedError.NONE, r[4].count() });
		ret.add(new Object[] { "(sum r1)", ExpectedError.NONE, r[1].sum() });
	}

	private static void validNary(Collection<Object[]> ret) {
		for(IntOperator op : IntOperator.values()) {
			if (op.binary()) {
				if (op!=MINUS)
					ret.add(new Object[] { "( "+op.toString()+ " "+ Integer.MAX_VALUE +" )", ExpectedError.NONE, IntExpression.compose(op, new IntExpression[]{max}) });
				ret.add(new Object[] { "( "+op.toString()+ " "+ Integer.MAX_VALUE + " " + Integer.MIN_VALUE + " )", ExpectedError.NONE, IntExpression.compose(op, max, min) });
				ret.add(new Object[] { "( "+op.toString()+ " (# r2)(#r3) )", ExpectedError.NONE, IntExpression.compose(op, r[2].count(),  r[3].count()) });
				if (op.nary()) {
					ret.add(new Object[] { "( "+op.toString()+ " (# r2)10(#r3) (sum r0) (sum r1) (#r5) (#r4) )",
							ExpectedError.NONE,
							IntExpression.compose(op, r[2].count(), constant(10),  r[3].count(), r[0].sum(), r[1].sum(), r[5].count(), r[4].count()) });
				} else {
					ret.add(new Object[] { "( "+op.toString()+ " (# r2)10(#r3) (sum r0) (sum r1) (#r5) (#r4) )",
							ExpectedError.NONE,
							r[2].count().compose(op, constant(10)).compose(op, r[3].count()).compose(op, r[0].sum()).compose(op, r[1].sum()).compose(op, r[5].count()).compose(op, r[4].count()) });
				}
			}

		}
	}

	private static void validIf(Collection<Object[]> ret) {
		for(int i = 0; i < r.length; i += 2) {
			ret.add(new Object[] { "(ite true (#" +r[i] + ")(#" + r[i+1]+ "))", ExpectedError.NONE, Formula.TRUE.thenElse(r[i].count(), r[i+1].count()) });
			ret.add(new Object[] { "(ite (some r5) (#  " +r[i] + ") " + min+ ")", ExpectedError.NONE, r[5].some().thenElse(r[i].count(), min) });
			ret.add(new Object[] { "(ite (lone r4) (+ " +max + ") (- (#" + r[i+1]+ ")))", ExpectedError.NONE, r[4].lone().thenElse(max, r[i+1].count().negate()) });
		}
	}

	private static void validLet(Collection<Object[]> ret) {
		for(Relation leaf : r) {
			ret.add(new Object[] { "(let ([i10 (#"+leaf+")]) i10)", ExpectedError.NONE, leaf.count() });
		}
		ret.add(new Object[] { "(let ([e5 r5][e0 r0]) (<< (#e0) (#e5)))", ExpectedError.NONE, r[0].count().shl(r[5].count()) });
		ret.add(new Object[] { "(let ([i5 (#r5)][i0 (#r0)]) (<< i0 i5))", ExpectedError.NONE, r[0].count().shl(r[5].count()) });
		ret.add(new Object[] { "(let ([i5 (#r5)][i0 (<< (#r0) i5)]) i0)", ExpectedError.NONE, r[0].count().shl(r[5].count()) });
		ret.add(new Object[] { "(let ([i5 (#r5)][i0 (#r0)]) (let ([i5 5]) (<< i0 i5)))", ExpectedError.NONE, r[0].count().shl(constant(5)) });
	}

	private static void validSum(Collection<Object[]> ret) {
		final Variable v10 = Variable.unary("v10"), v5 = Variable.unary("v5"), v6 = Variable.unary("v6");
		ret.add(new Object[] { "(sum ([v10 : r1]) (sum v10))", ExpectedError.NONE, v10.sum().sum(v10.oneOf(r[1])) });
		ret.add(new Object[] { "(sum ([v10 :one r1]) (+ (sum v10) (sum r0)))",
				ExpectedError.NONE, v10.sum().plus(r[0].sum()).sum(v10.oneOf(r[1])) });
		ret.add(new Object[] { "(sum ([v5 :one r0][v6 : (. v5 r2)]) (let ([i0 (#v5)][i1 (#v6)]) (/ i0 i1)))", ExpectedError.NONE,
				v5.count().divide(v6.count()).sum(v5.oneOf(r[0]).and(v6.oneOf(v5.join(r[2])))) });
	}

	@Test
	public void test() {
		final ParsingResult<?> result = checkExpectedErrors(expectedError, parse(input));
		if (expectedError != ExpectedError.NONE) return;
		assertEquals(1, result.valueStack.size());
		assertTrue(result.valueStack.peek() instanceof IntExpression);
		final IntExpression actual = (IntExpression) result.valueStack.pop();
		assertEquals(expectedExpr.toString(), actual.toString());
//		assertEquals(-1, globals().maxIndex('e'));
//		assertEquals(-1, globals().maxIndex('f'));
//		assertEquals(-1, globals().maxIndex('i'));
//		assertEquals(-1, globals().maxIndex('v'));
	}

}
