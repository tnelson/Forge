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

import static kodkod.ast.operator.ExprOperator.CLOSURE;
import static kodkod.ast.operator.ExprOperator.DIFFERENCE;
import static kodkod.ast.operator.ExprOperator.INTERSECTION;
import static kodkod.ast.operator.ExprOperator.JOIN;
import static kodkod.ast.operator.ExprOperator.OVERRIDE;
import static kodkod.ast.operator.ExprOperator.PRODUCT;
import static kodkod.ast.operator.ExprOperator.REFLEXIVE_CLOSURE;
import static kodkod.ast.operator.ExprOperator.TRANSPOSE;
import static kodkod.ast.operator.ExprOperator.UNION;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

import java.util.ArrayList;
import java.util.Collection;
import java.util.EnumSet;

import kodkod.ast.Expression;
import kodkod.ast.Formula;
import kodkod.ast.IntConstant;
import kodkod.ast.Relation;
import kodkod.ast.Variable;
import kodkod.ast.operator.ExprOperator;

import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.Parameterized;
import org.junit.runners.Parameterized.Parameters;
import org.parboiled.Parboiled;
import org.parboiled.support.ParsingResult;

/**
 * Tests for the {@link KodkodParser#Expr()} rules.
 * @author Emina Torlak
 */
@RunWith(Parameterized.class)
public class ExprRuleTest extends AbstractParserTest {
	private static final Relation[] r = {
		Relation.unary("r0"), Relation.unary("r1"),
		Relation.binary("r2"), Relation.binary("r3"),
		Relation.ternary("r4"), Relation.ternary("r5")
	};
	private final String input;
	private final ExpectedError expectedError;
	private final Expression expectedExpr;

	public ExprRuleTest(String input, ExpectedError expectedError, Expression expectedExpr) {
		this.input = input;
		this.expectedError = expectedError;
		this.expectedExpr = expectedExpr;
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
		setUp(parser, parser.Sequence(parser.OneOrMore(parser.Expr()), KodkodParser.EOI));
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
		validComprehension(ret);
		return ret;
	}

	private static void badSyntax(Collection<Object[]> ret) {
		ret.add(new Object[] { "()", ExpectedError.PARSE, null });
		for(ExprOperator op : ExprOperator.values()) {
			ret.add(new Object[] { "("+op.toString()+")", ExpectedError.PARSE, null });
			ret.add(new Object[] { "("+op.toString()+" 2)", ExpectedError.PARSE, null });
			ret.add(new Object[] { "("+op.toString()+" true)", ExpectedError.PARSE, null });
			if (op.unary()) {
				ret.add(new Object[] { "("+op.toString()+" iden none)", ExpectedError.PARSE, null });
			}
		}
		ret.add(new Object[] { "(set)", ExpectedError.PARSE, null });
		ret.add(new Object[] { "(lone)", ExpectedError.PARSE, null });
		ret.add(new Object[] { "(ite)", ExpectedError.PARSE, null });
		ret.add(new Object[] { "(ite true)", ExpectedError.PARSE, null });
		ret.add(new Object[] { "(ite true 0)", ExpectedError.PARSE, null });
		ret.add(new Object[] { "(ite true univ 1)", ExpectedError.PARSE, null });
		ret.add(new Object[] { "(let)", ExpectedError.PARSE, null });
		ret.add(new Object[] { "(let () ())", ExpectedError.PARSE, null });
		ret.add(new Object[] { "(let ([g univ]) none)", ExpectedError.PARSE, null });
		ret.add(new Object[] { "(let ([e0 univ]) 2)", ExpectedError.PARSE, null });
		ret.add(new Object[] { "(let ([e0 univ]) true)", ExpectedError.PARSE, null });
		ret.add(new Object[] { "(let ([f0 univ]) none)", ExpectedError.PARSE, null });
		ret.add(new Object[] { "(let ([i0 univ]) none)", ExpectedError.PARSE, null });
		ret.add(new Object[] { "{}", ExpectedError.PARSE, null });
		ret.add(new Object[] { "{()}", ExpectedError.PARSE, null });
		ret.add(new Object[] { "{([v0 : 3])}", ExpectedError.PARSE, null });
		ret.add(new Object[] { "{([v0 : univ])}", ExpectedError.PARSE, null });
		ret.add(new Object[] { "{([v0 : univ]) 3}", ExpectedError.PARSE, null });
		ret.add(new Object[] { "{([v0 : univ]) none}", ExpectedError.PARSE, null });
		ret.add(new Object[] { "{([v0 :no univ]) true}", ExpectedError.PARSE, null });
	}

	private static void badSemantics(Collection<Object[]> ret) {
		for(ExprOperator op : EnumSet.of(UNION, INTERSECTION, OVERRIDE, DIFFERENCE)) {
			ret.add(new Object[] { "("+op.toString()+" univ iden)", ExpectedError.ACTION, null });
		}
		for(ExprOperator op : EnumSet.of(TRANSPOSE, CLOSURE, REFLEXIVE_CLOSURE)) {
			ret.add(new Object[] { "("+op.toString()+" univ)", ExpectedError.ACTION, null });
		}
		ret.add(new Object[] { "(. univ univ)", ExpectedError.ACTION, null });
		ret.add(new Object[] { "(ite true    iden univ)", ExpectedError.ACTION, null });
		ret.add(new Object[] { "(ite false      r0 r2)", ExpectedError.ACTION, null });
		ret.add(new Object[] { "{([v0 :lone univ]) true}", ExpectedError.ACTION, null });
		ret.add(new Object[] { "{([v0 :some univ]) true}", ExpectedError.ACTION, null });
		ret.add(new Object[] { "{([v0 :set univ]) true}", ExpectedError.ACTION, null });
	}

	private static void validLeaf(Collection<Object[]> ret) {
		for(Relation leaf : r) {
			ret.add(new Object[] { leaf.toString(), ExpectedError.NONE, leaf  });
		}
		for(Expression leaf : new Expression[]{Expression.UNIV, Expression.IDEN, Expression.NONE, Expression.INTS}) {
			ret.add(new Object[] { leaf.toString(), ExpectedError.NONE, leaf  });
		}

	}

	private static void validUnaryAndCast(Collection<Object[]> ret) {
		for(ExprOperator op : EnumSet.of(TRANSPOSE, CLOSURE, REFLEXIVE_CLOSURE)) {
			ret.add(new Object[] { "("+op.toString()+" r2)", ExpectedError.NONE, r[2].apply(op) });
		}
		ret.add(new Object[] { "(lone 4)", ExpectedError.NONE, IntConstant.constant(4).toExpression() });
		ret.add(new Object[] { "(set 8)", ExpectedError.NONE, IntConstant.constant(8).toBitset() });
	}

	private static void validNary(Collection<Object[]> ret) {
		for(ExprOperator op : EnumSet.of(UNION, INTERSECTION, OVERRIDE, PRODUCT, DIFFERENCE, JOIN)) {
			ret.add(new Object[] { "("+op.toString()+" univ)", ExpectedError.NONE, Expression.compose(op, new Expression[]{Expression.UNIV}) });
			ret.add(new Object[] { "("+op.toString()+" r5)", ExpectedError.NONE, Expression.compose(op, new Expression[]{r[5]}) });
		}
		for(ExprOperator op : EnumSet.of(UNION, INTERSECTION, OVERRIDE, PRODUCT)) {
			ret.add(new Object[] { "("+op.toString()+" univ    r1)", ExpectedError.NONE, Expression.compose(op, Expression.UNIV, r[1]) });
			ret.add(new Object[] { "("+op.toString()+" r2 iden r3)", ExpectedError.NONE, Expression.compose(op, r[2], Expression.IDEN, r[3]) });
			ret.add(new Object[] { "("+op.toString()+" none r0 ints r1)", ExpectedError.NONE, Expression.compose(op, Expression.NONE, r[0], Expression.INTS, r[1])});
		}
		ret.add(new Object[] { "(- univ r1)", ExpectedError.NONE, Expression.UNIV.difference(r[1]) });
		ret.add(new Object[] { "(- r2 iden    r3)", ExpectedError.NONE, r[2].difference(Expression.IDEN).difference(r[3]) });
		ret.add(new Object[] { "(- none     r0 ints     r1)", ExpectedError.NONE, Expression.NONE.difference(r[0]).difference(Expression.INTS).difference(r[1])});
		ret.add(new Object[] { "(. r4 r1)", ExpectedError.NONE, r[4].join(r[1]) });
		ret.add(new Object[] { "(. r2 iden r3)", ExpectedError.NONE, r[2].join(Expression.IDEN).join(r[3]) });
		ret.add(new Object[] { "(. r3 r2 r5 r1)", ExpectedError.NONE, r[3].join(r[2]).join(r[5]).join(r[1])});
	}

	private static void validIf(Collection<Object[]> ret) {
		for(int i = 0; i < r.length; i += 2) {
			ret.add(new Object[] { "(ite true " +r[i] + " " + r[i+1]+ ")", ExpectedError.NONE, Formula.TRUE.thenElse(r[i], r[i+1]) });
			ret.add(new Object[] { "(ite (some r5) " +r[i] + " " + r[i+1]+ ")", ExpectedError.NONE, r[5].some().thenElse(r[i], r[i+1]) });
			ret.add(new Object[] { "(ite (lone r4) (+ " +r[i] + ") (& " + r[i+1]+ "))", ExpectedError.NONE, r[4].lone().thenElse(r[i], r[i+1]) });
		}
	}

	private static void validLet(Collection<Object[]> ret) {
		for(Relation leaf : r) {
			ret.add(new Object[] { "(let ([e0 "+leaf+"]) e0)", ExpectedError.NONE, leaf });
		}
		ret.add(new Object[] { "(let ([e5 r5][e0 r0]) (. e0 e5))", ExpectedError.NONE, r[0].join(r[5]) });
		ret.add(new Object[] { "(let ([e5 r5][e0 r0]) (let ([e5 r4]) (. e0 e5)))", ExpectedError.NONE, r[0].join(r[4]) });
	}

	private static void validComprehension(Collection<Object[]> ret) {
		final Variable v10 = Variable.unary("v10"), v5 = Variable.unary("v5"), v6 = Variable.unary("v6");
		ret.add(new Object[] { "{([v10 : r1]) true}", ExpectedError.NONE, Formula.TRUE.comprehension(v10.oneOf(r[1])) });
		ret.add(new Object[] { "{([v10 :one r1]) (in v10 r0)}", ExpectedError.NONE, v10.in(r[0]).comprehension(v10.oneOf(r[1])) });
		ret.add(new Object[] { "{([v5 :one r0][v6 : (. v5 r2)]) (= v5 v6)}", ExpectedError.NONE,
				v5.eq(v6).comprehension(v5.oneOf(r[0]).and(v6.oneOf(v5.join(r[2])))) });
		ret.add(new Object[] { "{([v5 :one r0][v6 : {([v10 : (. v5 r2)]) (one v10)}]) (= v5 v6)}", ExpectedError.NONE,
				v5.eq(v6).comprehension(v5.oneOf(r[0]).and(v6.oneOf(v10.one().comprehension(v10.oneOf(v5.join(r[2])))))) });
		ret.add(new Object[] { "{([v5 :one r0][v6 : {([v6 : (. v5 r2)]) (no v6)}]) (= v5 v6)}", ExpectedError.NONE,
				v5.eq(v6).comprehension(v5.oneOf(r[0]).and(v6.oneOf(v6.no().comprehension(v6.oneOf(v5.join(r[2])))))) });
	}

	@Test
	public void test() {
		final ParsingResult<?> result = checkExpectedErrors(expectedError, parse(input));
		if (expectedError != ExpectedError.NONE) return;
		assertEquals(1, result.valueStack.size());
		assertTrue(result.valueStack.peek() instanceof Expression);
		final Expression actual = (Expression) result.valueStack.pop();
		assertEquals(expectedExpr.arity(), actual.arity());
		assertEquals(expectedExpr.toString(), actual.toString());
		assertEquals(0, globals().keys('e').size());
		assertEquals(0, globals().keys('f').size());
		assertEquals(0, globals().keys('i').size());
		assertEquals(0, globals().keys('v').size());
	}

}
