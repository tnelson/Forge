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

import java.util.ArrayList;
import java.util.Collection;

import kodkod.ast.IntConstant;
import kodkod.ast.Node;
import kodkod.ast.Relation;

import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.Parameterized;
import org.junit.runners.Parameterized.Parameters;
import org.parboiled.Parboiled;
import org.parboiled.errors.ActionException;
import org.parboiled.support.ParsingResult;

/**
 * Tests for the {@link KodkodParser#DefNode()} rules.
 * @author Emina Torlak
 */
@RunWith(Parameterized.class)
public class NodeRuleTest extends AbstractParserTest {
	private static final Relation[] r = {
		Relation.unary("r0"), Relation.unary("r1"),
		Relation.binary("r2"), Relation.binary("r3"),
		Relation.ternary("r4"), Relation.ternary("r5")
	};
	private final String input;
	private final ExpectedError expectedError;
	private final DefEnv expectedEnv;

	public NodeRuleTest(String input, ExpectedError expectedError, DefEnv expectedEnv) {
		this.input = input;
		this.expectedError = expectedError;
		this.expectedEnv = expectedEnv;
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
		setUp(parser, parser.Sequence(parser.OneOrMore(parser.DefNode()), KodkodParser.EOI));
	}

	@Parameters
	public static Collection<Object[]> inputs() {
		final Collection<Object[]> ret = new ArrayList<Object[]>();
		badSyntax(ret);
		badSemantics(ret);
		validDefs(ret);
		return ret;
	}

	private static void badSyntax(Collection<Object[]> ret) {
		ret.add(new Object[] { "()", ExpectedError.PARSE, null });
		ret.add(new Object[] { "(define)", ExpectedError.PARSE, null });
		ret.add(new Object[] { "(v0 univ)", ExpectedError.PARSE, null });
		ret.add(new Object[] { "(r0 univ)", ExpectedError.PARSE, null });
		ret.add(new Object[] { "(g0 univ)", ExpectedError.PARSE, null });
		ret.add(new Object[] { "(e0)", ExpectedError.PARSE, null });
		ret.add(new Object[] { "(e0 1)", ExpectedError.PARSE, null });
		ret.add(new Object[] { "(e0 true)", ExpectedError.PARSE, null });
		ret.add(new Object[] { "(i0)", ExpectedError.PARSE, null });
		ret.add(new Object[] { "(i0 none)", ExpectedError.PARSE, null });
		ret.add(new Object[] { "(i0 false)", ExpectedError.PARSE, null });
		ret.add(new Object[] { "(f0)", ExpectedError.PARSE, null });
		ret.add(new Object[] { "(f0 1)", ExpectedError.PARSE, null });
		ret.add(new Object[] { "(f0 iden)", ExpectedError.PARSE, null });
	}

	private static void badSemantics(Collection<Object[]> ret) {
		ret.add(new Object[] { "(e0 e0)", ExpectedError.ACTION, null });
		ret.add(new Object[] { "(f0 f0)", ExpectedError.ACTION, null });
		ret.add(new Object[] { "(i0 i0)", ExpectedError.ACTION, null });
		ret.add(new Object[] { "(e0 e10)", ExpectedError.ACTION, null });
		ret.add(new Object[] { "(f0 f10)", ExpectedError.ACTION, null });
		ret.add(new Object[] { "(i0 i10)", ExpectedError.ACTION, null });
		ret.add(new Object[] { "(e0 v10)", ExpectedError.ACTION, null });
		ret.add(new Object[] { "(f0 (some v10))", ExpectedError.ACTION, null });
		ret.add(new Object[] { "(i0 (#v10))", ExpectedError.ACTION, null });
		ret.add(new Object[] { "(e0 r10)", ExpectedError.ACTION, null });
		ret.add(new Object[] { "(f0 (some r10))", ExpectedError.ACTION, null });
		ret.add(new Object[] { "(i0 (#r10))", ExpectedError.ACTION, null });
		ret.add(new Object[] { "(e0 univ)(e0 none)", ExpectedError.ACTION, null });
		ret.add(new Object[] { "(f0 true)(f0 true)", ExpectedError.ACTION, null });
		ret.add(new Object[] { "(i0 1)(i0 2)", ExpectedError.ACTION, null });
	}

	private static void validDefs(Collection<Object[]> ret) {
		ret.add(new Object[] { "(e10 r5)", ExpectedError.NONE, env('e',"10",r[5]) });
		ret.add(new Object[] { "(f100 (let ([f100 (some r2)]) f100))", ExpectedError.NONE, env('f', "100", r[2].some()) });
		ret.add(new Object[] { "(i0 (sum r1))", ExpectedError.NONE, env('i', "0", r[1].sum()) });
		final DefEnv env = new DefEnv();
		env.def('e', "10", r[5]);
		env.def('f', "0", r[5].some());
		env.def('i', "100", r[5].some().thenElse(r[5].count(), IntConstant.constant(3)));
		ret.add(new Object[] { "(e10 r5)(f0 (some e10))(i100 (ite f0 (# e10) 3))", ExpectedError.NONE, env });
	}

	private static DefEnv env(char reg, String name, Node val) {
		final DefEnv env = new DefEnv();
		env.def(reg, name, val);
		return env;
	}

	@Test
	public void test() {
		final ParsingResult<?> result = checkExpectedErrors(expectedError, parse(input));
		if (expectedError != ExpectedError.NONE) return;
		assertEquals(0, result.valueStack.size());
		final DefEnv actual = globals();
		for(char reg : new char[]{'e', 'f', 'i'}) {
			assertEquals(expectedEnv.keys(reg), actual.keys(reg));
			for(String name : expectedEnv.keys(reg)) {
				Node en = null, an = null;
				try {
					en = expectedEnv.use(reg, name);
				} catch (ActionException e) {} // ignore
				try {
					an = actual.use(reg, name);
				} catch (ActionException e) {} // ignore
				assertEquals(String.valueOf(en), String.valueOf(an));
			}
		}
	}

}
