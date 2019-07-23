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

import kodkod.ast.Formula;
import kodkod.ast.Relation;

import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.Parameterized;
import org.junit.runners.Parameterized.Parameters;
import org.parboiled.Parboiled;
import org.parboiled.support.ParsingResult;

/**
 * Tests for the {@link KodkodParser#Assert()} rules.
 * @author Emina Torlak
 */
@RunWith(Parameterized.class)
public class AssertRuleTest extends AbstractParserTest {
	private static final Relation[] r = { 
		Relation.unary("r0"), Relation.unary("r1"),
		Relation.binary("r2"), Relation.binary("r3"),
		Relation.ternary("r4"), Relation.ternary("r5")
	};
	private final String input;
	private final ExpectedError expectedError;
	private final Formula expectedAsserts;
	
	public AssertRuleTest(String input, ExpectedError expectedError, Formula expectedAsserts) {
		this.input = input;
		this.expectedError = expectedError;
		this.expectedAsserts = expectedAsserts;
//		System.out.println(input);
	}
	
	@Before
	public void setUp() throws Exception {
		final KodkodParser parser = Parboiled.createParser(KodkodParser.class);
		setUp(parser, parser.Sequence(parser.DeclareUniverse(),parser.DeclareInts(),parser.OneOrMore(parser.DeclareRelation())));
		parse("(univ 10)" + 
			  "(ints [(1 0) (2 1) (4 2) (-8 3)])" + 
			  "(r0 [{} ints])" + 
			  "(r1 [{} univ])" + 
			  "(r2 [(-> none none) (-> ints univ)])" + 
			  "(r3 [(-> none none) (-> univ ints)])" + 
			  "(r4 [(-> none none none) (-> ints ints ints)])" +
			  "(r5 [(-> none none none) (-> univ ints univ)])");
		setUp(parser, parser.Sequence(parser.ZeroOrMore(parser.DefNode()),parser.OneOrMore(parser.Assert()), KodkodParser.EOI));
	}
	
	@Parameters
	public static Collection<Object[]> inputs() {
		final Collection<Object[]> ret = new ArrayList<Object[]>();
		badSyntax(ret);
		badSemantics(ret);
		validAsserts(ret);
		return ret;
	}
	
	private static void badSyntax(Collection<Object[]> ret) {
		ret.add(new Object[] { "()", ExpectedError.PARSE, null });
		ret.add(new Object[] { "(assert)", ExpectedError.PARSE, null });
		ret.add(new Object[] { "(assert v0)", ExpectedError.PARSE, null });
		ret.add(new Object[] { "(assert r0)", ExpectedError.PARSE, null });
		ret.add(new Object[] { "(assert g0)", ExpectedError.PARSE, null });
		ret.add(new Object[] { "(assert e0)", ExpectedError.PARSE, null });
		ret.add(new Object[] { "(assert univ)", ExpectedError.PARSE, null });
		ret.add(new Object[] { "(assert 1)", ExpectedError.PARSE, null });
		ret.add(new Object[] { "(assert i0)", ExpectedError.PARSE, null });
		ret.add(new Object[] { "(assert true false)", ExpectedError.PARSE, null });
	}

	private static void badSemantics(Collection<Object[]> ret) {
		ret.add(new Object[] { "(assert f0)", ExpectedError.ACTION, null });
	}
	
	private static void validAsserts(Collection<Object[]> ret) {
		ret.add(new Object[] { "(f100 true)(assert f100)", ExpectedError.NONE, Formula.TRUE });
		ret.add(new Object[] { "(f50 false)(assert f50)", ExpectedError.NONE, Formula.FALSE });
		final StringBuilder subIn = new StringBuilder();
		final Collection<Formula> subExpected = new ArrayList<>();
		int f = 0;
		for(Relation leaf : r) {
			subIn.append("(f").append(f++).append(" (lone ").append(leaf.toString()).append("))");
			subExpected.add(leaf.lone());
		}
		for(int i = 0; i < r.length; i+= 2) {
			subIn.append("(f").append(f++).append(" (in ").append(r[i].toString()).append(" ").append(r[i+1]).append("))");
			subExpected.add(r[i].in(r[i+1]));
			subIn.append("(f").append(f++).append(" (!(= ").append(r[i].toString()).append(" ").append(r[i+1]).append(")))");
			subExpected.add(r[i].eq(r[i+1]).not());
		}
		subIn.append("(assert");
		for(int i = 0; i < f; i++) {
			subIn.append(" f").append(i);
		}
		subIn.append(")");
		ret.add(new Object[] { subIn.toString(), ExpectedError.NONE, Formula.and(subExpected) });
	}
	
	
	@Test
	public void test() {
		final ParsingResult<?> result = checkExpectedErrors(expectedError, parse(input));
		if (expectedError != ExpectedError.NONE) return;
		assertEquals(0, result.valueStack.size());
		assertEquals(expectedAsserts.toString(), asserts().toString());
	}

}
