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
import java.util.Arrays;
import java.util.Collection;
import java.util.List;

import kodkod.instance.TupleFactory;
import kodkod.instance.TupleSet;
import kodkod.instance.Universe;
import kodkod.util.ints.IntSet;
import kodkod.util.ints.Ints;

import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.Parameterized;
import org.junit.runners.Parameterized.Parameters;
import org.parboiled.Parboiled;
import org.parboiled.support.ParsingResult;

/**
 * Tests for the {@link KodkodParser#TupleSet()} rules.
 * @author Emina Torlak
 */
@RunWith(Parameterized.class)
public class TupleSetRuleTest extends AbstractParserTest {
	private final String input;
	private final ExpectedError expectedError;
	private final int expectedArity;
	private final IntSet expectedIndices;

	public TupleSetRuleTest(String input, ExpectedError expectedError, int expectedArity, IntSet expectedIndices) {
		this.input = input;
		this.expectedError = expectedError;
		this.expectedArity = expectedArity;
		this.expectedIndices = expectedIndices;
		//System.out.println(input);
		//System.out.println()
	}

	@Before
	public void setUpParser() {
		final KodkodParser parser = Parboiled.createParser(KodkodParser.class);
		setUp(parser, parser.Sequence(parser.DeclareUniverse(),parser.DeclareInts()));
		parse("(univ 10)(ints [(1 0) (2 1) (4 2) (-8 3)])");
		setUp(parser, parser.TupleSet());
	}

	@Parameters
	public static Collection<Object[]> inputs() {
		final Collection<Object[]> ret = new ArrayList<Object[]>();
		badSyntax(ret);
		badSemantics(ret);
		validTupleEnumerations(ret);

		validBuiltInConstants(ret);

		// OK so this guy like combines with previosu things.
		validTupleSetExprs(ret);
		return ret;
	}

	private static void badSyntax(Collection<Object[]> ret) {
		ret.add(new Object[] { "()", ExpectedError.PARSE, 0, null });
		ret.add(new Object[] { "(+)", ExpectedError.PARSE, 0, null });
		ret.add(new Object[] { "(&)", ExpectedError.PARSE, 0, null });
		ret.add(new Object[] { "(->)", ExpectedError.PARSE, 0, null });
		ret.add(new Object[] { "(-)", ExpectedError.PARSE, 0, null });
		ret.add(new Object[] { "{ (0) ... }", ExpectedError.PARSE, 0, null });
		ret.add(new Object[] { "{ ... (0) }", ExpectedError.PARSE, 0, null });
		ret.add(new Object[] { "{ (0) # }", ExpectedError.PARSE, 0, null });
		ret.add(new Object[] { "{ # (0) }", ExpectedError.PARSE, 0, null });
	}

	private static void badSemantics(Collection<Object[]> ret) {
		ret.add(new Object[] { "{ (10) }", ExpectedError.ACTION, 0, null });
		ret.add(new Object[] { "{ (0) (10) }", ExpectedError.ACTION, 0, null });
		ret.add(new Object[] { "{ (0 9) (5) }", ExpectedError.ACTION, 0, null });
		ret.add(new Object[] { "{ (4 3) (0 10) }", ExpectedError.ACTION, 0, null });
		ret.add(new Object[] { "{ (1 1) ... (0 0) }", ExpectedError.ACTION, 0, null });
		ret.add(new Object[] { "{ (2 2 2) # (0 0 0) }", ExpectedError.ACTION, 0, null });
		ret.add(new Object[] { "(+ univ iden)", ExpectedError.ACTION, 0, null });
		ret.add(new Object[] { "(& iden none)", ExpectedError.ACTION, 0, null });
		ret.add(new Object[] { "(- none iden)", ExpectedError.ACTION, 0, null });
	}

	private static void validTupleEnumerations(Collection<Object[]> ret) {
		ret.add(new Object[] { "{}", ExpectedError.NONE, 1, Ints.EMPTY_SET });
		ret.add(new Object[] { "{(0)}", ExpectedError.NONE, 1, Ints.singleton(0) });
		ret.add(new Object[] { "{(9)}", ExpectedError.NONE, 1, Ints.singleton(9) });
		ret.add(new Object[] { "{(1)(0) (5)}", ExpectedError.NONE, 1, Ints.asSet(new int[]{0, 1, 5}) });
		ret.add(new Object[] { "{(1 9)(0 0) (2 5) (3 7)}", ExpectedError.NONE, 2, Ints.asSet(new int[]{0, 19, 25, 37}) });
		ret.add(new Object[] { "{(0)...(0)}", ExpectedError.NONE, 1, Ints.singleton(0) });
		ret.add(new Object[] { "{(9) ... (9)}", ExpectedError.NONE, 1, Ints.singleton(9) });
		ret.add(new Object[] { "{(3)...(8)}", ExpectedError.NONE, 1, Ints.rangeSet(Ints.range(3, 8)) });
		ret.add(new Object[] { "{(0 3)...(2 8)}", ExpectedError.NONE, 2, Ints.rangeSet(Ints.range(3, 28)) });
		ret.add(new Object[] { "{(0)#(0)}", ExpectedError.NONE, 1, Ints.singleton(0) });
		ret.add(new Object[] { "{(9) # (9)}", ExpectedError.NONE, 1, Ints.singleton(9) });
		ret.add(new Object[] { "{(3)#(8)}", ExpectedError.NONE, 1, Ints.rangeSet(Ints.range(3, 8)) });
		ret.add(new Object[] { "{(1 1 1)#(3 3 3)}", ExpectedError.NONE, 3,
								Ints.asSet(new int[]{111, 112, 113, 121, 122, 123, 131, 132, 133,
													 211, 212, 213, 221, 222, 223, 231, 232, 233,
													 311, 312, 313, 321, 322, 323, 331, 332, 333}) });
	}

	private static void validBuiltInConstants(Collection<Object[]> ret) {
		ret.add(new Object[] { "none", ExpectedError.NONE, 1, Ints.EMPTY_SET });
		ret.add(new Object[] { "univ", ExpectedError.NONE, 1, Ints.rangeSet(Ints.range(0, 9)) });
		ret.add(new Object[] { "ints", ExpectedError.NONE, 1, Ints.rangeSet(Ints.range(0, 3)) });
		ret.add(new Object[] { "iden", ExpectedError.NONE, 2,
								Ints.asSet(new int[]{0, 11, 22, 33, 44, 55, 66, 77, 88, 99})});
	}

	private static void validTupleSetExprs(Collection<Object[]> ret) {
		final String[] ops = { "+", "&", "-", "->" };
		final List<Object[]> valid = new ArrayList<>();
		final List<Object[]> composite = new ArrayList<>();

		// So this just makes a bunch of unary operator expressions
		// with each valid expression in ret.
		for(Object[] test : ret) {
			if (test[1]==ExpectedError.NONE) {
				valid.add(test);
				for(String op : ops) {
					final Object[] alt = Arrays.copyOf(test, test.length);
					alt[0] = "(" + op + alt[0] + ")";
					composite.add(alt);
				}
			}
		}
		ret.addAll(composite);
		valid.addAll(composite);
		composite.clear();

		// ALL of the issues come from this section.
		final TupleFactory f = (new Universe(0,1,2,3,4,5,6,7,8,9)).factory();
		// These are just the locations of arity and indices in the object lists we're using.
		final int arity = 2, indices = 3;

		// ok so the problem is in here
		for(Object[] left : valid) {
			// Just duplicates the tupleset that left represents.
			final TupleSet lts = f.setOf((Integer)left[arity], (IntSet)left[indices]);
			int op = 0;
			for(Object[] right : valid) {
				final TupleSet rts = f.setOf((Integer)right[arity], (IntSet)right[indices]);
				final TupleSet prod = lts.product(rts);

				// So we're just calculating the relational product of left and right.
				Object[] alt = { "( -> " + left[0] + " " + right[0] + ")",
								 ExpectedError.NONE, prod.arity(), prod.indexView() };
				composite.add(alt);

				
				if (lts.arity()==rts.arity()) {
					final TupleSet opSet = f.noneOf(lts.arity());
					opSet.addAll(lts);
					switch(ops[op]) {
					case "+" : opSet.addAll(rts); break;
					case "&" : opSet.retainAll(rts); break;
					case "-" : opSet.removeAll(rts); break;
					default  : throw new AssertionError("unreachable");
					}
					alt = new Object[]{ "(" +ops[op] + " " + left[0] + " " + right[0] + ")",
							 			ExpectedError.NONE, opSet.arity(), opSet.indexView() };
					composite.add(alt);
					op = (op+1)%3;
				}
			}
		}

		ret.addAll(composite);
		composite.clear();
		// problem section ends here


		ret.add(new Object[] { "(+univ ints none)", ExpectedError.NONE, 1, Ints.rangeSet(Ints.range(0, 9)) });
		ret.add(new Object[] { "(&univ none ints)", ExpectedError.NONE, 1, Ints.EMPTY_SET });
		ret.add(new Object[] { "(-univ none ints)", ExpectedError.NONE, 1, Ints.rangeSet(Ints.range(4, 9)) });
		ret.add(new Object[] { "(->none univ ints)", ExpectedError.NONE, 3, Ints.EMPTY_SET });
	}

	@Test
	public void testTupleSet() {
		final ParsingResult<?> result = checkExpectedErrors(expectedError, parse(input));
		// So if there was an error, whether we expected that error or not, return.
		if (expectedError != ExpectedError.NONE) return;

		boolean succ = true;
		if (1 != result.valueStack.size()) printOut(input);
		assertEquals(1, result.valueStack.size());
		assertTrue(result.valueStack.peek() instanceof TupleSet);
		final TupleSet ts = (TupleSet) result.valueStack.pop();
		// System.out.println(input);
		// System.out.println(ts.toString());
		assertEquals(bounds().universe(), ts.universe());
		assertEquals(expectedArity, ts.arity());
		assertEquals(expectedIndices, ts.indexView());
	}

	public void printOut(String input){
		System.out.println("Input: " + input);
	}

	public void printOut(String input, TupleSet ts){
		System.out.println("Input: " + input);
		System.out.println("Output tupleset: " + ts);
	}
}
