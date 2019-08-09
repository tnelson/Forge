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

import kodkod.instance.TupleSet;
import kodkod.util.ints.IndexedEntry;
import kodkod.util.ints.IntSet;
import kodkod.util.ints.Ints;
import kodkod.util.ints.SparseSequence;
import kodkod.util.ints.TreeSequence;

import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.Parameterized;
import org.junit.runners.Parameterized.Parameters;
import org.parboiled.Parboiled;
import org.parboiled.support.ParsingResult;

/**
 * Tests for the {@link KodkodParser#DeclareInts()} rules.
 * @author Emina Torlak
 *
 */
@RunWith(Parameterized.class)
public class IntsRuleTest extends AbstractParserTest {
	private final String input;
	private final ExpectedError expectedError;
	private final SparseSequence<IntSet> expectedBounds;
	
	public IntsRuleTest(String input, ExpectedError expectedError, SparseSequence<IntSet> expectedBounds) {
		this.input = input;
		this.expectedError = expectedError;
		this.expectedBounds = expectedBounds;
//		System.out.println(input);
	}

	@Before 
	public void setUpParser() {
		final KodkodParser parser = Parboiled.createParser(KodkodParser.class);
		setUp(parser, parser.Sequence(parser.DeclareUniverse(), parser.DeclareInts(), KodkodParser.EOI));
	}
	
	@Parameters
	public static Collection<Object[]> inputs() {
		final Collection<Object[]> ret = new ArrayList<Object[]>();
		badSyntax(ret);
		badSemantics(ret);
		validBounds(ret);
		return ret;
	}
	
	private static void badSyntax(Collection<Object[]> ret) {
		ret.add(new Object[] { "(univ 10)()", ExpectedError.PARSE, null });
		ret.add(new Object[] { "(univ 10)(define)", ExpectedError.PARSE, null });
		ret.add(new Object[] { "(univ 10)(ints)", ExpectedError.PARSE, null });
		ret.add(new Object[] { "(univ 10)(ints [])", ExpectedError.PARSE, null });
		ret.add(new Object[] { "(univ 10)(ints [()])", ExpectedError.PARSE, null });
		ret.add(new Object[] { "(univ 10)(ints [(1)])", ExpectedError.PARSE, null });
		ret.add(new Object[] { "(univ 10)(ints [(0 -1)])", ExpectedError.PARSE, null });
		ret.add(new Object[] { "(univ 10)(ints [(0 1 2)])", ExpectedError.PARSE, null });
	}

	private static void badSemantics(Collection<Object[]> ret) {
		ret.add(new Object[] { "(univ 10)(ints [(-1 10)])", ExpectedError.ACTION, null });
		ret.add(new Object[] { "(univ 10)(ints [(-1 10)])", ExpectedError.ACTION, null });
		ret.add(new Object[] { "(univ 10)(ints [(-1 0)(2 1)(-1 1)])", ExpectedError.ACTION, null });
		ret.add(new Object[] { "(univ 10)(ints [(-1 0)(2 1)(3 0)])", ExpectedError.ACTION, null });
	}
	
	private static void validBounds(Collection<Object[]> ret) {	 
		validBounds(ret, 10, -1, 9);
		validBounds(ret, 1, 100, 0);
		validBounds(ret, 10, Integer.MIN_VALUE, 9, 1000, 0, Integer.MAX_VALUE, 5);
		validBounds(ret, 3, -1, 0, 1000, 1, -400, 2);
	}
	
	private static void validBounds(Collection<Object[]> ret, int size, int ... intBounds) {
		final StringBuilder input = new StringBuilder();
		input.append("(univ ").append(size).append(")");
		final SparseSequence<IntSet> expected = new TreeSequence<>();
		input.append("(ints [");
		for(int i = 0; i < intBounds.length; i += 2) {
			input.append("(");
			input.append(intBounds[i]);
			input.append(" ");
			input.append(intBounds[i+1]);
			input.append(")");
			expected.put(intBounds[i], Ints.singleton(intBounds[i+1]));
		}
		input.append("])");
		ret.add(new Object[]{ input.toString(), ExpectedError.NONE, expected});
	}
			
	@Test
	public void test() {
		final ParsingResult<?> result = checkExpectedErrors(expectedError, parse(input));
		if (expectedError != ExpectedError.NONE) return;
		assertEquals(0, result.valueStack.size());
		final SparseSequence<TupleSet> actual = bounds().intBounds();
		assertEquals(expectedBounds.size(), actual.size());
		for(IndexedEntry<IntSet> e : expectedBounds) {
			assertEquals(e.value(), actual.get(e.index()).indexView());
		}
	}
	
}
