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

import kodkod.ast.Relation;
import kodkod.instance.Bounds;
import kodkod.instance.TupleFactory;
import kodkod.instance.TupleSet;
import kodkod.instance.Universe;
import kodkod.util.ints.Ints;

import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.Parameterized;
import org.junit.runners.Parameterized.Parameters;
import org.parboiled.Parboiled;

/**
 * Tests for the {@link KodkodParser#DeclareRelation()} rules.
 * @author Emina Torlak
 */
@RunWith(Parameterized.class)
public class RelationRuleTest extends AbstractParserTest {
	private final String input;
	private final ExpectedError expectedError;
	private final Bounds expectedBounds;

	public RelationRuleTest(String input, ExpectedError expectedError, Bounds expectedBounds) {
		this.input = input;
		this.expectedError = expectedError;
		this.expectedBounds = expectedBounds;
		//System.out.println(input);
	}

	@Before
	public void setUpParser() {
		final KodkodParser parser = Parboiled.createParser(KodkodParser.class);
		setUp(parser, parser.Sequence(parser.DeclareUniverse(),parser.DeclareInts()));
		parse("(univ 10)(ints [(1 0) (2 1) (4 2) (-8 3)])");
		setUp(parser, parser.Sequence(parser.OneOrMore(parser.DeclareRelation()), KodkodParser.EOI));
	}

	@Parameters
	public static Collection<Object[]> inputs() {
		final Collection<Object[]> ret = new ArrayList<Object[]>();
		badSyntax(ret);
		badSemantics(ret);
		validBoundDeclarations(ret);
		return ret;
	}

	private static void badSyntax(Collection<Object[]> ret) {
		final String[] bad =  {
				"(define)", "(r)", "(r0 [])",
				"(r0 [{} :: some])", "(r0 [some :: univ])",
				"(r0 [none :: s])",
				"(s [{}])", "(all [{}])", "(1 [{}])"
		};
		for(String input : bad) {
			ret.add(new Object[] {input, ExpectedError.PARSE, null});
		}
	}

	static Bounds bound(String r, TupleSet low, TupleSet high, Bounds bounds) {
		bounds.bound(Relation.nary(r, low.arity()), low, high);
		return bounds;
	}

	static Bounds bound(String r, TupleSet low, TupleSet high) {
		final Bounds bounds = new Bounds(low.universe());
		bounds.bound(Relation.nary(r, low.arity()), low, high);
		return bounds;
	}

	static Bounds bound(String r, TupleSet val, Bounds bounds) {
		return bound(r, val, val, bounds);
	}

	static Bounds bound(String r, TupleSet val) {
		return bound(r, val, val);
	}

	static Bounds bound(TupleSet val, String...rs) {
		final Bounds bounds = new Bounds(val.universe());
		for(String r : rs) {
			bound(r, val, bounds);
		}
		return bounds;
	}

	static Bounds bound(TupleSet low, TupleSet high, String...rs) {
		final Bounds bounds = new Bounds(low.universe());
		for(String r : rs) {
			bound(r, low, high, bounds);
		}
		return bounds;
	}


	private static void badSemantics(Collection<Object[]> ret) {
		// original contained (r0 [univ :: ints]), which doesn't make sense because they're both unary relations,
		// so this is a perfectly valid bounds.
		final String[] bad =  {
				"(r0 [univ :: iden])", "(r0 [iden :: univ])", "(r0 [univ :: (-> ints ints)])",
				"(r0 [{} :: r1])", "(r0 [r1 :: univ])", "(r0 [r1 :: r1])", "(r0 [r1 :: r2])",
		};
		for(String input : bad) {
			ret.add(new Object[] {input, ExpectedError.ACTION, null});
		}
		final TupleFactory f = (new Universe(0,1,2,3,4,5,6,7,8,9)).factory();
		ret.add(new Object[] {"(r0 [ints])(r0 [ints])", ExpectedError.ACTION,
				bound("r0", f.setOf(1, Ints.rangeSet(Ints.range(0, 3))))});
		ret.add(new Object[] {"(r0 r0 [ints])", ExpectedError.ACTION,
				bound("r0", f.setOf(1, Ints.rangeSet(Ints.range(0, 3))))});
		ret.add(new Object[] {"(r0 [ints :: univ])(r0 [ints :: univ])", ExpectedError.ACTION,
				bound("r0", f.setOf(1, Ints.rangeSet(Ints.range(0, 3))), f.allOf(1))});
		ret.add(new Object[] {"(r0 [ints])(r0 [none :: univ])", ExpectedError.ACTION,
				bound("r0", f.setOf(1, Ints.rangeSet(Ints.range(0, 3))))});
		ret.add(new Object[] {"(r0 [(-> {}{}) :: (-> univ univ)])(r0 [none :: univ])", ExpectedError.ACTION,
				bound("r0", f.noneOf(2), f.allOf(2))});
		ret.add(new Object[] {"(r0 [none :: ints])(r1 [r0])", ExpectedError.ACTION,
				bound("r0", f.noneOf(1), f.setOf(1, Ints.rangeSet(Ints.range(0, 3))))});
	}

	private static void validBoundDeclarations(Collection<Object[]> ret) {
		final TupleFactory f = (new Universe(0,1,2,3,4,5,6,7,8,9)).factory();
		ret.add(new Object[] { "(r0 [{}])", ExpectedError.NONE, bound("r0", f.noneOf(1)) });
		ret.add(new Object[] { "(r0 [{}::{}])", ExpectedError.NONE, bound("r0", f.noneOf(1)) });
		ret.add(new Object[] { "(r1 [none])", ExpectedError.NONE, bound("r1", f.noneOf(1)) });
		ret.add(new Object[] { "(r1 [none :: {}])", ExpectedError.NONE, bound("r1", f.noneOf(1)) });
		ret.add(new Object[] { "(r1 [{} :: none])", ExpectedError.NONE, bound("r1", f.noneOf(1)) });
		ret.add(new Object[] { "(r2 [ints])", ExpectedError.NONE, bound("r2", f.setOf(0,1,2,3)) });
		ret.add(new Object[] { "(r2 [ints :: ints])", ExpectedError.NONE, bound("r2", f.setOf(0,1,2,3)) });
		ret.add(new Object[] { "(r2 [ints :: {(0)(1)(2)(3)}])", ExpectedError.NONE, bound("r2", f.setOf(0,1,2,3)) });
		ret.add(new Object[] { "(r2 [{(0)...(3)} :: ints])", ExpectedError.NONE, bound("r2", f.setOf(0,1,2,3)) });
		ret.add(new Object[] { "(r2 [(+ {(0)#(1)} {(1)#(3)}) :: ints])", ExpectedError.NONE, bound("r2", f.setOf(0,1,2,3)) });
		ret.add(new Object[] { "(r3 [univ])", ExpectedError.NONE, bound("r3", f.allOf(1)) });
		ret.add(new Object[] { "(r2 [{(0)...(3)} :: (& ints univ)])", ExpectedError.NONE, bound("r2", f.setOf(0,1,2,3)) });
		ret.add(new Object[] { "(r4 [iden])", ExpectedError.NONE,
								bound("r4", f.setOf(2, Ints.asSet(new int[]{0,11,22,33,44,55,66,77,88,99}))) });
		ret.add(new Object[] { "(r0 [univ])(r1 [r0])(r2 [r0 :: r1])(r3 [(+ r0 r1 r2)])",
								ExpectedError.NONE,
								bound(f.allOf(1), "r0", "r1", "r2", "r3") });
		ret.add(new Object[] { "(r0 [ints])(r1 [r0])(r2 r5 [r0 :: r1])(r3 r6 r7 [(+ r0 r1 r2)])(r4 [(- univ r3) :: univ])",
				ExpectedError.NONE,
				bound("r4", f.setOf(1, Ints.rangeSet(Ints.range(4,9))), f.allOf(1),
					  bound(f.setOf(0,1,2,3), "r0", "r1", "r2", "r3", "r5", "r6", "r7")) });
		ret.add(new Object[] { "(r0 r1 r2 [ints :: {(7)...(9)}])",
				ExpectedError.NONE,
				bound(f.setOf(0,1,2,3), f.setOf(0,1,2,3,7,8,9), "r0", "r1", "r2") });
	}


	@Test
	public void testDeclares() {
		checkExpectedErrors(expectedError, parse(input));
		if (expectedBounds == null) return;
		final Bounds actual = bounds();
		assertEquals(expectedBounds.relations().size(), actual.relations().size());
		for(Relation er : expectedBounds.relations()) {
			final Relation ar = declared(er.name());
			assertEquals(er.arity(), ar.arity());
			assertEquals(expectedBounds.lowerBound(er).indexView(), actual.lowerBound(ar).indexView());
			assertEquals(expectedBounds.upperBound(er).indexView(), actual.upperBound(ar).indexView());
		}
	}
}
