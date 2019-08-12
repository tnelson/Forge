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

import kodkod.engine.config.Options;
import kodkod.engine.satlab.SATFactory;

import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.Parameterized;
import org.junit.runners.Parameterized.Parameters;
import org.parboiled.Parboiled;
import org.parboiled.support.ParsingResult;

/**
 * Tests for the {@link KodkodParser#Configure()} rules.
 * @author Emina Torlak
 */
@RunWith(Parameterized.class)
public final class ConfigureRuleTest extends AbstractParserTest {
	private final String input;
	private final ExpectedError expectedError;
	private final Options expectedOpts;

	public ConfigureRuleTest(String input, ExpectedError expectedError, Options expectedOpts) {
		this.input = input;
		this.expectedError = expectedError;
		this.expectedOpts = expectedOpts;
//		System.out.println(input);
	}

	@Before
	public void setUpParser() {
		final KodkodParser parser = Parboiled.createParser(KodkodParser.class);
		setUp(parser, parser.Sequence(parser.Configure(),KodkodParser.EOI));
	}

	@Parameters
	public static Collection<Object[]> inputs() {
		final Collection<Object[]> ret = new ArrayList<Object[]>();
		badSyntax(ret);
		badSemantics(ret);
		validConfigs(ret);
		return ret;
	}

	private static void badSyntax(Collection<Object[]> ret) {
		ret.add(new Object[] { "(configure)", ExpectedError.PARSE, null });
		ret.add(new Object[] { "(configure)", ExpectedError.PARSE, null });
		ret.add(new Object[] { "(configure :solver )", ExpectedError.PARSE, null });
		ret.add(new Object[] { "(configure :solver Foo)", ExpectedError.PARSE, null });
		ret.add(new Object[] { "(configure :bitwidth )", ExpectedError.PARSE, null });
		ret.add(new Object[] { "(configure :bitwidth -1)", ExpectedError.PARSE, null });
		ret.add(new Object[] { "(configure :produce-cores )", ExpectedError.PARSE, null });
		ret.add(new Object[] { "(configure :produce-cores 1)", ExpectedError.PARSE, null });
	}

	private static void badSemantics(Collection<Object[]> ret) {
		ret.add(new Object[] { "(configure :bitwidth 0)", ExpectedError.ACTION, null });
		ret.add(new Object[] { "(configure :bitwidth 33)", ExpectedError.ACTION, null });
	}

	private static void validConfigs(Collection<Object[]> ret) {
		final Object[][] supported = {	{"MiniSat", SATFactory.MiniSat},
										//{"MiniSatProver", SATFactory.MiniSatProver},
										//{"Glucose", SATFactory.Glucose},
										{"Lingeling", SATFactory.Lingeling},
										{"SAT4J",SATFactory.DefaultSAT4J}};
		for(Object[] solver : supported) {
			if (SATFactory.available((SATFactory)solver[1]))
				ret.add(new Object[] { "(configure :solver "+solver[0]+")", ExpectedError.NONE, setSolver(new Options(), (SATFactory)solver[1]) });
		}
		ret.add(new Object[] { "( configure :bitwidth 1 )", ExpectedError.NONE, setBitwidth(new Options(), 1) });
		ret.add(new Object[] { "( configure :bitwidth 7 )", ExpectedError.NONE, setBitwidth(new Options(), 7) });
		ret.add(new Object[] { "( configure :bitwidth 32 )", ExpectedError.NONE, setBitwidth(new Options(), 32) });
		ret.add(new Object[] { "( configure :produce-cores false )", ExpectedError.NONE, disableCore(new Options()) });
		if (SATFactory.available(SATFactory.MiniSatProver)) {
			ret.add(new Object[] { "( configure :produce-cores true )", ExpectedError.NONE, enableCore(new Options()) });
			if (SATFactory.available(SATFactory.Glucose)) {
				ret.add(new Object[] {	"(configure :produce-cores true\t)\n" +
										"(configure :bitwidth 32)\n" +
										"(configure :solver Glucose)\n", ExpectedError.NONE,
										setSolver(setBitwidth(enableCore(new Options()), 32), SATFactory.Glucose) });
				ret.add(new Object[] {	"(configure :produce-cores true :bitwidth 32 :solver Glucose)\n",
						ExpectedError.NONE,  setSolver(setBitwidth(enableCore(new Options()), 32), SATFactory.Glucose) });
			}
		} else {
			if (SATFactory.available(SATFactory.Glucose)) {
				ret.add(new Object[] {	"(configure :produce-cores false\t)\n" +
										"(configure :bitwidth 16)\n" +
										"(configure :bitwidth 32)\n", ExpectedError.NONE,
										setBitwidth(disableCore(new Options()), 32) });
				ret.add(new Object[] {	"(configure :produce-cores false :bitwidth 16 :bitwidth 32)\n", ExpectedError.NONE,
						setBitwidth(disableCore(new Options()), 32) });
			}
		}
	}

	private static Options setSolver(Options opts, SATFactory factory) {
		opts.setSolver(factory);
		return opts;
	}
	private static Options setBitwidth(Options opts, int bw) {
		opts.setBitwidth(bw);
		return opts;
	}
	private static Options enableCore(Options opts) {
		assert (SATFactory.available(SATFactory.MiniSatProver)) ;
		 opts.setLogTranslation(1);
		 opts.setCoreGranularity(0);
		 opts.setSolver(SATFactory.MiniSatProver);
		return opts;
	}
	private static Options disableCore(Options opts) {
		opts.setLogTranslation(0);
		opts.setCoreGranularity(0);
		return opts;
	}

	@Test
	public void test() {
		final ParsingResult<?> result = checkExpectedErrors(expectedError, parse(input));
		if (expectedError != ExpectedError.NONE) return;
		assertEquals(0, result.valueStack.size());
		final Options actual = options();
		assertEquals(expectedOpts.bitwidth(), actual.bitwidth());
		assertEquals(expectedOpts.solver(), actual.solver());
		assertEquals(expectedOpts.logTranslation(), actual.logTranslation());
		assertEquals(expectedOpts.coreGranularity(), actual.coreGranularity());
		assertEquals(expectedOpts.intEncoding(), actual.intEncoding());
		assertEquals(expectedOpts.sharing(), actual.sharing());
		assertEquals(expectedOpts.skolemDepth(), actual.skolemDepth());
		assertEquals(expectedOpts.symmetryBreaking(), actual.symmetryBreaking());
	}
}
