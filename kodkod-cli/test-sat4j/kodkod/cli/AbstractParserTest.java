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

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import java.text.NumberFormat;
import java.util.Iterator;

import kodkod.ast.Formula;
import kodkod.ast.Relation;
import kodkod.engine.config.Options;
import kodkod.instance.Bounds;

import org.junit.BeforeClass;
import org.parboiled.Parboiled;
import org.parboiled.Rule;
import org.parboiled.errors.ActionError;
import org.parboiled.errors.BasicParseError;
import org.parboiled.errors.ParseError;
import org.parboiled.parserunners.ErrorLocatingParseRunner;
import org.parboiled.support.ParsingResult;

/**
 * Tests for a rule in the Kodkod parser.
 *
 * @specfield parser: {@link KodkodParser}
 * @specfield rule: {@link Rule}
 *
 * @author Emina Torlak
 */
public abstract class AbstractParserTest {
	private KodkodParser parser;
	private Rule rule;

	/**
	 * Creates an uninitialized parser test.
	 */
	AbstractParserTest() {
		this.parser = null;
		this.rule = null;
	}

	/**
	 * Loads and instruments the parser class.
	 * @throws java.lang.Exception
	 */
	@BeforeClass
	public static void setUpBeforeClass() throws Exception {
		 Parboiled.createParser(KodkodParser.class);
	}


	/**
	 * Sets the parser and rule to be tested.  The rule must have been obtained from the parser.
	 * @ensures this.parser' = parser && this.rule' = rule
	 */
	final void setUp(KodkodParser parser, Rule rule) {
		this.parser = parser;
		this.rule = rule;
	}

	/**
	 * Runs {@code this.rule} on the given input using the {@link ErrorLocatingParseRunner} and returns the result.
	 * @return the result of running {@code this.rule} with the {@link ErrorLocatingParseRunner} on the given input.
	 */
	final ParsingResult<?> parse(String input) {
		//System.out.println(input);
		return (new ErrorLocatingParseRunner<Object>(rule)).run(input);
	}

	/**
	 * Returns {@code this.parser}.
	 * @return this.parser
	 */
	final KodkodParser parser() { return parser; }

	/**
	 * Returns {@code this.parser.problem().options}.
	 * @return this.parser.problem().options
	 */
	final Options options() { return parser.problem().options(); }

	/**
	 * Returns {@code this.parser.problem().bounds}.
	 * @return this.parser.problem().bounds
	 */
	final Bounds bounds() { return parser.problem().bounds(); }

	/**
	 * Returns {@code this.parser.problem().globals()}.
	 * @return this.parser.problem().globals()
	 */
	final DefEnv globals() { return parser.problem().env(); }

	/**
	 * Returns {@code this.parser.problem().asserts()}.
	 * @return this.parser.problem().asserts()
	 */
	final Formula asserts() { return parser.problem().asserts(); }
	/**
	 * Returns the relation with the given name that is in {@code this.parser.problem().bounds},
	 * or throws an exception if such a relation is not present.
	 * @return this.parser.problem().globals().use(name)
	 */
	final Relation declared(String name) { return parser.problem().env().use(name); }

	static enum ExpectedError {
		NONE, ACTION, PARSE
	}

	/**
	 * Checks that the given result has the expected error status.
	 */
	final ParsingResult<?> checkExpectedErrors(ExpectedError expected, ParsingResult<?> result) {
		switch(expected) {
		case NONE   : return checkNoErrors(result);
		case ACTION : return checkActionError(result);
		case PARSE  : return checkParseError(result);
		default     : throw new AssertionError("Unreachable code");
		}
	}
	/**
	 * Checks that the given result contains no errors.
	 */
	final ParsingResult<?> checkNoErrors(ParsingResult<?> result) {
//		System.out.println("\nParse Errors:\n" + ErrorUtils.printParseErrors(result));
		// if (result.hasErrors()){
		// 	System.out.println(result);
		// }


				if (result.hasErrors()){
					System.out.println(result.parseErrors);
				}
		assertFalse(result.hasErrors());


		return result;
	}

	/**
	 * Checks that the given result contains a single {@link ActionError}
	 * and zero or more parser errors.
	 */
	final ParsingResult<?> checkActionError(ParsingResult<?> result) {
		assertTrue(result.hasErrors());
//		System.out.println("\nParse Errors:\n" + ErrorUtils.printParseErrors(result));
		final Iterator<ParseError> itr = result.parseErrors.iterator();
		assertTrue(itr.next() instanceof ActionError);
		while(itr.hasNext()) {
			assertFalse(itr.next() instanceof ActionError);
		}
		return result;
	}

	/**
	 * Checks that the given result contains only {@link BasicParseError parse errors}
	 * and no action errors.
	 */
	final ParsingResult<?> checkParseError(ParsingResult<?> result) {
		assertTrue(result.hasErrors());
		for(ParseError err : result.parseErrors)
			assertTrue(err instanceof BasicParseError);
		return result;
	}



	final void printMemInfo() {
	    	Runtime runtime = Runtime.getRuntime();
	        NumberFormat format = NumberFormat.getInstance();
	        StringBuilder sb = new StringBuilder();
	        long maxMemory = runtime.maxMemory();
	        long allocatedMemory = runtime.totalMemory();
	        long freeMemory = runtime.freeMemory();
	        sb.append("Free memory: ");
	        sb.append(format.format(freeMemory / 1024));
	        sb.append("\n");
	        sb.append("Allocated memory: ");
	        sb.append(format.format(allocatedMemory / 1024));
	        sb.append("\n");
	        sb.append("Max memory: ");
	        sb.append(format.format(maxMemory / 1024));
	        sb.append("\n");
	        sb.append("Total free memory: ");
	        sb.append(format.format((freeMemory + (maxMemory - allocatedMemory)) / 1024));
	        sb.append("\n");
	        System.out.println(sb);
	    }
}
