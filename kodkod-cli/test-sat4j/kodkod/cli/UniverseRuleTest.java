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

import org.junit.Before;
import org.junit.Test;
import org.parboiled.Parboiled;

/**
 * Tests for the {@link KodkodParser#DeclareUniverse()} rules.
 * @author Emina Torlak
 */
public class UniverseRuleTest extends AbstractParserTest {

	public UniverseRuleTest() { }

	@Before 
	public void setUpParser() {
		final KodkodParser parser = Parboiled.createParser(KodkodParser.class);
		setUp(parser, parser.DeclareUniverse());
	}
	
	@Test
	public void testBadSyntax() {
		checkParseError(parse("(univ)"));
		checkParseError(parse("(univ -1)"));
		checkParseError(parse("(univ 2 [])"));
		checkParseError(parse("(univ 2 foo)"));
	}

	@Test
	public void testBadSemantics() {
		checkActionError(parse("(univ 0)"));
	}
	
	@Test
	public void testValidDeclarationSingleton() {
		checkNoErrors(parse("(univ 1)"));
	}
	
	@Test
	public void testValidDeclaration() {
		checkNoErrors(parse("(univ 10)"));
	}
	
	@Test
	public void testValidDeclarationWithExtraSpaces() {
		checkNoErrors(parse("(univ    100 ) "));
	}

	@Test
	public void testMultipleUniverseDeclarations() {
		checkNoErrors(parse("(univ 10)"));
		checkActionError(parse("(univ 10)"));
		checkActionError(parse("(univ 1)"));
	}

}
