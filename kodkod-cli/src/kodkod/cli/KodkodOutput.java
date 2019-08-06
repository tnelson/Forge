/*
 * Kodkod -- Copyright (c) 2005-2012, Emina Torlak
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

import kodkod.engine.Solution;
import kodkod.ast.Relation;
import kodkod.instance.Instance;

import kodkod.engine.Proof;
import kodkod.ast.Formula;

/**
 * Specifies the interface for outputting {@link Solution solutions}
 * to {@link KodkodProblem Kodkod problems}.
 *
 * @author Emina Torlak
 */
public interface KodkodOutput {

	/**
	 * Outputs the given solution to the given problem.  The output format and
	 * destination are implementation dependent.
	 */
	public abstract void writeSolution(Solution sol, KodkodProblem problem);

	// WARNING HACK
	public abstract void writeInstance(Instance sol, Defs<Relation> defs);

	// WARNING HACK
	public void writeCore(Proof proof, Defs<Formula> defs);

 }
