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

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.OutputStreamWriter;

import org.junit.Test;
import org.parboiled.support.Chars;


// ADDED
//import java.util.concurrent.TimeUnit;

/**
 * Tests the interaction with the {@link KodkodServer} class.
 *
 * @author Emina Torlak
 */
public class ServerTest {

	@Test
	public void myTest(){
		final Process p = call("");
		/*send(p, "(configure :bitwidth 5 :produce-cores false :max-solutions 1 :solver SAT4J :verbosity 6)\n"+
		        "(univ 5)\n" +
				"(ints [(1 0)(2 1) (4 2) (8 3) (-16 4)])\n" +
				"(r0 [(-> ints ints)])\n" +
				"(f0 (= r0 (* r0)))\n" +
				"(assert f0)\n" +
				"(solve)\n");*/

		/*send(p, "(configure :bitwidth 5 :produce-cores false :solver SAT4J :verbosity 0)"+
				"(univ 10)" +
				"(ints [(1 0)(2 1) (4 2) (8 3) (-16 4)])"+
				"(r0 [(+ ints {(5) ... (9)})])" +
				"(f0 (= r0 univ))"+
				"(f1 (= 4 (+ 2 2)))" +
				"(f2 (&& f0 f1))" +
				"(assert f2)" +
				"(solve)");*/

		send(p, "(configure :bitwidth 5 :produce-cores false :solver SAT4J :verbosity 0)"+
				"(univ 10)" +
				"(ints [(0 0) (1 1) (2 2) (3 3) (4 4) (5 5)]" +
				"(i0 (+ 2 5))" +
				"(f0 (= 7 i0))" +
				"(assert f0)" +
				"(solve)");

/*
(configure :bitwidth 5 :produce-cores false :solver SAT4J :verbosity 6)
(univ 10)
(ints [(0 0) (1 1) (2 2) (3 3) (4 4) (5 5)])
(r0 [ints])

(i0 (+ 2 5))
(r1 [{(1) (2) (4)}])

(f0 (= r1 (set i0)))
(assert f0)
(solve)



(configure :bitwidth 5 :produce-cores false :solver SAT4J :verbosity 6)
(univ 10)
(ints [(0 0) (1 1) (2 2) (3 3) (4 4) (5 5)])
(r0 [ints])

(i0 (+ 2 5))
(r1 [{(3)}])

(f0 (= r1 (lone (- i0 4))))
(assert f0)
(solve)
*/
		myreceive(p);
		p.destroy();
	}

	private boolean myreceive(Process p) {
		try {
			final BufferedReader pout = new BufferedReader(new InputStreamReader(p.getInputStream()));

			String msg = pout.readLine();
			while (!msg.equals("\0")){
				System.out.println(msg);
				msg = pout.readLine();
			}
			return false;

			/*
			if (msg.matches("\\(sat.*\\)"))
				return true;
			else if (msg.matches("\\(unsat.*\\)"))
				return false;
			else
				throw new AssertionError("Bad server output: " + msg);*/
		} catch (IOException e) {
			throw new AssertionError(e);
		}
	}

	//@Test
	public void testStandardSolver0() {
		final Process p = call("");
		/*
		send(p, "(configure :bitwidth 5 :produce-cores false :solver SAT4J :verbosity 0)"+
		        "(univ 5)" +
				"(ints [(1 0)(2 1) (4 2) (8 3) (-16 4)])"+
				"(r0 [none :: {(0) ... (0)}])"+
				"(f0 (some r0))"+
		        "(r1 [none :: {(0) ... (0)}])"+
				"(f1 (some r1))" +
				"(f2 (&& f0 f1))" +
				"(assert f2)" +
				"(solve)");*/

		send(p, "(configure :bitwidth 5 :produce-cores false :solver SAT4J :verbosity 0)"+
		        "(univ 10)" +
				"(ints [(1 0)(2 1) (4 2) (8 3) (-16 4)])"+
				"(r0 [(+ ints {(5) ... (9)})])" +
				"(f0 (= r0 univ))"+
				"(assert f0)" +
				"(solve)");
		assertTrue(receive(p));
		p.destroy();
	}

	//@Test
	public void testStandardSolver1() {
		final Process p = call("");
		send(p, "(configure :bitwidth 4 :solver SAT4J  :max-solutions 1  :verbosity 0)" +
				"(univ 10)" +
				"(ints [(1 0)(2 1) (4 2) (-8 3)])\n" +
				"(r0 r1 r2 [ none :: (+ ints { (4) ... (6) })])\n" +
				"(r3 [iden :: (-> univ univ)])\n" +
				"(i0 (sum r0))\n" +
				"(i1 (sum r1))" +
				"(f0 (< i0 i1))" +
				"(assert f0)" +
				"(e0 (- r2 ints))" +
				"(e1 (. e0 r3))" +
				"(f1 (lone e1))" +
				"(assert f1)" +
				"(solve)");

/*
(configure :bitwidth 4 :solver SAT4J  :max-solutions 1  :verbosity 6)
(univ 10)
(ints [(1 0)(2 1) (4 2) (-8 3)])
(r0 r1 r2 [ none :: (+ ints { (4) ... (6) })])
(r3 [iden :: (-> univ univ)])
(i0 (sum r0))
(i1 (sum r1))
(f0 (< i0 i1))
(assert f0)
(e0 (- r2 ints))
(e1 (. e0 r3))
(f1 (lone e1))
(assert f1)
(solve)
*/
		assertTrue(receive(p));
		send(p, "(configure :produce-cores false :verbosity 0)" +
				"(univ 10)" +
				"(ints [(1 0)(2 1) (4 2) (-8 3)])" +
				"(r0 r1 r2 [ none :: (+ ints { (4) ... (6) })])" +
				"(i0 (sum r0))" +
				"(i1 (sum r1))" +
				"(r3 [iden :: (-> univ univ)])" +
				"(f0 (< i0 i1))" +
				"(e0 (- r2 ints))" +
				"(e1 (. e0 r3))" +
				"(f1 (lone e1))" +
				"(f2 (some e1))" +
				"(f3 (no e1))" +
				"(assert f0 f1 f2 f3)" +
				"(solve)");
		assertFalse(receive(p));
		p.destroy();
	}

	//@Test
	public void testStandardSolver2() {
		final Process p = call("");
		send(p, "(configure :bitwidth 32 :produce-cores false :solver SAT4J :verbosity 0)" +
				"(univ 32)"+
				"(ints [(1 0)(2 1)(4 2)(8 3)(16 4)(32 5)(64 6)(128 7)(256 8)(512 9)(1024 10)(2048 11)(4096 12)(8192 13)(16384 14)(32768 15)(65536 16)(131072 17)(262144 18)(524288 19)(1048576 20)(2097152 21)(4194304 22)(8388608 23)(16777216 24)(33554432 25)(67108864 26)(134217728 27)(268435456 28)(536870912 29)(1073741824 30)(-2147483648 31)])" +
				"(r0 [none :: ints])" +
				"(i0 (sum r0))" +
				"(f0 (<= 0 i0))" +
				"(f1 (<= i0 32))" +
				"(f2 (&& f0 f1))" +
				"(assert f2)" +
				"(r1 [none :: ints])" +
				"(i1 (sum r1))" +
				"(r2 [none :: ints])" +
				"(i2 (sum r2))" +
				"(i3 (>> i1 i0))" +
				"(i4 (^ i1 i3))" +
				"(i5 (& i2 i4))" +
				"(i6 (<< i5 i0))" +
				"(i7 (^ i6 i5))" +
				"(i8 (^ i1 i7))" +
				"(i9 (^ i1 i5))" +
				"(i10 (^ i9 i6))" +
				"(f3 (= i8 i10))" +
				"(f4 (! f3))" +
				"(assert f4)" +
				"(solve)");
		assertFalse(receive(p));
		p.destroy();
	}

	//@Test
	public void testIncrementalSolver0() {
		final Process p = call("-incremental");
		final String ex =
				"(configure :bitwidth 5 :produce-cores false :solver SAT4J :verbosity 0)" +
				"(univ 5)" +
				"(r0 [none :: {(0) ... (0)}])" +
				"(f0 (some r0))" +
				"(assert f0)" +
				"(solve)";
		send(p, ex);
		assertTrue(receive(p));
		send(p, "(solve)");
		assertTrue(receive(p));
		send(p, "(clear)");
		send(p, ex);
		assertTrue(receive(p));
		p.destroy();
	}

	//@Test
	public void testIncrementalSolver1() {
		final Process p = call("-incremental");
		send(p, "(configure :bitwidth 5 :produce-cores false :solver SAT4J :verbosity 0)" +
				"(univ 5)" +
				"(ints [(1 0) (2 1) (4 2) (8 3) (-16 4)])" +
				"(r0 r1 [none :: {(0) ... (0)}])" +
				"(f0 (some r0))" +
				"(f1 (some r1))" +
				"(f2 (|| f0 f1))" +
				"(assert f2)" +
				"(solve)");
		assertTrue(receive(p));
		send(p, "(r2 [none :: {(0) ... (0)}])" +
				"(f3 (some r2))" +
				"(f4 (|| f0 f3))" +
				"(assert f4)" +
				"(solve)");
		assertTrue(receive(p));
		send(p, "(f5 (no r0))" +
				"(f6 (no r1))" +
				"(f7 (no r2))" +
				"(assert f5 f6 f7)" +
				"(solve)");
		assertFalse(receive(p));
		send(p, "(clear)");
		send(p, "(configure :bitwidth 5 :produce-cores false :solver SAT4J :verbosity 0)" +
				"(univ 5)" +
				"(r0 [none :: {(0) ... (0)}])" +
				"(f0 (no r0))" +
				"(assert f0)" +
				"(solve)");
		assertTrue(receive(p));
		p.destroy();
	}


	private Process call(String options) {
		final String cmd =
				"java -Xmx2G -cp " + System.getProperty("java.class.path") +
				" kodkod.cli.KodkodServer " + options;
		try {
			final Process p = Runtime.getRuntime().exec(cmd.split("\\s"));
			new Thread(drain(new BufferedReader(new InputStreamReader(p.getErrorStream())))).start();
			return p;
		} catch (IOException e) {
			throw new AssertionError(e);
		}
	}

	private void send(Process p, String msg) {
		try {
			final BufferedWriter pin = new BufferedWriter(new OutputStreamWriter(p.getOutputStream(), "UTF-8"));
			pin.write(msg);
			pin.append(Chars.EOI);
			pin.flush();
		} catch (IOException e) {
			throw new AssertionError(e);
		}
	}

	private boolean receive(Process p) {
		try {
			final BufferedReader pout = new BufferedReader(new InputStreamReader(p.getInputStream()));
			final String msg = pout.readLine();
			if (msg.matches("\\(sat.*\\)"))
				return true;
			else if (msg.matches("\\(unsat.*\\)"))
				return false;
			else
				throw new AssertionError("Bad server output: " + msg);
		} catch (IOException e) {
			throw new AssertionError(e);
		}
	}

	/**
	 * Returns a runnable that drains the specified input stream.
	 * @return a runnable that drains the specified input stream.
	 */
	private static Runnable drain(final BufferedReader input) {
		return new Runnable() {
			public void run() {
				try {
					String line = null;
					while((line = input.readLine()) != null) {
						System.err.println(line);
					}
				} catch (IOException ex) {
				} finally {
					try {
						input.close();
					} catch (IOException e) { }
				}
			}
		};
	}
}
