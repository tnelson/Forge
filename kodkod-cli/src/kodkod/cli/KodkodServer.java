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

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.Reader;
import java.util.logging.ConsoleHandler;
import java.util.logging.Handler;
import java.util.logging.Logger;

import kodkod.engine.IncrementalSolver;

import org.parboiled.Parboiled;
import org.parboiled.Rule;
import org.parboiled.buffers.DefaultInputBuffer;
import org.parboiled.buffers.InputBuffer;
import org.parboiled.buffers.InputBufferUtils;
import org.parboiled.errors.ErrorUtils;
import org.parboiled.errors.ParseError;
import org.parboiled.parserunners.BasicParseRunner;
import org.parboiled.parserunners.ErrorLocatingParseRunner;
import org.parboiled.support.Chars;
import org.parboiled.support.ParsingResult;

/**
 * Provides a server interface to Kodkod.  A {@link KodkodServer} can be used
 *  either in batch or online mode.  In batch more, a set of problem
 * specifications is read from a file and {@link KodkodParser parsed} into a
 * sequence of {@link KodkodProblem problems}.  Any commands that
 * are specified for a given problem are executed as soon as the problem is
 * constructed and before subsequent problems are parsed.  Once all problems
 * have been processed, the batch process exits.  The process may also terminate
 * early due to parsing, construction or solving errors.  All diagnostic and
 * error messages are logged to the {@link Logger#getGlobal() global logger},
 * to which a {@link ConsoleHandler} is added at the beginning of a server session.
 * Solutions are printed to the {@link System#out standard output} stream using
 * {@link StandardKodkodOutput}.
 *
 * <p>In online mode, the server receives problems from the {@link System#in standard input}
 * stream.  Each batch of problems must be terminated by the end-of-input non-character '\uFFFF'.
 * When a batch of (one or more) problems is received, it is processed as
 * described above.  The process exits abnormally if an error is encountered.  Otherwise it waits
 * for the next batch of problems and continues processing
 * them until it receives the (Exit) command. In that case, it exits normally regardless of
 * whether there is any input after the first (Exit) command.  The server also exits normally
 * if its input stream is closed by the caller.</p>
 *
 * <p>The kind of problem specifications accepted by a {@link KodkodServer} instance depends
 * on whether the instance is executing in incremental or standard mode.  When running in
 * incremental mode, the server can accept and solve a sequence of partial problems that make up a complete
 * specification, analogously to an incremental SAT solver.  The server accepts additions to
 * the specification until it encounters the {@code (clear)} command.  At that point, it
 * resets its state to expect a new (incremental) problem specification.  For more details
 * on Kodkod's support for incremental solving, see the {@link IncrementalSolver} documentation.</p>
 *
 * <p>When running in standard mode, the server accepts and solves only complete problem
 * specifications.  This mode is more efficient, supports minimal core extraction, and should
 * be used when incremental solving is not needed by client code.</p>
 *
 * @specfield problem: {@link KodkodProblem} // the problem being processed
 *
 * @author Emina Torlak
 */
public final class KodkodServer {

	static { // set up log handling
		final Logger global = Logger.getGlobal();
		global.setUseParentHandlers(false);
		for(Handler h : Logger.getGlobal().getHandlers()) {
			global.removeHandler(h);
		}
		final Handler h = new ConsoleHandler();
		h.setFormatter(new KodkodFormatter());
		global.addHandler(h);
	}

	private final boolean fastParsing;
	private final String errorOut;
	private final KodkodParser parser;
	private final char[] buf = new char[1024];


	/**
	 * Creates a new {@link KodkodServer} that will write solutions to the given output instance.
	 * The solving and parsing modes are determined by the given flags.  If the {@code incremental} flag is on,
	 * the server will execute in incremental mode; otherwise it will run in standard
	 * mode. If the {@code fastParsing} flag is on, the server will use a fast
	 * parser that will not output any information about parsing errors, except that
	 * they occurred. With the flag off, the solver will use a slower parser that will
	 * report detailed parsing errors. The {@code errorOut} parameter, if not false, specifies the name of
	 * the file to which to dump error-causing input, if any, before exiting.
	 */
	KodkodServer(boolean incremental, boolean fastParsing, KodkodOutput out, String errorOut) {
		if (incremental)
			this.parser = Parboiled.createParser(KodkodParser.class, KodkodProblem.incremental(), out);
		else
			this.parser = Parboiled.createParser(KodkodParser.class,  KodkodProblem.complete(), out);
		this.fastParsing = fastParsing;
		this.errorOut = errorOut;
	}

	/**
	 * Creates a new {@link KodkodServer} that will write solutions to a {@link StandardKodkodOutput} instance.
	 * The solving and parsing modes are determined by the given flags.  If the {@code incremental} flag is on,
	 * the server will execute in incremental mode; otherwise it will run in standard
	 * mode. If the {@code fastParsing} flag is on, the server will use a fast
	 * parser that will not output any information about parsing errors, except that
	 * they occurred. With the flag off, the solver will use a slower parser that will
	 * report detailed parsing errors.  The {@code errorOut} parameter, if not false, specifies the name of
	 * the file to which to dump error-causing input, if any, before exiting.
	 */
	public KodkodServer(boolean incremental, boolean fastParsing, String errorOut) {
		this(incremental, fastParsing, new StandardKodkodOutput(), errorOut);
	}

	/**
	 * Parses and executes the batch of problems specified by the given input buffer.
	 */
	public void serve(InputBuffer batch) {
		final KodkodProblem problem = parser.problem;
		final Rule rule;
		if (problem.isIncremental()) {
			if (problem.isPartial())
				rule = parser.RestOfIncrementalProblems();
			else
				rule = parser.IncrementalProblems();
		} else {
			rule = parser.Problems();
		}
		final ParsingResult<Object> result;
		if (fastParsing) {
			result = (new BasicParseRunner<Object>(rule)).run(batch);
		} else {
			result = (new ErrorLocatingParseRunner<Object>(rule)).run(batch);
		}
		if (!result.matched) {
			if (result.hasErrors()) {
				final Logger logger = Logger.getGlobal();
				for(ParseError err : result.parseErrors) {
					logger.severe(ErrorUtils.printParseError(err));
				}
			} else {
				Logger.getGlobal().severe(	"Error in the input problem.  "+
											"To see the source of the error, re-run a new instance of KodkodServer on this problem " +
											"without -fast-parsing and, optionally, with -error-out <filename>.");
			}
			if (errorOut != null) {
				try(FileWriter fw = new FileWriter(new File(errorOut))) {
					fw.write(InputBufferUtils.collectContent(batch));
				} catch (IOException e) { }
			}
			System.exit(1);
		}
	}



	/**
	 * Parses and executes the problems specified in the given file.
	 */
	public void serve(File file) {
		try(FileReader fr = new FileReader(file)) {
			serve(read(fr));
		} catch (IOException e) {
			Logger.getGlobal().severe(e.getMessage());
		}
	}

	/**
	 * Parses and executes problems received from the {@link System#in standard input}
	 * stream until it reaches the end of the stream or receives a problem with
	 * the Exit command, whichever comes first.
	 */
	public void serve() {
		try(InputStreamReader ir = new InputStreamReader(System.in, "UTF-8")) {
			while(true) {
				serve(read(ir));
			}
		} catch (IOException e) {
			Logger.getGlobal().severe(e.getMessage());
		}
	}

	/**
	 * Returns an input buffer with the data from the given reader.
	 * This method reads data from the reader into an intermediate
	 * character buffer until the reader returns -1 or until the
	 * last character read on a given read attempt is {@link Chars#EOI},
	 * whichever comes first.
	 * @return an input buffer populated with the data from the given reader.
	 */
	private InputBuffer read(Reader r) throws IOException {
		BufferedReader br = new BufferedReader(r);
		final StringBuilder str = new StringBuilder();
		int len;
		while((len=br.read(buf))>0) {
			str.append(buf, 0, len);
			if (buf[len-1]==Chars.EOI)
				break;
		}

		// if we've reached the end of the stream without reading any data, we
		// were either given an empty problem file or the standard input
		// stream to this process has been closed externally.  in either case,
		// we just exit normally.
		if (len<0 && str.length()==0) {
			System.exit(0);
		}

		final char[] tmp = new char[str.length()];
		str.getChars(0, str.length(), tmp, 0);
		//System.err.println(str);
		return new DefaultInputBuffer(tmp);
	}

	/** Prints version.*/
	private static void version() {
		System.out.println("KodkodServer version 2.0 (October 12 2012)");
	}

	/** Prints usage and exists with the given code. */
	private static void usage(int code) {
		version();
		(code == 0 ? System.out : System.err).println(
				"Usage: java kodkod.cli.KodkodServer [options] [filename]\n" +
						"options:\n" +
						"  -help                 Show usage and exit\n" +
						"  -version              Show version number and exit\n" +
						"  -incremental          Run the solver in incremental model\n" +
						"  -fast-parsing         Use a fast parser with no error localizing\n" +
						"  -error-out <filename> Write failure causing input to the specified file before exiting\n");
		System.exit(code);
	}

	/**
	 * Creates and executes an instance of KodkodServer.  Run
	 * {@code java kodkod.cli.KodkodServer -help} for usage options.
	 */
	public static void main(String[] args) {
		//(new ServerTest()).testStandardSolver0();

		boolean incremental = false, fastParsing = false;
		String errorOut = null;
		for(int i = 0, len = args.length; i < len; i++) {
			switch(args[i]) {
			case "-help" 		: usage(0);
			case "-version"		: version(); System.exit(0);
			case "-incremental"	: incremental = true; break;
			case "-fast-parsing": fastParsing = true; break;
			case "-error-out"   :
				if (++i < len) {
					errorOut = args[i];
				} else {
					usage(1);
				}
				break;
			default :
				if (i+1 < len) {
					usage(1);
				} else {
					(new KodkodServer(incremental, fastParsing, errorOut)).serve(new File(args[i]));
					System.exit(0);
				}
			}
		}
		(new KodkodServer(incremental, fastParsing, errorOut)).serve();
		
	}
}
