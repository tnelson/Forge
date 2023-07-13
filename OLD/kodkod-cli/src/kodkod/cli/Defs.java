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

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.List;

import org.parboiled.errors.ActionException;

/**
 * A single-assignment definition register that is backed by an array list. 
 * A definition register consists of a set of variable names, each of which 
 * can be assigned a value at most once. A variable is referenced in one of 
 * two ways:  using its integer index in the definition register, or using 
 * its name.  The latter is a concatenation of the register's name, which is 
 * a single character, and the variable's integer index.   
 * 
 * For best performance, variable indices in a definition register should 
 * form a densely packed prefix of natural numbers. 
 * 
 * @specfield prefix: char          // name of this definition register
 * @specfield def: [0..) ->lone V   // binding from variable indices to their values, if any
 * 
 * @author Emina Torlak
 */
final class Defs<V>  {
	private final List<V> def;
	private final char prefix;

	@SuppressWarnings("unchecked")
	private Defs(char prefix, List<?> def) { 
		assert Character.isLetter(prefix);
		this.def = (List<V>) def;
		this.prefix = prefix;
	}
	/**
	 * Creates a new empty definition register.
	 * @ensures no this.def' && this.prefix' = prefix
	 */
	public Defs(char prefix) { this(prefix, new ArrayList<>()); }
	
	/**
	 * Constructs an empty, unmodifiable definition register with the given prefix.
	 * @return some d: Defs | d.prefix' = prefix && no d.defs'
	 */
	@SuppressWarnings("unchecked")
	public static <V> Defs<V> empty(char prefix) { return new Defs<V>(prefix, (List<V>)Collections.emptyList()); }
	
	/**
	 * Returns the name of this register (that is, its variable prefix).
	 * @return this.prefix
	 */
	public char prefix() { return prefix; }
	
	/**
	 * Returns the maximum index over all variables defined so far, or -1 if no variables have been defined.
	 * @return some this.def => max(this.def.V) else -1
	 */
	public int maxIndex() { return def.size()-1; }
	
	/**
	 * Returns the variable index for the given name.
	 * @requires var.charAt(0)  = this.pre && nteger.parseInt(var.substring(1)) >= 0
	 * @return Integer.parseInt(var.substring(1))
	 */
	public int varIdx(String var) {
//		assert regName.charAt(0)==prefix : "Expected an identifier with the prefix " + prefix + ", given " + regName + ".";
//		return Integer.parseInt(regName.substring(1));
		try {
			if (var.charAt(0)!=prefix)
				throw new ActionException("Expected a variable name with the prefix " + prefix + ", given " + var + ".");
			final int idx = Integer.parseInt(var.substring(1));
			if (idx < 0)
				throw new ActionException("Expected a variable name with a non-negative suffix, given " + var + ".");
			return idx;
		} catch (NumberFormatException|IndexOutOfBoundsException e) {
			throw new ActionException("Expected a variable name of the form "+ prefix + "[0-9]+, given " +var + ".", e);
		}
	}
	
	/**
	 * Returns true iff the variable at the given index has been assigned a value.
	 * @return some this.def[idx]
	 */
	public boolean isDef(int idx) {
		return 0 <= idx && idx < def.size() && def.get(idx) != null;
	}
		
	/**
	 * Retrieves the value of the variable at the given index, or <code>null</code> if 
	 * the variable is undefined.
	 * @requires idx >= 0
	 * @return this.def[idx]
	 */
	public V use(int idx) {
		return idx < def.size() ? def.get(idx) : null;
	}
	
	/**
	 * This method has the same effect as calling {@code this.use(varIdx(var))}.
	 * @return this.use(varIdx(var))
	 */
	public V use(String var) { return use(varIdx(var)); }
	
	/**
	 * This method has the same effect as calling {@code this.def(varIdx(var), val)}.  
	 * @return  this.def(varIdx(var), val)
	 */
	public boolean def(String var, V val) { return def(varIdx(var), val); }
	
	/**
	 * Assigns the given value to the variable with the given index, increasing 
	 * the size of this definition register if necessary.  Throws an {@link ActionException} if 
	 * the given variable index is invalid or the variable is already defined.  
	 * @requires idx >= 0 && val != null
	 * @requires no this.def[idx]
	 * @ensures this.def' = ({ j: int, v: null | 0 <= j <= max(#this.def, idx) } ++ this.def) + idx->val  
	 * @return true
	 * @throws ActionException some this.def[idx]
	 * @throws UnsupportedOperationException this is an unmodifiable definition list
	 */
	@SuppressWarnings("unchecked")
	public boolean def(int idx, V val) {
		assert val != null;
		final int size = def.size();
		if (idx == size) { // common case
			return def.add(val);
		} else if (idx < size) { // probably an error
			if (isDef(idx))
				throw new ActionException("Duplicate definition for " + prefix + idx + ".");
			def.set(idx, val);
			return true;
		} else { // hopefully uncommon
			def.addAll((Collection<? extends V>)Collections.nCopies(idx-size, null));
			return def.add(val);
		}
	}
	
	/**
	 * {@inheritDoc}
	 * @see java.lang.Object#toString()
	 */
	public String toString() {
		return prefix + "[" + (def.isEmpty() ? "" : "0.." + (def.size()-1)) + "]";
	}
}
