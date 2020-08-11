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

import kodkod.ast.Expression;
import kodkod.ast.Formula;
import kodkod.ast.IntExpression;
import kodkod.ast.Node;
import kodkod.ast.Relation;
import kodkod.ast.Variable;

import org.parboiled.errors.ActionException;

import java.util.Set;
import java.util.logging.Logger;

/**
 * A definition environment keeps track of definition registers
 * for Kodkod nodes created and referenced during parsing.
 * These include a global {@link Relation} register that is
 * shared between all definition environments, and each
 * environment's registers for intermediate nodes.  The
 * contents of the intermediate registers are looked up using standard
 * lexical scoping rules.
 *
 * @specfield parent: lone {@link DefEnv}
 * @specfield defs: 'r' -> Defs<Relation> +
 *                  'v' -> Defs<Variable> +
 *                  'e' -> Defs<Expression> +
 *                  'i' -> Defs<IntExpression> +
 *                  'f' -> Defs<Formula>
 * @invariant some parent => parent.defs['r'] = defs['r']
 * @invariant no parent => no defs['v'].def
 * @author Emina Torlak
 *
 */
public final class StringDefEnv {
    private final StringDefEnv parent;
    private final StringDefs<Relation> r;
    private final StringDefs<Expression> e;
    private final StringDefs<Formula> f;
    private final StringDefs<IntExpression> i;
    private final StringDefs<Variable> v;
    private final StringDefs<Relation> a;

    /**
     * Constructs an extended definition environment with the given parent.
     * @requires parent !=  null
     * @ensures this.parent' = parent && this.defs'['r'] = parent.defs['r'] && no this.defs'['e'+'f'+'i'+'v']
     */
    private StringDefEnv(StringDefEnv parent) {
        this.parent = parent;
        this.r = parent.r;
        this.e = new StringDefs<>('e');
        this.f = new StringDefs<>('f');
        this.i = new StringDefs<>('i');
        this.v = new StringDefs<>('v');
        this.a = new StringDefs<>('a');
    }

    /**
     * Constructs a root {@link DefEnv} with no parent.
     * The root env has no variable definitions.
     * @ensures no d.defs['r'+'e'+'f'+'i'+'v'] && no this.parent
     */
    StringDefEnv() {
        this.parent = null;
        this.r = new StringDefs<>('r');
        this.e = new StringDefs<>('e');
        this.f = new StringDefs<>('f');
        this.i = new StringDefs<>('i');
        this.v = StringDefs.empty('v');
        this.a = new StringDefs<>('a');
    }

    /**
     * Constructs a root {@link DefEnv} with no parent that contains
     * the specified relation register. The root env has no variable definitions.
     * @requires defs.prefix = 'r'
     * @ensures this.defs = defs.prefix->defs && no this.parent
     */
    StringDefEnv(StringDefs<Relation> r) {
        if (r.prefix()!='r') throw new IllegalArgumentException("Expected an 'r' register, given " + r);
        this.parent = null;
        this.r = r;
        this.e = new StringDefs<>('e');
        this.f = new StringDefs<>('f');
        this.i = new StringDefs<>('i');
        this.v = StringDefs.empty('v');
        this.a = new StringDefs<>('a');
    }

    /**
     * Returns a new {@link DefEnv} that has this as its parent,
     * and that shares its relation register.
     * @return some e: {@link DefEnv} |
     * 			d.parent = this && d.defs['r'] = this.defs['r'] && no d.defs['e'+'f'+'i'+'v']
     */
    StringDefEnv extend() {
        return new StringDefEnv(this);
    }

    /**
     * Returns the parent environment.
     * @return this.parent
     */
    StringDefEnv parent() { return parent; }

    /**
     * Assigns the given value to the variable with the given index in the given register, increasing
     * the size of that register if necessary.  Throws an {@link ActionException} if
     * the given variable index is invalid or the variable is already defined.  Assumes that the value
     * is non-null.
     * @requires reg in 'r'+'e'+'f'+'i'+'v'
     * @requires idx >= 0 && val != null
     * @requires no this.defs[reg].def[idx]
     * @ensures this.defs[reg].def(idx, value)
     * @return true
     * @throws ActionException some this.defs[reg].def[idx]
     * @throws UnsupportedOperationException this is an unmodifiable definition list
     */
    final boolean def(char reg, String name, Node value) {
//        Logger.getGlobal().severe("Defining " + reg + name + " as " + value);
        switch(reg){
            case 'e'	: return e.def(name, (Expression)value);
            case 'f'	: return f.def(name, (Formula)value);
            case 'i'	: return i.def(name, (IntExpression)value);
            case 'r'	: return r.def(name, (Relation)value);
            case 'v'	: return v.def(name, (Variable)value);
            case 'a'    : return a.def(name, (Relation)value);
            default     : throw new ActionException("Invalid identifier: " + reg + name);
        }
    }

    /**
     * Returns the definition register with the given name.  The returned register
     * should not be modified by client code except through  {@link #def(char, String, Node)}.
     * @return (this.defs) & prefix.name
     */
    final StringDefs<? extends Node> defs(char name) {
        switch(name){
            case 'e'	: return e;
            case 'f'	: return f;
            case 'i'	: return i;
            case 'r'	: return r;
            case 'v'	: return v;
            case 'a'    : return a;
            default     : throw new ActionException("Invalid identifier prefix: " + name);
        }
    }

//    /**
//     * Returns the maximum index over all variables defined in the given register, or -1 if no variables have been defined.
//     * @return this.defs[name].maxIndex()
//     */
//    final int maxIndex(char name) {
//        return defs(name).maxIndex();
//    }

    final Set<String> keys(char name) {
        return defs(name).keys();
    }

    /**
     * Returns the lexical definition of the variable at the given index
     * in the given register. The value of the variable is looked up the environment
     * chain, if needed, using standard lexical scoping rules.
     * Throws {@link ActionException} if the identified variable is undefined.
     * @return lexical definition of the variable at the given index
     * in the given register
     */
    @SuppressWarnings("unchecked")
    final <N extends Node> N use(char reg, String name) {
        StringDefEnv top = this;
        do {
            final StringDefs<? extends Node> defs = top.defs(reg);
//            System.out.println("Looking for " + reg + name + " in " + defs);
            if (defs.isDef(name)) {
//                System.out.println("Found " + defs.use(name));
                return (N) defs.use(name);
            } else
                top = top.parent;
        } while (top != null);
        throw new ActionException("No definition found for " + reg + name  + ".");
    }

    /**
     * This method has the same effect as calling {@code this.use(var.charAt(0), Integer.parseInt(var.substring(1)))}.
     * @return this.use(var.charAt(0), Integer.parseInt(var.substring(1)))
     */
    final <N extends Node> N use(String var) {
        return use(var.charAt(0), var.substring(1));
    }

    public String toString() {
        return "{" + e + ", " + f + ", " + i + ", " + r + ", " + v + ", " + a + "," + "}";
    }
}
