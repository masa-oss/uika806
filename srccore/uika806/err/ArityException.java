/**
 * SPDX-License-Identifier: MPL-2.0
 * Author:  Masahito Hemmi.
 */
package uika806.err;

/**
 *
 * @author hemmi
 */
public class ArityException extends LispException {

    final public int actual;

    final public String name;

    public ArityException(int actual, String name) {
        this(actual, name, null);
    }
    
    public ArityException(int actual, String name, Throwable cause) {
        super("Wrong number of args (" + actual + ") passed to: " + /*Compiler.demunge(*/ name, cause);
        this.actual = actual;
        this.name = name;
        
    }
}
