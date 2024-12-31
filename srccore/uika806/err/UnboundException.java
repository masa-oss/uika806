/**
 * SPDX-License-Identifier: MPL-2.0
 * Author:  Masahito Hemmi.
 */
package uika806.err;

import uika806.objects.SSymbol;
import uika806.syntax.Environ;

/**
 *
 */
public class UnboundException extends LispException {

    private final SSymbol symbol;
    private final Environ environ;

    public UnboundException(SSymbol sym, Environ env) {
        super("Unbound variable " + sym);
        this.symbol = sym;
        this.environ = env;

    }



    /**
     * @return the symbol
     */
    public SSymbol getSymbol() {
        return symbol;
    }

    /**
     * @return the environ
     */
    public Environ getEnviron() {
        return environ;
    }

}
