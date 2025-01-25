/**
 * SPDX-License-Identifier: MPL-2.0
 * Author:  Masahito Hemmi.
 */
package uika806.syntax;

import uika806.err.LispException;

/**
 *
 */
public class SyntaxException extends LispException {
    
    /**
     * Creates a new instance
     */
    public SyntaxException() {
    }

    /**
     * Creates a new instance
     *
     * @param msg
     */
    public SyntaxException(String msg) {
        super(msg);
        if (msg == null) throw new NullPointerException();
        
    }



}
