/**
 * SPDX-License-Identifier: MPL-2.0
 * Author:  Masahito Hemmi.
 */
package uika806.err;

/**
 *
 */
public class BadArgumentInFunctionException extends LispException {
    
    public BadArgumentInFunctionException(String msg) {
        super(msg);
    }

    public BadArgumentInFunctionException(String msg, Throwable th) {
        super(msg, th);
    }
    
    @Override
    public String getMessage() {
        return "BadArgumentInFunctionException[" + super.getMessage() + "]";
    }
    
}
