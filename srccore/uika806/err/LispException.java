/**
 * SPDX-License-Identifier: MPL-2.0
 * Author:  Masahito Hemmi.
 */
package uika806.err;

/**
 */
public class LispException extends RuntimeException {

    /**
     * Creates a new instance
     */
    public LispException() {
    }

    /**
     * Creates a new instance
     *
     * @param msg
     */
    public LispException(String msg) {
        super(msg);
        if (msg == null) throw new NullPointerException();
        
    }

    /**
     * Creates a new instance
     *
     * @param msg
     * @param th
     */
    public LispException(String msg, Throwable th) {
        super(msg, th);
    }


    public static LispException illegalArgument(String msg, Object argu) {

        LispException ex = new LispException(msg);
        return ex;
    }

    
    //2024-11-05 comment
/*
    @Override
    public String getMessage() {

        return "LispException[" + super.getMessage() + "]";

    }
*/
}
