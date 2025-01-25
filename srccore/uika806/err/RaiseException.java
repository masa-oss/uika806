/**
 * SPDX-License-Identifier: MPL-2.0
 * Author:  Masahito Hemmi.
 */
package uika806.err;

import uika806.objects.Cell;
import uika806.objects.EmptyList;
import uika806.objects.SString;

/**
 *
 */
public class RaiseException extends LispException {

    public static final int READER_EXCEPTION = 1;
    public static final int FILE_EXCEPTION = 2;
    public static final int EVAL_EXCEPTION = 3;

    private final Object argument;
    final boolean isContinuable;
    private final int exceptionKind;

    public RaiseException(String msg, Object argu, boolean cont) {

        super(msg);
        if (argu == null) {
            throw new NullPointerException();
        }
        this.argument = argu;
        this.isContinuable = cont;

        this.exceptionKind = 0;
    }

    public RaiseException(int kind, String msg, Object argu, boolean cont) {

        super(msg);
        if (argu == null) {
            throw new NullPointerException();
        }
        this.argument = argu;
        this.isContinuable = cont;

        this.exceptionKind = kind;
    }
    
    public RaiseException(String msg, boolean cont) {

        super(msg);
        this.argument = new Cell(SString.fromString(msg), EmptyList.NIL);
        this.isContinuable = cont;

        this.exceptionKind = 0;
    }

    public RaiseException(int kind, String msg, boolean cont) {

        super(msg);
        this.argument = new Cell(SString.fromString(msg), EmptyList.NIL);
        this.isContinuable = cont;

        this.exceptionKind = kind;
    }

    /**
     * @return the argument
     */
    public Object getArgument() {
        return argument;
    }

    public boolean isContinuable() {
        return isContinuable;
    }

    @Override
    public String toString() {
        return "RaiseException[cont = " + isContinuable + "]";
    }

    /**
     * @return the exceptionKind
     */
    public int getExceptionKind() {
        return exceptionKind;
    }

}
