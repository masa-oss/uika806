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

    private final Object argument;
    boolean isContinuable;

    public RaiseException(String msg, Object argu, boolean cont) {

        super(msg);
        if (argu == null) {
            throw new NullPointerException();
        }
        this.argument = argu;
        this.isContinuable = cont;
    }

    public RaiseException(String msg, boolean cont) {

        super(msg);
        this.argument = new Cell(SString.fromString(msg), EmptyList.NIL);
        this.isContinuable = cont;
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

}
