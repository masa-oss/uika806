/**
 * SPDX-License-Identifier: MPL-2.0
 * Author:  Masahito Hemmi.
 */

package uika806.kernel;

import uika806.err.ArityException;
import uika806.syntax.Environ;
import uika806.objects.EmptyList;
import uika806.objects.Cell;

/**
 * This class like <i>clojure.lang.AFn</i> .
 *
 *
 */
public abstract class AFn implements Runnable {

    public abstract String getName();
    
    
    
    public Object invokeWithEnv(Object arg1, Environ env) {

        return invoke(arg1);
    }

    public Object invokeWithEnv(Object arg1, Object arg2, Environ env) {

        return invoke(arg1, arg2);
    }

    @Override
    public void run() {
        invoke();
    }

    public Object invoke() {
        return throwArity(0);
    }

    public Object invoke(Object arg1) {
        return throwArity(1);
    }

    public Object invoke(Object arg1, Object arg2) {
        return throwArity(2);
    }

    public Object invoke(Object arg1, Object arg2, Object arg3) {
        return throwArity(3);
    }

    public Object invoke(Object arg1, Object arg2, Object arg3, Object arg4) {
        return throwArity(4);
    }

    public Object invoke(Object arg1, Object arg2, Object arg3, Object arg4,
            Object arg5) {
        return throwArity(5);
    }

    public Object invoke(Object arg1, Object arg2, Object arg3, Object arg4,
            Object arg5, Object arg6) {
        return throwArity(6);
    }

    public Object invoke(Object arg1, Object arg2, Object arg3, Object arg4,
            Object arg5, Object arg6, Object arg7) {
        return throwArity(7);
    }

    public Object invoke(Object arg1, Object arg2, Object arg3, Object arg4,
            Object arg5, Object arg6, Object arg7, Object arg8) {
        return throwArity(8);
    }

    public Object throwArity(int n) {
        String name = getClass().getName();
        throw new ArityException(n, name);
    }

    public Object applyTo(Object arglist, Environ env) {

        return applyToHelper(this, arglist, env);
    }

    public static Object applyToHelper(AFn ifn, Object arglist, Environ env) {

        if (arglist == null) {
            throw new NullPointerException();
        }

        if (arglist instanceof EmptyList) {

            return ifn.invoke();
        }

        Object[] arr = new Object[10];

        int count = 0;
        while (arglist instanceof Cell) {
            Cell cel = (Cell) arglist;
            arr[count] = cel.getCar();
            count++;
            arglist = cel.getCdr();
        }

        switch (count) {
            case 0:
                return ifn.invoke();
            case 1:
                // return ifn.invoke(arr[0]);
                return ifn.invokeWithEnv(arr[0], env);

            case 2:
                // return ifn.invoke(arr[0], arr[1]);
                return ifn.invokeWithEnv(arr[0], arr[1], env);

            case 3:
                return ifn.invoke(arr[0], arr[1], arr[2]);
            case 4:
                return ifn.invoke(arr[0], arr[1], arr[2], arr[3]);
            case 5:
                return ifn.invoke(arr[0], arr[1], arr[2], arr[3], arr[4]);
            case 6:
                return ifn.invoke(arr[0], arr[1], arr[2], arr[3], arr[4], arr[5]);
            case 7:
                return ifn.invoke(arr[0], arr[1], arr[2], arr[3], arr[4], arr[5], arr[6]);
            case 8:
                return ifn.invoke(arr[0], arr[1], arr[2], arr[3], arr[4], arr[5], arr[6], arr[7]);
            default:
                throw new UnsupportedOperationException();
        }
    }

}
