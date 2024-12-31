/**
 * SPDX-License-Identifier: MPL-2.0
 * Author:  Masahito Hemmi.
 */
package uika806.kernel;

import uika806.err.LispException;
import uika806.objects.EmptyList;
import uika806.objects.SSymbol;
import uika806.objects.Cell;

/**
 *
 */
public class RT {

    public static final EmptyList EOL = EmptyList.NIL;

    public static Object UNDEF = SSymbol.Undefined;

    public static boolean isTail(Object o) {
        return !(o instanceof Cell);
    }

    public static boolean isNull(Object o) {
        return (o instanceof EmptyList);
    }

    public static Object append(Object x, Object y) {

        if (isNull(x)) {
            return y;
        }
        return new Cell(car(x), append(cdr(x), y));
    }

    public static Object list(Object b) {
        return new Cell(b, EOL);
    }

    public static Object list(Object a, Object b) {
        return new Cell(a, new Cell(b, EOL));
    }

    public static Object list(Object a, Object b, Object c) {
        return new Cell(a, new Cell(b, new Cell(c, EOL)));
    }

    public static Object list(Object a, Object b, Object c, Object d) {
        return new Cell(a, new Cell(b, new Cell(c, new Cell(d, EOL))));
    }

    public static Object list(Object a, Object b, Object c, Object d, Object e) {
        return new Cell(a, new Cell(b, new Cell(c, new Cell(d, new Cell(e, EOL)))));
    }

    public static Object immutableList(Object a, Object b, Object c, Object d, Object e) {
        return new Cell(a, new Cell(b, new Cell(c, new Cell(d, new Cell(e, EOL, false), false), false)
        ,false),false);
    }



    public static Object car(Object arg1) {

        if (arg1 instanceof Cell) {
            Cell cell = (Cell) arg1;
            return cell.getCar();
        } else {

            throw LispException.illegalArgument("car has unexpected argument", arg1);
        }
    }

    public static Object cdr(Object arg1) {

        if (arg1 instanceof Cell) {
            Cell cell = (Cell) arg1;
            return cell.getCdr();
        } else {

            throw LispException.illegalArgument("cdr has unexpected argument", arg1);
        }
    }

    public static Object caar(Object o) {
        return car(car(o));
    }

    public static Object cadr(Object o) {
        return car(cdr(o));
    }

    public static Object cddr(Object o) {
        return cdr(cdr(o));
    }

    public static Object cdar(Object o) {
        return cdr(car(o));
    }

    public static Object cadar(Object o) {
        return car(cdr(car(o)));
    }
    
    public static Object caddr(Object o) {
        return car(cdr(cdr(o)));
    }

    public static Object cadddr(Object o) {
        return car(cdr(cdr(cdr(o))));
    }

    public static Object cdddr(Object o) {
        return cdr(cdr(cdr(o)));
    }


    
    public static boolean isList(Object obj) {
        if (obj instanceof Cell) {
            return true;
        } else if (obj instanceof EmptyList) {
            return true;
        }
        return false;
    }
    
    
    
    
    // disable construct
    private RT() {
        
    }

}
