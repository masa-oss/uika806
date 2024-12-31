/**
 * SPDX-License-Identifier: MPL-2.0
 * Author:  Masahito Hemmi.
 */
package uika806.pico.fn;

import uika806.kernel.AFn;
import uika806.vm4.Closure;
import uika806.objects.Cell;
import uika806.err.LispException;
import uika806.objects.SSymbol;

/**
 *
 * @author hemmi
 */
public final class Func011 {

    public static class Car extends AFn {

        @Override
        public String getName() {
            return "car";
        }

        @Override
        public Object invoke(Object arg1) {
            return xcar(arg1, "car");
        }
    }

    static Object xcar(Object arg1, String funcName) {

        if (arg1 instanceof Cell) {
            Cell cell = (Cell) arg1;
            return cell.getCar();
        } else {

            throw LispException.illegalArgument(funcName + " has unexpected argument", arg1);
        }
    }

    public static class Cdr extends AFn {

        @Override
        public String getName() {
            return "cdr";
        }

        @Override
        public Object invoke(Object arg1) {

            return xcdr(arg1, "cdr");
        }
    }

    static Object xcdr(Object arg1, String funcName) {

        if (arg1 instanceof Cell) {
            Cell cell = (Cell) arg1;
            return cell.getCdr();
        } else {

            throw LispException.illegalArgument(funcName + " has unexpected argument", arg1);
        }
    }

    public static class Caar extends AFn {

        @Override
        public String getName() {
            return "caar";
        }

        @Override
        public Object invoke(Object arg1) {
            return xcar(xcar(arg1, "caar"), "caar");
        }
    }

    public static class Cadr extends AFn {

        @Override
        public String getName() {
            return "cadr";
        }

        @Override
        public Object invoke(Object arg1) {
            return xcar(xcdr(arg1, "cadr"), "cadr");
        }
    }

    public static class Cdar extends AFn {

        @Override
        public String getName() {
            return "cdar";
        }

        @Override
        public Object invoke(Object arg1) {
            return xcdr(xcar(arg1, "cdar"), "cdar");
        }
    }

    public static class Cddr extends AFn {

        @Override
        public String getName() {
            return "cddr";
        }

        @Override
        public Object invoke(Object arg1) {
            return xcdr(xcdr(arg1, "cddr"), "cddr");
        }
    }

    // ------------------ CXR ------------------
    public static class Caaar extends AFn {

        @Override
        public String getName() {
            return "caaar";
        }

        @Override
        public Object invoke(Object arg1) {
            return xcar(xcar(xcar(arg1, "caaar"), "caaar"), "caaar");
        }
    }

    public static class Caadr extends AFn {

        @Override
        public String getName() {
            return "caadr";
        }

        @Override
        public Object invoke(Object arg1) {
            return xcar(xcar(xcdr(arg1, "caadr"), "caadr"), "caadr");
        }
    }

    public static class Cadar extends AFn {

        @Override
        public String getName() {
            return "cadar";
        }

        @Override
        public Object invoke(Object arg1) {
            return xcar(xcdr(xcar(arg1, "cadar"), "cadar"), "cadar");
        }
    }

    public static class Caddr extends AFn {

        @Override
        public String getName() {
            return "caddr";
        }

        @Override
        public Object invoke(Object arg1) {
            return xcar(xcdr(xcdr(arg1, "caddr"), "caddr"), "caddr");
        }
    }

    public static class Cdaar extends AFn {

        @Override
        public String getName() {
            return "cdaar";
        }

        @Override
        public Object invoke(Object arg1) {
            return xcdr(xcar(xcar(arg1, "cdaar"), "cdaar"), "cdaar");
        }
    }

    public static class Cdadr extends AFn {

        @Override
        public String getName() {
            return "cdadr";
        }

        @Override
        public Object invoke(Object arg1) {
            return xcdr(xcar(xcdr(arg1, "cdadr"), "cdadr"), "cdadr");
        }
    }

    public static class Cddar extends AFn {

        @Override
        public String getName() {
            return "cddar";
        }

        @Override
        public Object invoke(Object arg1) {
            return xcdr(xcdr(xcar(arg1, "cddar"), "cddar"), "cddar");
        }
    }

    public static class Cdddr extends AFn {

        @Override
        public String getName() {
            return "cdddr";
        }

        @Override
        public Object invoke(Object arg1) {
            return xcdr(xcdr(xcdr(arg1, "cdddr"), "cdddr"), "cdddr");
        }
    }

    // ------------------ CXR ------------------

    public static class SetCarFn extends AFn {

        @Override
        public String getName() {
            return "set-car!";
        }

        @Override
        public Object invoke(Object arg1, Object arg2) {
            if (arg1 instanceof Cell) {
                Cell cell = (Cell)arg1;
                cell.setCar(arg2);
                return SSymbol.Undefined;
            }
            throw new LispException("set-car!");
        }
    }
    
    public static class SetCdrFn extends AFn {

        @Override
        public String getName() {
            return "set-cdr!";
        }

        @Override
        public Object invoke(Object arg1, Object arg2) {
            if (arg1 instanceof Cell) {
                Cell cell = (Cell)arg1;
                cell.setCdr(arg2);
                return SSymbol.Undefined;
            }
            throw new LispException("set-cdr!");
        }
    }

    public static class MyCons extends AFn {

        @Override
        public String getName() {
            return "cons";
        }

        @Override
        public Object invoke(Object arg1, Object arg2) {
            // create mutable Cell
            return new Cell(arg1, arg2);
        }
    }

    public static class MyAdd extends AFn {

        @Override
        public String getName() {
            return "+";
        }

        @Override
        public Object invoke() {
            return 0L;
        }

        @Override
        public Object invoke(Object arg1) {
            return (arg1);
        }

        @Override
        public Object invoke(Object arg1, Object arg2) {
            return Numbers.add(arg1, arg2);
        }

        @Override
        public Object invoke(Object arg1, Object arg2, Object arg3) {
            return Numbers.add(Numbers.add(arg1, arg2), arg3);
        }

        @Override
        public Object invoke(Object arg1, Object arg2, Object arg3, Object arg4) {
            return Numbers.add(Numbers.add(Numbers.add(arg1, arg2), arg3), arg4);
        }

    }

    public static class MySub extends AFn {

        @Override
        public String getName() {
            return "-";
        }

        @Override
        public Object invoke(Object arg1) {
            return Numbers.minus(0L, arg1);
        }

        @Override
        public Object invoke(Object arg1, Object arg2) {
            return Numbers.minus(arg1, arg2);
        }

        @Override
        public Object invoke(Object arg1, Object arg2, Object arg3) {
            return Numbers.minus(Numbers.minus(arg1, arg2), arg3);
        }
    }

    public static class MyMultiply extends AFn {

        @Override
        public String getName() {
            return "*";
        }

        @Override
        public Object invoke() {

            return 1L;
        }

        @Override
        public Object invoke(Object arg1) {

            return (arg1);
        }

        @Override
        public Object invoke(Object arg1, Object arg2) {

            return Numbers.multiply(arg1, arg2);
        }
    }

    public static class Square extends AFn {

        @Override
        public String getName() {
            return "square";
        }

        @Override
        public Object invoke(Object arg1) {

            return Numbers.multiply(arg1, arg1);
        }
    }

    public static class MyDivide extends AFn {

        @Override
        public String getName() {
            return "/";
        }

        @Override
        public Object invoke(Object arg1) {

            return Numbers.divide(1L, arg1);
        }

        @Override
        public Object invoke(Object arg1, Object arg2) {

            return Numbers.divide(arg1, arg2);
        }

        @Override
        public Object invoke(Object arg1, Object arg2, Object arg3) {

            return Numbers.divide(Numbers.divide(arg1, arg2), arg3);
        }
    }

    public static class BooleanqFn extends AFn {

        @Override
        public String getName() {
            return "boolean?";
        }

        @Override
        public Object invoke(Object a) {
            if (a instanceof Boolean) {
                return Boolean.TRUE;
            } else {
                return Boolean.FALSE;
            }
        }
    }

    public static class SymbolqFn extends AFn {

        @Override
        public String getName() {
            return "symbol?";
        }

        @Override
        public Object invoke(Object a) {
            if (a instanceof SSymbol) {
                return Boolean.TRUE;
            } else {
                return Boolean.FALSE;
            }
        }
    }

    public static class ProcedureqFn extends AFn {

        @Override
        public String getName() {
            return "procedure?";
        }

        @Override
        public Object invoke(Object a) {
            if (a instanceof AFn) {
                return true;
            } else if (a instanceof Closure) {
                return true;
            }
            return false;
        }
    }

    public static class NumberqFn extends AFn {

        @Override
        public String getName() {
            return "number?";
        }

        @Override
        public Object invoke(Object o) {

            return (o instanceof Number);
        }
    }

    public static class PairqFn extends AFn {

        @Override
        public String getName() {
            return "pair?";
        }

        @Override
        public Object invoke(Object o) {

            return (o instanceof Cell);
        }
    }
/*
    Deprecated
    public static class MyLt extends AFn {

        @Override
        public Object invoke(Object arg1, Object arg2) {

            return Numbers.lt(arg1, arg2);
        }

    }

    Deprecated
    public static class MyGt extends AFn {

        @Override
        public Object invoke(Object arg1, Object arg2) {

            return Numbers.gt(arg1, arg2);
        }
    }
*/
    public static class LeFn extends AFn {

        @Override
        public String getName() {
            return "<=";
        }

        @Override
        public Object invoke(Object arg1, Object arg2, Object arg3) {
            return le(arg1, arg2) && le(arg2, arg3);
        }

        @Override
        public Object invoke(Object arg1, Object arg2) {
            return le(arg1, arg2);
        }

        boolean le(Object arg1, Object arg2) {

            if (arg1 instanceof Number) {
                if (arg2 instanceof Number) {
                    Number n1 = (Number) arg1;
                    Number n2 = (Number) arg2;

                    return n1.doubleValue() <= n2.doubleValue();
                }
            }
            return false;
        }

    }

    public static class GeFn extends AFn {

        @Override
        public String getName() {
            return ">=";
        }

        @Override
        public Object invoke(Object arg1, Object arg2, Object arg3) {
            return ge(arg1, arg2) && ge(arg2, arg3);
        }

        @Override
        public Object invoke(Object arg1, Object arg2) {
            return ge(arg1, arg2);
        }

        boolean ge(Object arg1, Object arg2) {

            if (arg1 instanceof Number) {
                if (arg2 instanceof Number) {
                    Number n1 = (Number) arg1;
                    Number n2 = (Number) arg2;

                    return n1.doubleValue() >= n2.doubleValue();
                }
            }
            return false;
        }

    }

    public static class MyEquiv extends AFn {

        @Override
        public String getName() {
            return "=";
        }

        @Override
        public Object invoke(Object arg1, Object arg2) {
            return eq(arg1, arg2);
        }

        @Override
        public Object invoke(Object arg1, Object arg2, Object arg3) {
            return eq(arg1, arg2) && eq(arg2, arg3);
        }

        boolean eq(Object arg1, Object arg2) {
            
            if (arg1 instanceof Long && arg2 instanceof Long) {
                Long ln1 = (Long) arg1;
                Long ln2 = (Long) arg2;
                return ln1.equals(ln2);
            }
            
            if (arg1 instanceof Double && arg2 instanceof Double) {
                Double dbl1 = (Double) arg1;
                Double dbl2 = (Double) arg2;
                
                //いずれかの引数が +nan.0 であれば #f を 返す
                if (Double.isNaN(dbl1)) return Boolean.FALSE;
                if (Double.isNaN(dbl2)) return Boolean.FALSE;
                
                return dbl1.equals(dbl2);
            }
            
            return Boolean.FALSE;
        }


    }

    public static class MyNot extends AFn {

        @Override
        public String getName() {
            return "not";
        }

        // Scheme では #f 以外全て true扱い
        @Override
        public Object invoke(Object a) {

            if (a instanceof Boolean) {
                Boolean aa = (Boolean) a;
                return !aa;
            }
            return Boolean.FALSE;
        }
    }

    private Func011() {
    }
}
