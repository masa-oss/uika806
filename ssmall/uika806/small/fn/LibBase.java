/**
 * SPDX-License-Identifier: MPL-2.0
 * Author:  Masahito Hemmi.
 */
package uika806.small.fn;

import java.io.UnsupportedEncodingException;
import java.math.BigInteger;
import java.util.ArrayList;


import uika806.kernel.AFn;
import uika806.err.LispException;
import uika806.kernel.RT;
import uika806.kernel.Values;
import uika806.pico.fn.EqvqFn;
import uika806.objects.Cell;
import uika806.objects.Complex;
import uika806.objects.EmptyList;
import uika806.objects.EndOfFile;
import uika806.objects.Ratio;
import uika806.objects.SArray;
import uika806.objects.SChar;
import uika806.objects.SString;
import uika806.objects.SSymbol;
import uika806.objects.U8Array;

/**
 *
 */
public final class LibBase {

    public static class AbsFn extends AFn {

        @Override
        public String getName() {
            return "abs";
        }

        @Override
        public Object invoke(Object n) {

            if (n instanceof Double) {
                Double d = (Double) n;
                return Math.abs(d);
            }

            if (n instanceof Long) {
                Long ln = (Long) n;
                return Math.absExact(ln);
            }

            throw new UnsupportedOperationException();
        }
    }

    public static class AssvFn extends AFn {

        @Override
        public String getName() {
            return "assv";
        }

        EqvqFn eqv = new EqvqFn();

        @Override
        public Object invoke(Object key, Object list) {

            if (!(list instanceof Cell)) {
                return Boolean.FALSE;
            } else {
                Object car = RT.car(list);
                Boolean b = (Boolean) eqv.invoke(RT.car(car), key);
                if (b) {
                    return car;
                } else {
                    return invoke(key, RT.cdr(list));
                }
            }
        }
    }

    public static class ByteVectorFn extends AFn {

        @Override
        public String getName() {
            return "byte-vector?";
        }

        @Override
        public Object invoke() {

            byte[] arr = new byte[0];
            return new U8Array(arr, true);
        }

        @Override
        public Object invoke(Object arg1) {

            byte[] arr = new byte[]{by(arg1)};
            return new U8Array(arr, true);
        }

        @Override
        public Object invoke(Object arg1, Object arg2) {

            byte[] arr = new byte[]{by(arg1), by(arg2)};
            return new U8Array(arr, true);
        }

        @Override
        public Object invoke(Object arg1, Object arg2, Object arg3) {

            byte[] arr = new byte[]{by(arg1), by(arg2), by(arg3)};
            return new U8Array(arr, true);
        }

        @Override
        public Object invoke(Object arg1, Object arg2, Object arg3, Object arg4) {

            byte[] arr = new byte[]{by(arg1), by(arg2), by(arg3), by(arg4)};
            return new U8Array(arr, true);
        }

        @Override
        public Object invoke(Object arg1, Object arg2, Object arg3, Object arg4, Object arg5) {

            byte[] arr = new byte[]{by(arg1), by(arg2), by(arg3), by(arg4), by(arg5)};
            return new U8Array(arr, true);
        }

        @Override
        public Object invoke(Object arg1, Object arg2, Object arg3, Object arg4, Object arg5, Object arg6) {

            byte[] arr = new byte[]{by(arg1), by(arg2), by(arg3), by(arg4), by(arg5), by(arg6)};

            return new U8Array(arr, true);
        }

        @Override
        public Object invoke(Object arg1, Object arg2, Object arg3, Object arg4, Object arg5, Object arg6, Object arg7) {

            byte[] arr = new byte[]{by(arg1), by(arg2), by(arg3), by(arg4), by(arg5), by(arg6), by(arg7)};
            return new U8Array(arr, true);
        }

        @Override
        public Object invoke(Object arg1, Object arg2, Object arg3, Object arg4, Object arg5, Object arg6, Object arg7, Object arg8) {

            byte[] arr = new byte[]{by(arg1), by(arg2), by(arg3), by(arg4), by(arg5), by(arg6), by(arg7), by(arg8)};
            return new U8Array(arr, true);
        }

        byte by(Object o) {

            return ((Number) o).byteValue();
        }

    }

    public static class ByteVectorLengthFn extends AFn {

        @Override
        public String getName() {
            return "byte-vector-length";
        }

        @Override
        public Object invoke(Object arg1) {

            U8Array arr = (U8Array) arg1;
            long ln = arr.length();
            return ln;
        }
    }

    public static class ByteVectorRefFn extends AFn {

        @Override
        public String getName() {
            return "byte-vector-ref";
        }

        @Override
        public Object invoke(Object arg1, Object arg2) {

            int it = ((Number) arg2).intValue();
            U8Array arr = (U8Array) arg1;
            return arr.getNthAsInt(it);
        }
    }

    public static class ByteVectorSetFn extends AFn {

        @Override
        public String getName() {
            return "Byte-Vector-SetFn";
        }

        @Override
        public Object invoke(Object arg1, Object arg2, Object arg3) {

            U8Array sa = (U8Array) arg1;
            int nth = ((Number) arg2).intValue();
            long newVal = ((Number) arg3).longValue();
            return sa.setNth(nth, newVal);
        }
    }

    public static class ByteVectorCopy extends AFn {

        @Override
        public String getName() {
            return "byte-vector-copy";
        }

        @Override
        public Object invoke(Object arg1) {

            U8Array sa = (U8Array) arg1;
            int len = sa.length();
            return sa.subvector(0, len);
        }

        @Override
        public Object invoke(Object arg1, Object arg2) {

            U8Array sa = (U8Array) arg1;
            int len = sa.length();
            int start = ((Number) arg2).intValue();
            if (start < 0 || len < start) {
                throw new LispException("Bad index to vector-copy: " + start);
            }

            return sa.subvector(start, len);
        }

        @Override
        public Object invoke(Object arg1, Object arg2, Object arg3) {

            U8Array sa = (U8Array) arg1;
            int len = sa.length();
            int start = ((Number) arg2).intValue();
            if (start < 0 || len < start) {
                throw new LispException("Bad index to vector-copy: " + start);
            }

            int end = ((Number) arg3).intValue();
            if (end < 0 || len < end) {
                throw new LispException("Bad index to vector-copy: " + start);
            }
            if (!(start <= end)) {
                throw new LispException("Bad index to vector-copy: " + start);
            }
            return sa.subvector(start, end);
        }
    }

    public static class ByteVectorCopyeFn extends AFn {

        @Override
        public String getName() {
            return "byte-vector-copy!";
        }

        @Override
        public Object invoke(Object arg1, Object arg2, Object arg3) {
            U8Array ss = (U8Array) arg1;

            long end = ss.length();
            return invoke(arg1, arg2, arg3, 0L, end);
        }

        @Override
        public Object invoke(Object arg1, Object arg2, Object arg3, Object arg4) {

            U8Array ss = (U8Array) arg1;
            long end = ss.length();
            return invoke(arg1, arg2, arg3, arg4, end);
        }

        @Override
        public Object invoke(Object arg1, Object arg2, Object arg3, Object arg4, Object arg5) {

            U8Array ss = (U8Array) arg1;
            int at = ((Number) arg2).intValue();
            U8Array from = (U8Array) arg3;
            int start = ((Number) arg4).intValue();
            int end = ((Number) arg5).intValue();

            ss.copyFrom(from, at, start, end);

            return Boolean.TRUE;
        }
    }

    public static class ByteVectorAppendFn extends AFn {

        @Override
        public String getName() {
            return "byte-vector-append";
        }

        @Override
        public Object invoke(Object arg1) {
            return (arg1);
        }

        @Override
        public Object invoke(Object arg1, Object arg2) {

            U8Array s1 = (U8Array) arg1;
            U8Array s2 = (U8Array) arg2;

            return U8Array.append(s1, s2, true);
        }

        @Override
        public Object invoke(Object arg1, Object arg2, Object arg3) {

            U8Array s1 = (U8Array) arg1;
            U8Array s2 = (U8Array) arg2;
            U8Array s3 = (U8Array) arg3;

            return U8Array.append(s1,
                    U8Array.append(s2, s3, true),
                    true);
        }
    }

    public static class Utf8StringFn extends AFn {

        @Override
        public String getName() {
            return "utf8->string";
        }

        @Override
        public Object invoke(Object arg1) {

            U8Array vec = (U8Array) arg1;

            String utf8 = null;
            try {
                utf8 = vec.utf8ToAtring();

            } catch (UnsupportedEncodingException uee) {
                throw new RuntimeException("utf8->string", uee);
            }
            return SString.fromString(utf8);
        }

        @Override
        public Object invoke(Object arg1, Object arg2) {

            U8Array vec = (U8Array) arg1;
            int len = vec.length();

            return invoke(arg1, arg2, len);
        }

        @Override
        public Object invoke(Object arg1, Object arg2, Object arg3) {

            U8Array vec = (U8Array) arg1;

            int start = ((Number) arg2).intValue();
            int end = ((Number) arg3).intValue();

            String utf8 = null;
            try {
                utf8 = vec.utf8ToAtring(start, end);

            } catch (UnsupportedEncodingException uee) {
                throw new RuntimeException("utf8->string", uee);
            }
            return SString.fromString(utf8);
        }
    }

    public static class CharLtFn extends AFn {

        @Override
        public String getName() {
            return "char<";
        }

        @Override
        public Object invoke(Object a, Object b) {
            SChar aa = (SChar) a;
            SChar bb = (SChar) b;

            int ca = aa.getCodepoint();
            int cb = bb.getCodepoint();

            return (ca < cb);
        }

        @Override
        public Object invoke(Object a, Object b, Object c) {

            SChar aa = (SChar) a;
            SChar bb = (SChar) b;
            SChar cc = (SChar) c;

            int ca = aa.getCodepoint();
            int cb = bb.getCodepoint();
            int gc = cc.getCodepoint();

            return (ca < cb) && (cb < gc);
        }
    }

    public static class CharLteFn extends AFn {

        @Override
        public String getName() {
            return "char<=";
        }

        @Override
        public Object invoke(Object a, Object b) {
            SChar aa = (SChar) a;
            SChar bb = (SChar) b;

            int ca = aa.getCodepoint();
            int cb = bb.getCodepoint();

            return (ca <= cb);
        }

        @Override
        public Object invoke(Object a, Object b, Object c) {

            SChar aa = (SChar) a;
            SChar bb = (SChar) b;
            SChar cc = (SChar) c;

            int ca = aa.getCodepoint();
            int cb = bb.getCodepoint();
            int gc = cc.getCodepoint();

            return (ca <= cb) && (cb <= gc);
        }
    }

    public static class CharEqqFn extends AFn {

        @Override
        public String getName() {
            return "char=";
        }

        @Override
        public Object invoke(Object arg1, Object arg2) {

            SChar sc1 = (SChar) arg1;
            SChar sc2 = (SChar) arg2;

            return sc1.getCodepoint() == sc2.getCodepoint();
        }

        @Override
        public Object invoke(Object arg1, Object arg2, Object arg3) {

            SChar sc1 = (SChar) arg1;
            SChar sc2 = (SChar) arg2;
            SChar sc3 = (SChar) arg3;

            return (sc1.getCodepoint() == sc2.getCodepoint())
                    && (sc2.getCodepoint() == sc3.getCodepoint());
        }
    }

    public static class CharGtFn extends AFn {

        @Override
        public String getName() {
            return "char>";
        }

        @Override
        public Object invoke(Object a, Object b) {
            SChar aa = (SChar) a;
            SChar bb = (SChar) b;

            int ca = aa.getCodepoint();
            int cb = bb.getCodepoint();

            return (ca > cb);
        }

        @Override
        public Object invoke(Object a, Object b, Object c) {

            SChar aa = (SChar) a;
            SChar bb = (SChar) b;
            SChar cc = (SChar) c;

            int ca = aa.getCodepoint();
            int cb = bb.getCodepoint();
            int gc = cc.getCodepoint();

            return (ca > cb) && (cb > gc);
        }
    }

    public static class CharGteFn extends AFn {

        @Override
        public String getName() {
            return "char>=";
        }

        @Override
        public Object invoke(Object a, Object b) {
            SChar aa = (SChar) a;
            SChar bb = (SChar) b;

            int ca = aa.getCodepoint();
            int cb = bb.getCodepoint();

            return (ca >= cb);
        }

        @Override
        public Object invoke(Object a, Object b, Object c) {

            SChar aa = (SChar) a;
            SChar bb = (SChar) b;
            SChar cc = (SChar) c;

            int ca = aa.getCodepoint();
            int cb = bb.getCodepoint();
            int gc = cc.getCodepoint();

            return (ca >= cb) && (cb >= gc);
        }
    }

    public static class ComplexqFn extends AFn {

        @Override
        public String getName() {
            return "complex?";
        }

        @Override
        public Object invoke(Object arg1) {

            if (arg1 instanceof Complex) {
                return Boolean.TRUE;
            } else if (arg1 instanceof Long) {
                return Boolean.TRUE;
            }
            return Boolean.FALSE;
        }
    }

    public static class DenominatorFn extends AFn {

        @Override
        public String getName() {
            return "denominator";
        }

        ExactFn exactFn = new ExactFn();

        @Override
        public Object invoke(Object arg1) {

            if (arg1 instanceof Ratio) {
                Ratio ra = (Ratio) arg1;

                Ratio irr = ra.getIrreducible();

                BigInteger denominator = irr.getDenominator();

                return denominator.longValueExact();
            }
            if (arg1 instanceof Double) {

                Object o = exactFn.invoke(arg1);
                if (o instanceof Ratio) {
                    Ratio rr = (Ratio) o;
                    Ratio ir = rr.getIrreducible();

                    BigInteger denominator = ir.getDenominator();

                    return denominator.doubleValue();
                }
                throw new IllegalArgumentException("denominator");
            }
            throw new IllegalArgumentException("denominator");
        }
    }

    public static class ExactqFn extends AFn {

        @Override
        public String getName() {
            return "exact?";
        }

        @Override
        public Object invoke(Object arg1) {

            if (arg1 instanceof Long) {
                return Boolean.TRUE;
            } else if (arg1 instanceof Ratio) {
                return Boolean.TRUE;
            }
            return Boolean.FALSE;
        }
    }

    public static class ExactIntegerqFn extends AFn {

        @Override
        public String getName() {
            return "exact-integer?";
        }

        @Override
        public Object invoke(Object arg1) {

            if (arg1 instanceof Long) {
                return Boolean.TRUE;
            }
            return Boolean.FALSE;
        }
    }

    public static class ExactIntegerSqrtFn extends AFn {

        @Override
        public String getName() {
            return "exact-integer-sqrt";
        }

        @Override
        public Object invoke(Object arg1) {

            if (arg1 instanceof Long) {

                Long ln = (Long) arg1;
                double sq = Math.sqrt(ln.doubleValue());

                Double sqi = Math.floor(sq);

                long ln2 = sqi.longValue();

                long rest = ln - (ln2 * ln2);
                return new Values(ln2, rest);
            }
            throw new IllegalArgumentException("exact-integer-sqrt");
        }
    }

    static FloorSlashFn floorSlashFn = new FloorSlashFn();

    public static class FloorSlashFn extends AFn {

        @Override
        public String getName() {
            return "floor/";
        }

        @Override
        public Object invoke(Object n, Object y) {

            if (n instanceof Long) {
                Long ln = (Long) n;
                if (y instanceof Long) {
                    Long yy = (Long) y;

                    if (ln < 0 && yy > 0) {

                        long r1 = (ln - yy) / yy;
                        long r2 = ln - (r1 * yy);
                        return new Values(r1, r2);

                    } else if (ln > 0 && yy < 0) {

                        long r1 = -((ln - yy) / -yy);
                        long r2 = ln - (r1 * yy);
                        return new Values(r1, r2);

                    } else if (ln < 0 && yy < 0) {

                        long r1 = ln / yy;
                        long r2 = ln - (r1 * yy);
                        return new Values(r1, r2);

                    } else {
                        long r1 = ln / yy;
                        long r2 = ln % yy;
                        return new Values(r1, r2);
                    }
                }
            }
            throw new UnsupportedOperationException();
        }
    }

    public static class FloorQuotientFn extends AFn {

        @Override
        public String getName() {
            return "floor-quotient";
        }

        @Override
        public Object invoke(Object n, Object y) {

            Values v = (Values) floorSlashFn.invoke(n, y);
            return v.getValue1();
        }
    }

    public static class FloorRemainderFn extends AFn {

        @Override
        public String getName() {
            return "floor-remainder";
        }

        @Override
        public Object invoke(Object n, Object y) {

            Values v = (Values) floorSlashFn.invoke(n, y);
            return v.getValue2();
        }
    }

    public static class GcdFn extends AFn {

        @Override
        public String getName() {
            return "gcd";
        }

        @Override
        public Object invoke() {
            return 0L;
        }

        @Override
        public Object invoke(Object arg1, Object arg2) {

            long ln1 = ((Number) arg1).longValue();
            BigInteger bi = BigInteger.valueOf(ln1);

            long ln2 = ((Number) arg2).longValue();
            BigInteger bi2 = BigInteger.valueOf(ln2);

            BigInteger gcd = bi.gcd(bi2);
            return gcd.longValue();
        }
    }

    public static class InExactqFn extends AFn {

        @Override
        public String getName() {
            return "in-exact?";
        }

        @Override
        public Object invoke(Object arg1) {

            if (arg1 instanceof Double) {
                return Boolean.TRUE;
            }
            return Boolean.FALSE;
        }
    }

    public static class LcmFn extends AFn {

        @Override
        public String getName() {
            return "lcm";
        }

        GcdFn fn = new GcdFn();

        @Override
        public Object invoke() {
            return 1L;
        }

        @Override
        public Object invoke(Object arg1, Object arg2) {

            if (arg1 instanceof Long) {
                Long ln1 = (Long) arg1;
                Long ln2 = (Long) arg2;
                Long gcd = (Long) fn.invoke(ln1, ln2);

                long mul = Math.multiplyExact(Math.abs(ln1.longValue()), Math.abs(ln2.longValue()));
                long lcm = mul / gcd;
                return lcm;
            } else if (arg1 instanceof Double) {
                Long ln1 = ((Double) arg1).longValue();
                Long ln2 = (Long) arg2;
                Long gcd = (Long) fn.invoke(ln1, ln2);

                long mul = Math.multiplyExact(Math.abs(ln1.longValue()), Math.abs(ln2.longValue()));
                double lcm = mul / gcd;
                return lcm;

            }
            throw new IllegalArgumentException("lcm");
        }
    }

    public static class ModuloFn extends AFn {

        @Override
        public String getName() {
            return "modulo";
        }

        FloorSlashFn fn = new FloorSlashFn();

        @Override
        public Object invoke(Object n, Object y) {

            // ，modulo は floor-remainder と等価である
            Values v = (Values) fn.invoke(n, y);
            return v.getValue2();

        }
    }

    public static class IntegerqFn extends AFn {

        @Override
        public String getName() {
            return "integer?";
        }

        @Override
        public Object invoke(Object arg1) {

            if (arg1 instanceof Long) {
                return Boolean.TRUE;
            } else if (arg1 instanceof Double) {
                Double dbl = (Double) arg1;
                return isInt(dbl);

            } else if (arg1 instanceof Complex) {
                Complex co = (Complex) arg1;

                return co.im() == 0.0 && isInt(co.re());

            }
            return Boolean.FALSE;
        }

        boolean isInt(Double dbl) {
            long ln = dbl.longValue();
            return dbl.doubleValue() == ln;
        }
    }

    public static class ListVectorFn extends AFn {

        @Override
        public String getName() {
            return "list->vector";
        }

        @Override
        public Object invoke(Object arg1) {

            if (arg1 instanceof EmptyList) {
                Object[] arr = new Object[0];
                return new SArray(arr);
            }

            if (arg1 instanceof Cell) {
                return toArray((Cell) arg1);
            }

            throw new IllegalArgumentException("list->vector");
        }

        SArray toArray(Cell start) {

            ArrayList<Object> list = new ArrayList<>();
            Cell x = start;
            while (x instanceof Cell) {

                list.add(x.getCar());
                Object cdr = x.getCdr();
                if (cdr instanceof Cell) {
                    x = (Cell) cdr;
                } else {
                    break;
                }

            }
            Object[] arr = list.toArray();
            return new SArray(arr, true);
        }
    }

    public static class NumeratorFn extends AFn {

        @Override
        public String getName() {
            return "numerator";
        }

        ExactFn exactFn = new ExactFn();

        @Override
        public Object invoke(Object arg1) {

            if (arg1 instanceof Ratio) {
                Ratio ra = (Ratio) arg1;

                Ratio irr = ra.getIrreducible();

                BigInteger numerator = irr.getNumerator();

                return numerator.longValueExact();
            }
            if (arg1 instanceof Double) {

                Object o = exactFn.invoke(arg1);
                if (o instanceof Ratio) {
                    Ratio rr = (Ratio) o;
                    Ratio ir = rr.getIrreducible();

                    BigInteger numerator = ir.getNumerator();

                    return numerator.doubleValue();
                }
                throw new IllegalArgumentException("numerator");
            }

            throw new IllegalArgumentException("numerator");
        }
    }

    public static class RationalqFn extends AFn {

        @Override
        public String getName() {
            return "rational?";
        }

        @Override
        public Object invoke(Object arg1) {

            if (arg1 instanceof Long) {
                return Boolean.TRUE;
            } else if (arg1 instanceof Ratio) {
                return Boolean.TRUE;
            }
            return Boolean.FALSE;
        }
    }

    public static class RealqFn extends AFn {

        @Override
        public String getName() {
            return "real?";
        }

        @Override
        public Object invoke(Object arg1) {

            if (arg1 instanceof Double) {
                return Boolean.TRUE;
            } else if (arg1 instanceof Long) {
                return Boolean.TRUE;
            } else if (arg1 instanceof Ratio) {
                return Boolean.TRUE;
            } else if (arg1 instanceof Complex) {
                Complex c = (Complex) arg1;
                return c.im() == 0.0;
            }
            return Boolean.FALSE;
        }
    }

    public static class StringListFn extends AFn {

        @Override
        public String getName() {
            return "string->list";
        }

        @Override
        public Object invoke(Object arg1) {

            SString ss = (SString) arg1;
            return stringList(ss);
        }

        @Override
        public Object invoke(Object arg1, Object arg2) {
            SString ss = (SString) arg1;
            int start = ((Number) arg2).intValue();
            int end = ss.length();

            return stringList(ss, start, end);
        }

        @Override
        public Object invoke(Object arg1, Object arg2, Object arg3) {
            SString ss = (SString) arg1;
            int start = ((Number) arg2).intValue();
            int end = ((Number) arg3).intValue();

            return stringList(ss, start, end);
        }

        public static Object stringList(SString str, int start, int end) {

            int len = str.length();
            if (end > len) {
                throw new IllegalArgumentException("string-list, end");
            }
            if (start < 0) {
                throw new IllegalArgumentException("string-list, start");
            }
            if (start > end) {
                throw new IllegalArgumentException("string-list, start, end");
            }

            Object cdr = RT.EOL;
            for (int i = end - 1; i >= start; i--) {
                int code = str.getNth(i);
                SChar sc = SChar.valueOf(code);
                cdr = new Cell(sc, cdr);

            }
            return cdr;
        }

        public static Object stringList(SString str) {

            int len = str.length();

            Object cdr = RT.EOL;
            for (int i = len - 1; i >= 0; i--) {
                int code = str.getNth(i);
                SChar sc = SChar.valueOf(code);
                cdr = new Cell(sc, cdr);

            }
            return cdr;
        }
    }

    public static class RemainderFn extends AFn {

        @Override
        public String getName() {
            return "remainder";
        }

        TruncateSlashFn fn = new TruncateSlashFn();

        @Override
        public Object invoke(Object n, Object y) {

            //  truncate-remainder と 等価であり
            Values v = (Values) fn.invoke(n, y);

            return v.getValue2();
        }
    }

    static TruncateSlashFn truncateSlashFn = new TruncateSlashFn();

    public static class TruncateSlashFn extends AFn {

        @Override
        public String getName() {
            return "truncate/";
        }

        @Override
        public Object invoke(Object n, Object y) {

            if (n instanceof Long) {
                Long ln = (Long) n;
                if (y instanceof Long) {
                    Long yy = (Long) y;

                    if (ln < 0 && yy > 0) {

                        long r1 = (ln) / yy;
                        long r2 = ln - (r1 * yy);
                        return new Values(r1, r2);

                    } else if (ln > 0 && yy < 0) {

                        long r1 = -((ln) / -yy);
                        long r2 = ln - (r1 * yy);
                        return new Values(r1, r2);

                    } else if (ln < 0 && yy < 0) {

                        long r1 = ln / yy;
                        long r2 = ln - (r1 * yy);
                        return new Values(r1, r2);

                    } else {
                        long r1 = ln / yy;
                        long r2 = ln % yy;
                        return new Values(r1, r2);
                    }
                }
            }

            if (n instanceof Long) {
                Long ln = (Long) n;
                if (y instanceof Double) {
                    Long yy = ((Double) y).longValue();

                    if (ln < 0 && yy > 0) {

                        long r1 = (ln) / yy;
                        double r2 = ln - (r1 * yy);
                        return new Values(r1, r2);

                    } else if (ln > 0 && yy < 0) {

                        long r1 = -((ln) / -yy);
                        double r2 = ln - (r1 * yy);
                        return new Values(r1, r2);

                    } else if (ln < 0 && yy < 0) {

                        long r1 = ln / yy;
                        double r2 = ln - (r1 * yy);
                        return new Values(r1, r2);

                    } else {
                        long r1 = ln / yy;
                        double r2 = ln % yy;
                        return new Values(r1, r2);
                    }
                }
            }
            if (n instanceof Double) {
                Double ln = (Double) n;
                if (y instanceof Long) {
                    Long yy = (Long) y;

                    if (ln < 0 && yy > 0) {

                        double r1 = (ln) / yy;
                        double r2 = ln - (r1 * yy);
                        return new Values(r1, r2);

                    } else if (ln > 0 && yy < 0) {

                        double r1 = -((ln) / -yy);
                        double r2 = ln - (r1 * yy);
                        return new Values(r1, r2);

                    } else if (ln < 0 && yy < 0) {

                        double r1 = Math.floor(ln / yy);
                        double r2 = ln - (r1 * yy);
                        return new Values(r1, r2);

                    } else {
                        double r1 = ln / yy;
                        double r2 = ln % yy;
                        return new Values(r1, r2);
                    }
                }
            }
            throw new UnsupportedOperationException();
        }
    }

    public static class TruncateQuotientFn extends AFn {

        @Override
        public String getName() {
            return "truncate-quotient";
        }

        @Override
        public Object invoke(Object n, Object y) {
            Values v = (Values) truncateSlashFn.invoke(n, y);
            return v.getValue1();
        }
    }

    public static class TruncateRemainderFn extends AFn {

        @Override
        public String getName() {
            return "truncate-remainder";
        }

        @Override
        public Object invoke(Object n, Object y) {
            Values v = (Values) truncateSlashFn.invoke(n, y);
            return v.getValue2();
        }
    }

    public static class RoundFn extends AFn {

        @Override
        public String getName() {
            return "round";
        }

        @Override
        public Object invoke(Object arg1) {

            if (arg1 instanceof Double) {

                double d1 = ((Number) arg1).doubleValue();

                double d = Math.round(d1);
                return d;
            } else if (arg1 instanceof Ratio) {
                double d1 = ((Number) arg1).doubleValue();

                return Math.round(d1);  // long
            } else if (arg1 instanceof Long) {
                return arg1;

            }
            throw new IllegalArgumentException("round");
        }
    }

    public static class ListStringFn extends AFn {

        @Override
        public String getName() {
            return "list-string";
        }

        CircularListq circular = new CircularListq();

        @Override
        public Object invoke(Object arg1) {

            Boolean b = (Boolean) circular.invoke(arg1);
            if (b) {
                // 循環リスト なら、
                throw new IllegalArgumentException("list-string, circular-list");
            }
            ArrayList<Integer> list = new ArrayList<>();
            if (arg1 instanceof EmptyList) {
                return SString.fromList(list);
            }

            Object x = arg1;
            while (x instanceof Cell) {
                Cell cell = (Cell) x;
                SChar c = (SChar) cell.getCar();
                list.add(c.getCodepoint());
                x = cell.getCdr();
            }
            return SString.fromList(list);
        }
    }

    public static class StringAppendFn extends AFn {

        @Override
        public String getName() {
            return "string-append";
        }

        @Override
        public Object invoke(Object arg1) {
            return (arg1);
        }

        @Override
        public Object invoke(Object arg1, Object arg2) {

            SString s1 = (SString) arg1;
            SString s2 = (SString) arg2;

            return SString.append(s1, s2, true);
        }

        @Override
        public Object invoke(Object arg1, Object arg2, Object arg3) {

            SString s1 = (SString) arg1;
            SString s2 = (SString) arg2;
            SString s3 = (SString) arg3;

            return SString.append(s1,
                    SString.append(s2, s3, true),
                    true);
        }
    }

    public static class StringCopyFn extends AFn {

        @Override
        public String getName() {
            return "string-copy";
        }

        @Override
        public Object invoke(Object arg1) {

            SString str = (SString) arg1;
            int nth = 0;
            int end = str.length();
            return str.substring(nth, end);
        }

        @Override
        public Object invoke(Object arg1, Object arg2) {

            SString str = (SString) arg1;
            Number num = (Number) arg2;
            int nth = num.intValue();
            int end = str.length();
            return str.substring(nth, end);
        }

        @Override
        public Object invoke(Object arg1, Object arg2, Object arg3) {

            SString str = (SString) arg1;
            Number num = (Number) arg2;
            int nth = num.intValue();

            Number num2 = (Number) arg3;
            int end = num2.intValue();

            return str.substring(nth, end);
        }
    }

    public static class StringCopyeFn extends AFn {

        @Override
        public String getName() {
            return "string-copy!";
        }

        @Override
        public Object invoke(Object arg1, Object arg2, Object arg3) {
            SString ss = (SString) arg1;

            long end = ss.length();
            return invoke(arg1, arg2, arg3, 0L, end);
        }

        @Override
        public Object invoke(Object arg1, Object arg2, Object arg3, Object arg4) {

            SString ss = (SString) arg1;
            long end = ss.length();
            return invoke(arg1, arg2, arg3, arg4, end);
        }

        @Override
        public Object invoke(Object arg1, Object arg2, Object arg3, Object arg4, Object arg5) {

            SString ss = (SString) arg1;
            int at = ((Number) arg2).intValue();
            SString from = (SString) arg3;
            int start = ((Number) arg4).intValue();
            int end = ((Number) arg5).intValue();

            ss.copyFrom(from, at, start, end);

            return Boolean.TRUE;
        }
    }

    public static class StringFillFn extends AFn {

        @Override
        public String getName() {
            return "string-fill";
        }

        @Override
        public Object invoke(Object arg1, Object arg2) {

            SString ss = (SString) arg1;
            long ln = ss.length();
            return invoke(arg1, arg2, 0L, ln);
        }

        @Override
        public Object invoke(Object arg1, Object arg2, Object arg3) {

            SString ss = (SString) arg1;
            long ln = ss.length();
            return invoke(arg1, arg2, arg3, ln);
        }

        @Override
        public Object invoke(Object arg1, Object arg2, Object arg3, Object arg4) {
            SString ss = (SString) arg1;
            SChar ch = (SChar) arg2;
            int start = ((Number) arg3).intValue();
            int end = ((Number) arg4).intValue();
            int len = ss.length();
            if (start < 0 || len <= start) {
                throw new IllegalArgumentException("string-fill, start");
            }
            if (end < 0 || len < end) {
                throw new IllegalArgumentException("string-fill, end");
            }
            if (end < start) {
                throw new IllegalArgumentException("string-fill, start, end");
            }

            for (int i = start; i < end; i++) {
                ss.setNthChar(i, ch);
            }
            return Boolean.FALSE;
        }
    }

    public static class StringUtf8Fn extends AFn {

        @Override
        public String getName() {
            return "string->utf8";
        }

        @Override
        public Object invoke(Object arg1, Object arg2, Object arg3) {

            SString ss = (SString) arg1;
            int len = ss.length();
            int start = ((Number) arg2).intValue();
            if (start < 0 || len < start) {
                throw new IllegalArgumentException("string->utf8");
            }
            int end = ((Number) arg3).intValue();
            if (end < 0 || len < end) {
                throw new IllegalArgumentException("string->utf8");
            }
            if (!(start <= end)) {
                throw new IllegalArgumentException("string->utf8");
            }

            SString str = ss.substring(start, end);
            String work = str.toString();

            byte[] arr = null;
            try {
                arr = work.getBytes("UTF-8");
            } catch (UnsupportedEncodingException ex) {
                throw new RuntimeException("string->utf8", ex);
            }
            return new U8Array(arr, true);
        }

        @Override
        public Object invoke(Object arg1, Object arg2) {

            SString ss = (SString) arg1;
            int len = ss.length();
            return invoke(arg1, arg2, len);
        }

        @Override
        public Object invoke(Object arg1) {

            SString ss = (SString) arg1;
            int len = ss.length();
            return invoke(arg1, 0L, len);
        }
    }

    public static class StringVectorFn extends AFn {

        @Override
        public String getName() {
            return "string->vector";
        }

        @Override
        public Object invoke(Object arg1) {

            SString str = (SString) arg1;
            int len = str.length();
            ArrayList<Object> list = new ArrayList<>();
            for (int i = 0; i < len; i++) {
                list.add(str.getNthChar(i));
            }

            Object[] toArray = list.toArray();

            return new SArray(toArray, true);
        }

        @Override
        public Object invoke(Object arg1, Object arg2) {

            SString str = (SString) arg1;
            int len = str.length();

            int start = ((Number) arg2).intValue();
            if (start < 0 || len < start) {
                throw new IllegalArgumentException("string->vector");
            }

            ArrayList<Object> list = new ArrayList<>();
            for (int i = start; i < len; i++) {
                list.add(str.getNthChar(i));
            }

            Object[] toArray = list.toArray();

            return new SArray(toArray, true);
        }

        @Override
        public Object invoke(Object arg1, Object arg2, Object arg3) {

            SString str = (SString) arg1;
            int len = str.length();

            int start = ((Number) arg2).intValue();
            if (start < 0 || len < start) {
                throw new IllegalArgumentException("string->vector");
            }

            int end = ((Number) arg3).intValue();
            if (end < 0 || len < end) {
                throw new IllegalArgumentException("string->vector");
            }
            if (!(start <= end)) {
                throw new IllegalArgumentException("string->vector");
            }

            ArrayList<Object> list = new ArrayList<>();
            for (int i = start; i < end; i++) {
                list.add(str.getNthChar(i));
            }

            Object[] toArray = list.toArray();
            return new SArray(toArray, true);
        }
    }

    public static class ValuesFn extends AFn {

        @Override
        public String getName() {
            return "values";
        }

        @Override
        public Object invoke() {
            return new Values();
        }

        @Override
        public Object invoke(Object arg1) {
            return new Values(arg1);
        }

        @Override
        public Object invoke(Object arg1, Object arg2) {
            return new Values(arg1, arg2);
        }

        @Override
        public Object invoke(Object arg1, Object arg2, Object arg3) {
            return new Values(arg1, arg2, arg3);
        }

        @Override
        public Object invoke(Object arg1, Object arg2, Object arg3, Object arg4) {
            return new Values(arg1, arg2, arg3, arg4);
        }
    }

    public static class VectorFn extends AFn {

        @Override
        public String getName() {
            return "vector";
        }

        @Override
        public Object invoke() {

            Object[] arr = new Object[0];
            return new SArray(arr, true);
        }

        @Override
        public Object invoke(Object arg1) {

            Object[] arr = new Object[]{arg1};
            return new SArray(arr, true);
        }

        @Override
        public Object invoke(Object arg1, Object arg2) {

            Object[] arr = new Object[]{arg1, arg2};
            return new SArray(arr, true);
        }

        @Override
        public Object invoke(Object arg1, Object arg2, Object arg3) {

            Object[] arr = new Object[]{arg1, arg2, arg3};
            return new SArray(arr, true);
        }

        @Override
        public Object invoke(Object arg1, Object arg2, Object arg3, Object arg4) {

            Object[] arr = new Object[]{arg1, arg2, arg3, arg4};
            return new SArray(arr, true);
        }

        @Override
        public Object invoke(Object arg1, Object arg2, Object arg3, Object arg4, Object arg5) {

            Object[] arr = new Object[]{arg1, arg2, arg3, arg4, arg5};
            return new SArray(arr, true);
        }

        @Override
        public Object invoke(Object arg1, Object arg2, Object arg3, Object arg4, Object arg5, Object arg6) {

            Object[] arr = new Object[]{arg1, arg2, arg3, arg4, arg5, arg6};
            return new SArray(arr, true);
        }

        @Override
        public Object invoke(Object arg1, Object arg2, Object arg3, Object arg4, Object arg5, Object arg6, Object arg7) {

            Object[] arr = new Object[]{arg1, arg2, arg3, arg4, arg5, arg6, arg7};
            return new SArray(arr, true);
        }

        @Override
        public Object invoke(Object arg1, Object arg2, Object arg3, Object arg4, Object arg5, Object arg6, Object arg7, Object arg8) {

            Object[] arr = new Object[]{arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8};
            return new SArray(arr, true);
        }
    }

    public static class VectorLengthFn extends AFn {

        @Override
        public String getName() {
            return "vector-length";
        }

        @Override
        public Object invoke(Object arg1) {

            SArray arr = (SArray) arg1;
            long ln = arr.length();
            return ln;
        }
    }

    public static class VectorRefFn extends AFn {

        @Override
        public String getName() {
            return "vector-ref";
        }

        @Override
        public Object invoke(Object arg1, Object arg2) {

            int it = ((Number) arg2).intValue();
            SArray arr = (SArray) arg1;
            return arr.getNth(it);
        }
    }

    public static class VectorSetFn extends AFn {

        @Override
        public String getName() {
            return "vector-set";
        }

        @Override
        public Object invoke(Object arg1, Object arg2, Object arg3) {

            SArray sa = (SArray) arg1;
            int nth = ((Number) arg2).intValue();
            return sa.setNth(nth, arg3);
        }
    }

    public static class VectorListFn extends AFn {

        @Override
        public String getName() {
            return "vector->list";
        }

        @Override
        public Object invoke(Object arg1) {

            SArray sa = (SArray) arg1;
            int len = sa.length();

            Object result = RT.EOL;
            for (int i = len - 1; i >= 0; i--) {
                result = new Cell(sa.getNth(i), result);
            }
            return result;
        }

        @Override
        public Object invoke(Object arg1, Object arg2) {

            SArray sa = (SArray) arg1;
            int len = sa.length();
            int start = ((Number) arg2).intValue();
            if (start < 0 || len < start) {
                throw new IllegalArgumentException("vector-> list");
            }

            Object result = RT.EOL;
            for (int i = len - 1; i >= start; i--) {
                result = new Cell(sa.getNth(i), result);
            }
            return result;
        }

        @Override
        public Object invoke(Object arg1, Object arg2, Object arg3) {

            SArray sa = (SArray) arg1;
            int len = sa.length();
            int start = ((Number) arg2).intValue();
            if (start < 0 || len < start) {
                throw new IllegalArgumentException("vector-> list");
            }

            int end = ((Number) arg3).intValue();
            if (end < 0 || len < end) {
                throw new IllegalArgumentException("vector-> list");
            }
            if (!(start <= end)) {
                throw new IllegalArgumentException("vector-> list");
            }

            Object result = RT.EOL;
            for (int i = end - 1; i >= start; i--) {
                result = new Cell(sa.getNth(i), result);
            }
            return result;
        }
    }

    public static class VectorStringFn extends AFn {

        @Override
        public String getName() {
            return "vector->string";
        }

        @Override
        public Object invoke(Object arg1) {

            SArray vec = (SArray) arg1;

            int len = vec.length();
            SString work = new SString(len, 0);

            for (int i = 0; i < len; i++) {
                Object o = vec.getNth(i);
                SChar s = (SChar) o;
                work.setNthChar(i, s);
            }

            return work;
        }

        @Override
        public Object invoke(Object arg1, Object arg2) {

            SArray vec = (SArray) arg1;
            int len = vec.length();
            int start = ((Number) arg2).intValue();
            if (start < 0 || len < start) {
                throw new IllegalArgumentException("vector->string");
            }

            SString work = new SString(len - start, 0);

            int j = 0;
            for (int i = start; i < len; i++) {
                Object o = vec.getNth(i);
                SChar s = (SChar) o;
                work.setNthChar(j++, s);
            }

            return work;
        }

        @Override
        public Object invoke(Object arg1, Object arg2, Object arg3) {

            SArray vec = (SArray) arg1;
            int len = vec.length();
            int start = ((Number) arg2).intValue();
            if (start < 0 || len < start) {
                throw new IllegalArgumentException("vector->string");
            }

            int end = ((Number) arg3).intValue();
            if (end < 0 || len < end) {
                throw new IllegalArgumentException("vector->string");
            }
            if (!(start <= end)) {
                throw new IllegalArgumentException("vector->string");
            }

            SString work = new SString(end - start, 0);

            int j = 0;
            for (int i = start; i < end; i++) {
                Object o = vec.getNth(i);
                SChar s = (SChar) o;
                work.setNthChar(j++, s);
            }

            return work;
        }
    }

    public static class VectorCopy extends AFn {

        @Override
        public String getName() {
            return "vector-copy";
        }

        @Override
        public Object invoke(Object arg1) {

            SArray sa = (SArray) arg1;
            int len = sa.length();
            return sa.subvector(0, len);
        }

        @Override
        public Object invoke(Object arg1, Object arg2) {

            SArray sa = (SArray) arg1;
            int len = sa.length();
            int start = ((Number) arg2).intValue();
            if (start < 0 || len < start) {
                throw new LispException("Bad index to vector-copy: " + start);
            }

            return sa.subvector(start, len);
        }

        @Override
        public Object invoke(Object arg1, Object arg2, Object arg3) {

            SArray sa = (SArray) arg1;
            int len = sa.length();
            int start = ((Number) arg2).intValue();
            if (start < 0 || len < start) {
                throw new LispException("Bad index to vector-copy: " + start);
            }

            int end = ((Number) arg3).intValue();
            if (end < 0 || len < end) {
                throw new LispException("Bad index to vector-copy: " + start);
            }
            if (!(start <= end)) {
                throw new LispException("Bad index to vector-copy: " + start);
            }

            return sa.subvector(start, end);
        }
    }

    public static class VectorAppendFn extends AFn {

        @Override
        public String getName() {
            return "vector-append";
        }

        @Override
        public Object invoke(Object arg1) {
            return (arg1);
        }

        @Override
        public Object invoke(Object arg1, Object arg2) {

            SArray s1 = (SArray) arg1;
            SArray s2 = (SArray) arg2;

            return SArray.append(s1, s2, true);
        }

        @Override
        public Object invoke(Object arg1, Object arg2, Object arg3) {

            SArray s1 = (SArray) arg1;
            SArray s2 = (SArray) arg2;
            SArray s3 = (SArray) arg3;

            return SArray.append(s1,
                    SArray.append(s2, s3, true),
                    true);
        }
    }

    public static class VectorFillFn extends AFn {

        @Override
        public String getName() {
            return "vector-fill";
        }

        @Override
        public Object invoke(Object arg1, Object arg2) {

            SArray ss = (SArray) arg1;
            long ln = ss.length();
            return invoke(arg1, arg2, 0L, ln);
        }

        @Override
        public Object invoke(Object arg1, Object arg2, Object arg3) {

            SArray ss = (SArray) arg1;
            long ln = ss.length();
            return invoke(arg1, arg2, arg3, ln);
        }

        @Override
        public Object invoke(Object arg1, Object arg2, Object arg3, Object arg4) {
            SArray ss = (SArray) arg1;
            Object ch = arg2;
            int start = ((Number) arg3).intValue();
            int end = ((Number) arg4).intValue();
            int len = ss.length();
            if (start < 0 || len <= start) {
                throw new IllegalArgumentException("vector-fill, start");
            }
            if (end < 0 || len < end) {
                throw new IllegalArgumentException("vector-fill, end");
            }
            if (end < start) {
                throw new IllegalArgumentException("vector-fill, start, end");
            }

            for (int i = start; i < end; i++) {
                ss.setNth(i, ch);
            }
            return Boolean.FALSE;
        }
    }

    public static class VectorCopyeFn extends AFn {

        @Override
        public String getName() {
            return "vector-copy!";
        }

        @Override
        public Object invoke(Object arg1, Object arg2, Object arg3) {

            SArray ss = (SArray) arg1;
            long end = ss.length();
            return invoke(arg1, arg2, arg3, 0L, end);
        }

        @Override
        public Object invoke(Object arg1, Object arg2, Object arg3, Object arg4) {

            SArray ss = (SArray) arg1;
            long end = ss.length();
            return invoke(arg1, arg2, arg3, arg4, end);
        }

        @Override
        public Object invoke(Object arg1, Object arg2, Object arg3, Object arg4, Object arg5) {

            SArray ss = (SArray) arg1;
            int at = ((Number) arg2).intValue();
            SArray from = (SArray) arg3;
            int start = ((Number) arg4).intValue();
            int end = ((Number) arg5).intValue();

            ss.copyFrom(from, at, start, end);

            return Boolean.TRUE;
        }
    }

    public static class EqqFn extends AFn {

        @Override
        public String getName() {
            return "eq?";
        }

        @Override
        public Object invoke(Object arg1, Object arg2) {

            if (arg1 == arg2) {
                return true;
            }
            if ((arg1 instanceof SSymbol) && (arg2 instanceof SSymbol)) {
                SSymbol s1 = (SSymbol) arg1;
                SSymbol s2 = (SSymbol) arg2;
                return s1.equals(s2);
            }

            return false;
        }

    }

    public static class EofObjectQFn extends AFn {

        @Override
        public String getName() {
            return "eof-object?";
        }

        @Override
        public Object invoke(Object arg1) {

            return (arg1 instanceof EndOfFile);
        }

    }

    public static class EofObjectFn extends AFn {

        @Override
        public String getName() {
            return "eof-object";
        }

        @Override
        public Object invoke() {

            return EndOfFile.INSTANCE;
        }

    }

    public static class FeaturesFn extends AFn {

        @Override
        public String getName() {
            return "features";
        }

        static Object LIST = RT.immutableList(
                SSymbol.FEAT_R7RS, // SSymbol("r7rs");
                SSymbol.FEAT_UNICODE, // SSymbol("full-unicode");
                SSymbol.FEAT_JVM, //  SSymbol("jvm");
                SSymbol.FEAT_UIKA, // SSymbol("uika806");
                SSymbol.FEAT_UIKA_VER // SSymbol("uika806-0.1");
        );

        @Override
        public Object invoke() {

            return LIST;
        }
    }

    public static class InexactFn extends AFn {

        @Override
        public String getName() {
            return "inexact";
        }

        @Override
        public Object invoke(Object arg1) {

            if (arg1 instanceof Double) {
                return arg1;
            } else if (arg1 instanceof Long) {
                double ln = (Long) arg1;
                return ln;
            } else if (arg1 instanceof Complex) {
                return arg1;
            } else if (arg1 instanceof Ratio) {
                Ratio ra = (Ratio) arg1;
                return ra.doubleValue();
            }
            throw new IllegalArgumentException("inexact");
        }

    }

    public static class CeilingFn extends AFn {

        @Override
        public String getName() {
            return "ceiling";
        }

        @Override
        public Object invoke(Object arg1) {

            if (arg1 instanceof Number) {

                Double d1 = (Double) arg1;

                return Math.ceil(d1);

            }
            throw new IllegalArgumentException("ceiling");
        }

    }

    public static class TruncateFn extends AFn {

        @Override
        public String getName() {
            return "truncate";
        }

        @Override
        public Object invoke(Object arg1) {

            if (arg1 instanceof Long) {
                return arg1;
            } else if (arg1 instanceof Double) {
                Double d = (Double) arg1;

                double si = Math.signum(d);
                double abs = Math.abs(d);
                double f = Math.floor(abs);
                return si * f;
            }

            throw new IllegalArgumentException("truncate");
        }
    }

    public static class StringRefFn extends AFn {

        @Override
        public String getName() {
            return "string-ref";
        }

        @Override
        public Object invoke(Object arg1, Object arg2) {

            SString str = (SString) arg1;
            Number num = (Number) arg2;
            int nth = num.intValue();
            int code = str.getNth(nth);
            return SChar.valueOf(code);
        }

    }

    public static class StringqFn extends AFn {

        @Override
        public String getName() {
            return "string?";
        }

        @Override
        public Object invoke(Object arg1) {

            if (arg1 instanceof SString) {
                return Boolean.TRUE;
            }
            return Boolean.FALSE;
        }

    }

    public static class IntegerChar extends AFn {

        @Override
        public String getName() {
            return "integer->char";
        }

        @Override
        public Object invoke(Object arg1) {

            int codePoint = ((Number) arg1).intValue();

            return SChar.valueOf(codePoint);

        }

    }

    public static class FloorFn extends AFn {

        @Override
        public String getName() {
            return "floor";
        }

        @Override
        public Object invoke(Object arg1) {

            if (arg1 instanceof Number) {

                Double d1 = (Double) arg1;
                return Math.floor(d1);
            }
            throw new IllegalArgumentException("floor");
        }
    }

    public static class NumberStringFn extends AFn {

        @Override
        public String getName() {
            return "number->string";
        }

        @Override
        public Object invoke(Object arg1, Object arg2) {

            if (!(arg1 instanceof Long)) {

                String clazz = (arg1 == null) ? "null" : arg1.getClass().getName();
                throw new LispException("Unsuport arg1, number->string " + clazz);
            }

            if (arg2 instanceof Long) {
                int radix = ((Number) arg2).intValue();
                switch (radix) {
                    case 2:
                        return Long.toBinaryString((Long) arg1);
                    case 8:
                        return Long.toOctalString((Long) arg1);

                    case 10:
                        return Long.toString((Long) arg1);
                    case 16:
                        return Long.toHexString((Long) arg1);

                }
                throw new LispException("number->string, radix = " + arg2);

            }
            throw new LispException("number->string");
        }

        @Override
        public Object invoke(Object arg1) {
            return invoke(arg1, 10L);
        }

    }

    /**
     * boolean=?
     *
     * @author hemmi
     */
    public static class BooleanEqqFn extends AFn {

        @Override
        public String getName() {
            return "boolean=?";
        }

        @Override
        public Object invoke(Object arg1, Object arg2, Object arg3) {

            return eq(arg1, arg2) && eq(arg2, arg3);
        }

        @Override
        public Object invoke(Object arg1, Object arg2) {

            return eq(arg1, arg2);
        }

        boolean eq(Object arg1, Object arg2) {

            if ((arg1 instanceof Boolean) && (arg2 instanceof Boolean)) {
                return arg1.equals(arg2);
            }
            return false;
        }

    }

    public static class EvenqFn extends AFn {

        @Override
        public String getName() {
            return "even?";
        }

        @Override
        public Object invoke(Object arg1) {

            if (arg1 instanceof Long) {
                Long ln = (Long) arg1;
                return (ln.longValue() & 1) == 0;
            }
            return Boolean.FALSE;
        }
    }

    public static class ListRefFn extends AFn {

        @Override
        public String getName() {
            return "list-ref";
        }

        @Override
        public Object invoke(final Object arg1, final Object arg2) {

            int n = ((Number) arg2).intValue();
            if (n < 0) {
                throw new IllegalArgumentException("Index of list-ref must be positive");
            }

            Object x = arg1;
            for (;;) {
                if (n == 0) {
                    if (x instanceof Cell) {
                        Cell cell = (Cell) x;

                        return cell.getCar();
                    }
                    throw new IllegalArgumentException("list-ref");
                }

                if (x instanceof Cell) {
                    Cell cell = (Cell) x;

                    x = cell.getCdr();
                    n--;

                } else {
                    throw new IllegalArgumentException("list-ref");
                }

            }
        }

    }

    public static class ListSetFn extends AFn {

        @Override
        public String getName() {
            return "list-set";
        }

        @Override
        public Object invoke(Object arg1, Object arg2, Object arg3) {

            int n = ((Number) arg2).intValue();
            if (n < 0) {
                throw new IllegalArgumentException("Index of list-set must be positive");
            }

            Object x = arg1;
            for (;;) {
                if (n == 0) {
                    if (x instanceof Cell) {
                        Cell cell = (Cell) x;

                        cell.setCar(arg3);
                        return Boolean.TRUE;
                    }
                    return Boolean.FALSE;
                }

                if (x instanceof Cell) {
                    Cell cell = (Cell) x;

                    x = cell.getCdr();
                    n--;

                } else {
                    throw new IllegalArgumentException("list-set!");
                }

            }
        }

    }

    public static class MakeByteVectorFn extends AFn {

        @Override
        public String getName() {
            return "make-bytevector";
        }

        @Override
        public Object invoke(Object arg1) {

            Number num = (Number) arg1;
            int siz = num.intValue();

            return new U8Array(siz, Byte.MAX_VALUE);
        }

        @Override
        public Object invoke(Object arg1, Object arg2) {

            Number num = (Number) arg1;
            int siz = num.intValue();

            byte by = ((Number) arg2).byteValue();

            return new U8Array(siz, by);
        }

    }

    public static class OddqFn extends AFn {

        @Override
        public String getName() {
            return "odd?";
        }

        @Override
        public Object invoke(Object arg1) {

            if (arg1 instanceof Long) {
                Long ln = (Long) arg1;
                return (ln.longValue() & 1) == 1;
            }
            return Boolean.FALSE;
        }

    }

    public static class MakeListFn extends AFn {

        @Override
        public String getName() {
            return "make-list";
        }

        @Override
        public Object invoke(Object length, Object obj) {

            int len = ((Number) length).intValue();
            if (len < 0) {
                throw new IllegalArgumentException("make-list");
            }
            Object last = EmptyList.NIL;
            for (int i = 0; i < len; i++) {

                last = new Cell(obj, last);
            }
            return last;
        }

    }

    public static class MakeVectorFn extends AFn {

        @Override
        public String getName() {
            return "make-vector";
        }

        @Override
        public Object invoke(Object arg1) {

            Number num = (Number) arg1;
            int siz = num.intValue();

            return new SArray(siz, Boolean.FALSE);
        }

        @Override
        public Object invoke(Object arg1, Object arg2) {

            Number num = (Number) arg1;
            int siz = num.intValue();

            return new SArray(siz, arg2);
        }

    }

    public static class LtFn extends AFn {

        @Override
        public String getName() {
            return "<";
        }

        @Override
        public Object invoke(Object arg1, Object arg2, Object arg3) {
            return lt(arg1, arg2) && lt(arg2, arg3);
        }

        @Override
        public Object invoke(Object arg1, Object arg2) {
            return lt(arg1, arg2);
        }

        boolean lt(Object arg1, Object arg2) {

            if (arg1 instanceof Number) {
                if (arg2 instanceof Number) {
                    Number n1 = (Number) arg1;
                    Number n2 = (Number) arg2;

                    return n1.doubleValue() < n2.doubleValue();
                }
            }
            return false;
        }

    }

    /*
     */
    public static class StringSymbolFn extends AFn {

        @Override
        public String getName() {
            return "string->symbol";
        }

        @Override
        public Object invoke(Object arg1) {

            SString ss = (SString) arg1;
          //  return new SSymbol(ss.toString(), SSymbol.NAME_SPC);
            return new SSymbol(ss.toString());
        }
    }

    // disable construct
    private LibBase() {

    }
}
