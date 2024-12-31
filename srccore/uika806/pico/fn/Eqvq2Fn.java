/**
 * SPDX-License-Identifier: MPL-2.0
 * Author:  Masahito Hemmi.
 */
package uika806.pico.fn;

import java.util.Objects;

import uika806.objects.SChar;
import uika806.kernel.AFn;
import uika806.objects.Cell;
import uika806.err.LispException;
import uika806.kernel.RT;
import uika806.objects.SSymbol;
import uika806.objects.SString;
import uika806.objects.Complex;
import uika806.objects.Ratio;
import uika806.objects.SArray;
import uika806.objects.U8Array;

/**
 */
public class Eqvq2Fn extends AFn {

    @Override
    public String getName() {
        return "eqv";
    }

    @Override
    public Object invoke(Object x, Object y) {

        // obj1 と obj2 がともに #t またはともに #f である。
        if (x instanceof Boolean) {
            Boolean xx = (Boolean) x;
            if (y instanceof Boolean) {
                Boolean yy = (Boolean) y;
                return xx.equals(yy);
            } else {
                return Boolean.FALSE;
            }
        }
        // obj1 と obj2 がともにシンボルであって，かつ
        if (x instanceof SSymbol) {
            SSymbol xx = (SSymbol) x;
            if (y instanceof SSymbol) {
                SSymbol yy = (SSymbol) y;
                return xx.equals(yy);
            } else {
                return Boolean.FALSE;
            }
        }
        if (x instanceof Complex) {
            Complex xx = (Complex) x;
            if (y instanceof Complex) {
                Complex yy = (Complex) y;

                return (xx.re() == yy.re()) && (xx.im() == yy.im());

            } else {
                return Boolean.FALSE;
            }
        }

        // obj1 と obj2 がともに数であって, 数値的に等しく
        if (x instanceof Number) {
            Number xx = (Number) x;
            if (y instanceof Number) {
                Number yy = (Number) y;
                return numEqvq(xx, yy);
            } else {
                return Boolean.FALSE;
            }
        }
        // obj1 と obj2 がともに文字であって，かつ 
        if (x instanceof SChar) {
            SChar xx = (SChar) x;
            if (y instanceof SChar) {
                SChar yy = (SChar) y;
                return xx.equals(yy);
            } else {
                return Boolean.FALSE;
            }
        }
        // obj1 と obj2 がともに空リストである。
        if (RT.isNull(x) && RT.isNull(y)) {
            return Boolean.TRUE;
        }
        // obj1 と obj2 がともにペア
        if (x instanceof Cell) {
            if (y instanceof Cell) {
                return x == y;
            }
            return Boolean.FALSE;
        }

        //obj1 と obj2 がともに文字列
        if (x instanceof SString) {
            if (y instanceof SString) {
                //return x == y;
                return x.equals(y);
            }
            return Boolean.FALSE;
        }
        //obj1 と obj2 がともにVector 
        if (x instanceof SArray) {
            if (y instanceof SArray) {
                return eqVector((SArray) x, (SArray) y);
            }
            return Boolean.FALSE;
        }
        //obj1 と obj2 がともにU8Vector 
        if (x instanceof U8Array) {
            if (y instanceof U8Array) {
                return eqU8Vector((U8Array) x, (U8Array) y);
            }
            return Boolean.FALSE;
        }

        if (x instanceof AFn) {

            if (y instanceof AFn) {
                return x == y;
            }
            return Boolean.FALSE;
        }

        return Boolean.FALSE;
    }

    static boolean eqU8Vector(U8Array a, U8Array b) {

        int aLen = a.length();
        int bLen = b.length();
        if (aLen != bLen) {
            return false;
        }
        for (int i = 0; i < aLen; i++) {
            long oa = a.getNthAsInt(i);
            long ob = b.getNthAsInt(i);

            if (!(oa == ob)) {
                return false;
            }

        }
        return true;

    }

    static boolean eqVector(SArray a, SArray b) {

        int aLen = a.length();
        int bLen = b.length();
        if (aLen != bLen) {
            return false;
        }
        for (int i = 0; i < aLen; i++) {
            Object oa = a.getNth(i);
            Object ob = b.getNth(i);

            if (!(oa.equals(ob))) {
                return false;
            }

        }
        return true;

    }

    // 作成中
    static Boolean numEqvq(Object x, Object y) {

        if (x instanceof Long) {
            Long xx = (Long) x;
            if (y instanceof Long) {
                Long yy = (Long) y;
                return Objects.equals(xx, yy);
            } else if (y instanceof Ratio) {   //   ****   TODO x = Ratio, y = Long を同じにする
                Ratio ra = (Ratio) y;
                Ratio ra2 = ra.getIrreducible();

                long bunbo = ra2.getDenominator().longValue();
                if (bunbo != 1) {
                    return Boolean.FALSE;
                }
                long bunshi = ra2.getNumerator().longValue();

                return Objects.equals(xx, bunshi);
            } else {
                return Boolean.FALSE;
            }
        }
        if (x instanceof Double) {
            Double xx = (Double) x;
            if (y instanceof Double) {
                Double yy = (Double) y;
                return Objects.equals(xx, yy);
            } else {
                return Boolean.FALSE;
            }
        }

        if (x instanceof Ratio) {
            Ratio xx = (Ratio) x;
            if (y instanceof Ratio) {
                Ratio yy = (Ratio) y;

                return Objects.equals(xx.getNumerator(), yy.getNumerator())
                        && Objects.equals(xx.getDenominator(), yy.getDenominator());

            } else {
                return Boolean.FALSE;
            }
        }

        String a1 = (x == null) ? "null" : x.getClass().getName();
        String a2 = (y == null) ? "null" : y.getClass().getName();

        throw new LispException("Unsupported yet : x=" + a1 + ", y=" + a2);
    }

}
