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

/**
 *
 * @author hemmi
 */
public class EqvqFn extends AFn {

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
                return x == y;
            }
            return Boolean.FALSE;
        }
/*
        if (x instanceof NewMethodWrapper) {

            if (y instanceof NewMethodWrapper) {
                return x == y;
            }
            return Boolean.FALSE;
        }
*/
        return Boolean.FALSE;
    }
        

    // 作成中
    static Boolean numEqvq(Object x, Object y) {

        if (x instanceof Integer) {
            Integer xx = (Integer) x;
            if (y instanceof Integer) {
                Integer yy = (Integer) y;
                return Objects.equals(xx, yy);
            } else {
                return Boolean.FALSE;
            }
        }
        if (x instanceof Long) {
            Long xx = (Long) x;
            if (y instanceof Long) {
                Long yy = (Long) y;
                return Objects.equals(xx, yy);
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

        throw new LispException("Unsupported yet");
//        LOG.error("---------------- Unsupported yet");
  //      return Boolean.FALSE;
    }

    
}
