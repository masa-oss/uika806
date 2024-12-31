/**
 * SPDX-License-Identifier: MPL-2.0
 * Author:  Masahito Hemmi.
 */
package uika806.syntax;

import uika806.objects.Cell;
import uika806.objects.EmptyList;

/**
 *
 * @author henmi
 */
public class SUtil {

    public static Object first(Object list) {
        return ((Cell) list).getCar();
    }

    public static Object getCadr(Object list) {

        Object wk = ((Cell) list).getCdr();
        return ((Cell) wk).getCar();
    }

    public static Object getCdar(Object list) {

        Object wk = ((Cell) list).getCar();
        return ((Cell) wk).getCdr();
    }

    public static int getLength(Object list) {

        int len = 0;
        while (list instanceof Cell) {
            list = ((Cell) list).getCdr();
            ++len;
        }
        return len;
    }

    public static Object toList(Object... list) {

        if (list.length == 0) {
            return EmptyList.NIL;
        }
        Cell start = new Cell(list[0], EmptyList.NIL);
        Cell end = start;
        for (int i = 1; i < list.length; i++) {
            Cell newend = new Cell(list[i], EmptyList.NIL);
            end.setCdr(newend);
            end = newend;
        }
        return start;
    }

}
