/**
 * SPDX-License-Identifier: MPL-2.0
 * Author:  Masahito Hemmi.
 */
package uika806.pico.fn;

import uika806.kernel.AFn;
import uika806.objects.Cell;
import uika806.objects.EmptyList;

/**
 *
 */
public class AppendFn extends AFn {

    @Override
    public String getName() {
        return "append";
    }

    @Override
    public Object invoke(Object arg1, Object arg2, Object arg3) {
        return append(arg1, append(arg2, arg3));

    }

    @Override
    public Object invoke(Object arg1, Object arg2) {
        return append(arg1, arg2);
    }

    Object append(Object arg1, Object arg2) {

        if (arg1 instanceof Cell) {
            Cell cell = (Cell) arg1;
            return new Cell(cell.getCar(), append(cell.getCdr(), arg2));

        }
        return EmptyList.NIL;
    }

}
