package uika806.small.fn;

import uika806.kernel.AFn;
import uika806.objects.Cell;
import uika806.kernel.RT;

/**
 *
 * @author hemmi
 */
public class ListCopyFn extends AFn {

    CircularListq check = new CircularListq();

    @Override
    public String getName() {
        return "list-copy";
    }

    @Override
    public Object invoke(Object arg1) {

        Boolean b = (Boolean) check.invoke(arg1);
        if (b) {
            throw new IllegalArgumentException("CircularList to list-copy");
        }
        return copyIt(arg1);
    }

    public Object copyIt(Object arg1) {

        if (arg1 instanceof Cell) {
            return new Cell(RT.car(arg1), copyIt(RT.cdr(arg1)));
        } else {
            return arg1;
        }
    }

}
