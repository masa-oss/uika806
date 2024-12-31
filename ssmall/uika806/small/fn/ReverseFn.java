package uika806.small.fn;

import uika806.kernel.AFn;
import uika806.objects.Cell;
import uika806.objects.EmptyList;

/**
 *
 * @author hemmi
 */
public class ReverseFn extends AFn {

    @Override
    public String getName() {
        return "reverse";
    }

    CircularListq circular = new CircularListq();

    @Override
    public Object invoke(Object arg1) {

        Boolean b = (Boolean) circular.invoke(arg1);
        if (b) {
            throw new IllegalArgumentException("proper list required, for reverse");
        }

        Object res =  EmptyList.NIL ;
        Object x = arg1;
        for (;;) {
            if (x instanceof EmptyList) {
                return res;
            }
            if (!(x instanceof Cell)) {
                throw new IllegalArgumentException("proper list required, for reverse");
            }
            Cell cell = (Cell)x;
            res = new Cell( cell.getCar(), res  );
            
            x = cell.getCdr();
        }

    }

}
