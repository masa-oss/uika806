package uika806.small.fn;

import uika806.kernel.AFn;
import uika806.objects.Cell;

/**
 *
 * @author hemmi
 */
public class ListTailFn extends AFn {

    CircularListq circular = new CircularListq();
    
    @Override
    public String getName() {
        return "list-tail";
    }


    @Override
    public Object invoke(Object list, Object k) {

        Boolean b = (Boolean) circular.invoke(list);
        if (b) {
            throw new IllegalArgumentException("proper list required, for list-tail");
        }
        int nth = ((Number)k).intValue();
        if (nth < 0) {
            throw new IllegalArgumentException("number must be positive, for list-tail");
        }

        Object x = list;
        for (;;) {
            if (nth == 0) {
                return x;
            }
            if (!(x instanceof Cell)) {
                throw new IllegalArgumentException("proper list required, for list-tail");
            }
            Cell cell = (Cell)x;
            
            x = cell.getCdr();
            nth --;
        }

    }

}
