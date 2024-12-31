package uika806.small.fn;

import uika806.kernel.AFn;
import uika806.objects.Cell;
import uika806.objects.EmptyList;

/**
 *
 * @author hemmi
 */
public class LengthFn extends AFn {

    CircularListq circular = new CircularListq();
    
    @Override
    public String getName() {
        return "length";
    }


    @Override
    public Object invoke(Object arg1) {

        Boolean b = (Boolean) circular.invoke(arg1);
        if (b) {
            throw new IllegalArgumentException("proper list required, for length");
        }

        int len = 0;
        Object x = arg1;
        for (;;) {
            if (x instanceof EmptyList) {
                return (long)len;
            }
            if (!(x instanceof Cell)) {
                throw new IllegalArgumentException("proper list required, for length");
            }
            Cell cell = (Cell)x;
            x = cell.getCdr();
            len++;
        }

    }

}
