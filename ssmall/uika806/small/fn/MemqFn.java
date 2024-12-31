package uika806.small.fn;

import uika806.kernel.AFn;
import uika806.objects.Cell;
import uika806.kernel.RT;

/**
 *
 * @author hemmi
 */
public class MemqFn extends AFn {

    @Override
    public String getName() {
        return "memq";
    }

    
    EqqFn func = new EqqFn();

    @Override
    public Object invoke(Object key, Object list) {

        while (list instanceof Cell) {
            Object car = RT.car(list);
            Boolean b = (Boolean) func.invoke(key , car);
            if (b) {
                return list;
            }
            list = RT.cdr(list);
        }

        return Boolean.FALSE;
    }

}
