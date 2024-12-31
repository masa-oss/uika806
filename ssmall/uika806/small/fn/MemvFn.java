package uika806.small.fn;

import uika806.kernel.AFn;
import uika806.objects.Cell;
import uika806.kernel.RT;
import uika806.pico.fn.EqvqFn;

/**
 *
 * @author hemmi
 */
public class MemvFn extends AFn {

    @Override
    public String getName() {
        return "memv";
    }

    EqvqFn eqv = new EqvqFn();

    @Override
    public Object invoke(Object key, Object list) {

        while (list instanceof Cell) {
            Object car = RT.car(list);
            Boolean b = (Boolean) eqv.invoke(key, car);
            if (b) {
                return list;
            }
            list = RT.cdr(list);
        }

        return Boolean.FALSE;
    }

}
