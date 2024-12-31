package uika806.pico.fn;

import uika806.kernel.AFn;
import uika806.objects.EmptyList;

/**
 *
 * @author hemmi
 */
public class NullqFn extends AFn {

    @Override
    public String getName() {
        return "null?";
    }

    @Override
    public Object invoke(Object arg1) {

        if (arg1 instanceof EmptyList) {
            return Boolean.TRUE;
        }
        return Boolean.FALSE;

    }
    
}
