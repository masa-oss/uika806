package uika806.small.fn;

import uika806.pico.fn.EqualFn;
import uika806.kernel.AFn;
import uika806.objects.Cell;
import uika806.kernel.RT;

/**
 *
 * @author hemmi
 */
public class MemberFn extends AFn {
    
    EqualFn eqv = new EqualFn();

    @Override
    public String getName() {
        return "member";
    }


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

    @Override
    public Object invoke(Object key, Object list, Object compare) {

        
        AFn afn = (AFn)compare;
        
        while (list instanceof Cell) {
            Object car = RT.car(list);
            Boolean b = (Boolean) afn.invoke(key, car);
            if (b) {
                return list;
            }
            list = RT.cdr(list);
        }

        return Boolean.FALSE;
    }
}
