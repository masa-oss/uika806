package uika806.small.fn;

import uika806.pico.fn.EqualFn;
import uika806.kernel.AFn;
import uika806.objects.Cell;
import uika806.kernel.RT;

/**
 *
 * @author hemmi
 */
public class AssocFn extends AFn {

    EqualFn eqv = new EqualFn();
    
    @Override
    public String getName() {
        return "assoc";
    }
    

    @Override
    public Object invoke(Object key, Object list) {
        
        if (!(list instanceof Cell)) {
            return Boolean.FALSE;
        } else {
            Object car = RT.car(list);
            Boolean b = (Boolean) eqv.invoke(RT.car(car), key);
            if (b) {
                return car;
            } else {
                return invoke(key, RT.cdr(list));
            }
        }
    }
    
    @Override
    public Object invoke(Object key, Object list, Object compare) {
        
        AFn afn = (AFn) compare;
        if (!(list instanceof Cell)) {
            return Boolean.FALSE;
        } else {
            Object car = RT.car(list);
            Boolean b = (Boolean) afn.invoke(RT.car(car), key);
            if (b) {
                return car;
            } else {
                return invoke(key, RT.cdr(list));
            }
        }
    }
}
