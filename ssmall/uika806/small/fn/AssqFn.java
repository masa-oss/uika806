package uika806.small.fn;

import uika806.kernel.AFn;
import uika806.objects.Cell;
import uika806.kernel.RT;

/**
 *
 * @author hemmi
 */
public class AssqFn extends AFn {

    EqqFn func = new EqqFn();
    
    @Override
    public String getName() {
        return "assq";
    }
    

    @Override
    public Object invoke(Object key, Object list) {
        
        if (!(list instanceof Cell)) {
            return Boolean.FALSE;
        } else {
            Object car = RT.car(list);
            Boolean b = (Boolean) func.invoke(RT.car(car) , key);
            if (b) {
                return car;
            } else {
                return invoke(key, RT.cdr(list));
            }
        }
        
    }
    
}
