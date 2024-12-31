package uika806.small.fn;

import uika806.kernel.AFn;

/**
 *
 * @author hemmi
 */
public class GtFn extends AFn {

    @Override
    public String getName() {
        return ">";
    }

    @Override
    public Object invoke(Object arg1, Object arg2, Object arg3) {
        return gt (arg1, arg2) && gt(arg2, arg3);
    }

    @Override
    public Object invoke(Object arg1, Object arg2) {
        return gt(arg1, arg2);
    }
    
    boolean gt(Object arg1, Object arg2) {
        
        if (arg1 instanceof Number ) {
            if (arg2 instanceof Number) {
                Number n1 = (Number) arg1;
                Number n2 = (Number) arg2;
                
                return n1.doubleValue() > n2.doubleValue();
            }
        }
        return false;
    }


}
