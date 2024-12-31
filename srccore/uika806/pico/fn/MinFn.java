package uika806.pico.fn;

import uika806.kernel.AFn;

/**
 *
 * @author hemmi
 */
public class MinFn extends AFn {

    @Override
    public String getName() {
        return "min";
    }

    @Override
    public Object invoke(Object arg1, Object arg2, Object arg3) {
        return min(min(arg1, arg2), arg3);
    }

    @Override
    public Object invoke(Object arg1, Object arg2) {
        return min(arg1, arg2);
    }

    @Override
    public Object invoke(Object arg1) {
        return (arg1);
    }

    Object min(Object arg1, Object arg2) {
        
        if (arg1 instanceof Long) {
            if (arg2 instanceof Long) {
                return Math.min( (Long)arg1  ,(Long) arg2   );
            }
        }
        
        if (arg1 instanceof Number) {
            if (arg2 instanceof Number) {
        
                Number n1 = (Number) arg1;
                Number n2 = (Number) arg2;
                return Math.min(n1.doubleValue(), n2.doubleValue());
            }
        }
        return Boolean.FALSE;
    }

    
}
