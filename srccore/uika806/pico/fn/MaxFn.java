package uika806.pico.fn;

import uika806.kernel.AFn;

/**
 *
 * @author hemmi
 */
public class MaxFn extends AFn {

    @Override
    public String getName() {
        return "max";
    }

    @Override
    public Object invoke(Object arg1, Object arg2, Object arg3) {
        return max(max(arg1, arg2), arg3);
    }

    @Override
    public Object invoke(Object arg1, Object arg2) {
        return max(arg1, arg2);
    }

    @Override
    public Object invoke(Object arg1) {
        return (arg1);
    }

    Object max(Object arg1, Object arg2) {
        
        if (arg1 instanceof Long) {
            if (arg2 instanceof Long) {
                return Math.max( (Long)arg1  ,(Long) arg2   );
            }
        }
        
        if (arg1 instanceof Number) {
            if (arg2 instanceof Number) {
        
                Number n1 = (Number) arg1;
                Number n2 = (Number) arg2;
                return Math.max(n1.doubleValue(), n2.doubleValue());
            }
        }
        return Boolean.FALSE;
    }

    
}
