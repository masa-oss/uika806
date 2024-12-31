package uika806.small.fn;

import uika806.kernel.AFn;
import uika806.err.RaiseException;
import uika806.kernel.RT;

/**
 *
 * @author hemmi
 */
public class RaiseContinuableFn extends AFn {

    @Override
    public String getName() {
        return "raise-continuable";
    }

    @Override
    public Object invoke(Object arg1) {
        
        throw new RaiseException("raise-continuable", RT.list(arg1), true);
    }
    
    @Override
    public Object invoke(Object arg1, Object arg2) {
        
        throw new RaiseException("raise-continuable", RT.list(arg1, arg2), true);
    }
}
