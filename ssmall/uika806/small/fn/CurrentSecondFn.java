package uika806.small.fn;

import uika806.kernel.AFn;

/**
 *
 * @author hemmi
 */
public class CurrentSecondFn extends AFn {

    @Override
    public String getName() {
        return "current-second";
    }

    @Override
    public Object invoke() {
        
        double d = System.currentTimeMillis();
        d = d / 1000.0;
        return d;
    }
    
}
