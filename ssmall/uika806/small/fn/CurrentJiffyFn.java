package uika806.small.fn;

import uika806.kernel.AFn;

/**
 *
 * @author hemmi
 */
public class CurrentJiffyFn extends AFn {

    @Override
    public String getName() {
        return "current-jiffy";
    }

    @Override
    public Object invoke() {
        
        long d = System.currentTimeMillis();
        return d;
    }
    
}
