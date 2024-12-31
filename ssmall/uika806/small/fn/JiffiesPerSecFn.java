package uika806.small.fn;

import uika806.kernel.AFn;

/**
 *
 * @author hemmi
 */
public class JiffiesPerSecFn extends AFn {

    @Override
    public String getName() {
        return "jiffies-per-sec";
    }

    @Override
    public Object invoke() {
        return 1000L;
    }
    
}
