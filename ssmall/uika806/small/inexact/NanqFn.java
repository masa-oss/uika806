package uika806.small.inexact;

import uika806.kernel.AFn;
import uika806.objects.Complex;

/**
 *
 * @author hemmi
 */
public class NanqFn extends AFn {

    @Override
    public String getName() {
        return "nan?";
    }

    @Override
    public Object invoke(Object arg1) {
        
        if (arg1 instanceof Double) {
            Double db = (Double) arg1;
            
            return (Double.isNaN(db));
        }
        if (arg1 instanceof Complex) {
            Complex comp = (Complex) arg1;
            
            return Double.isNaN(comp.re()) ||
                    Double.isNaN(comp.im());
        }
        return Boolean.FALSE;
    }
    
}
