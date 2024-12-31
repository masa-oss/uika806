package uika806.small.inexact;

import uika806.kernel.AFn;
import uika806.objects.Complex;

/**
 *
 * @author hemmi
 */
public class MakeRectanglerFn extends AFn {

    @Override
    public String getName() {
        return "make-rectangler";
    }

    @Override
    public Object invoke(Object arg1, Object arg2) {
        
        double re = ((Number) arg1).doubleValue();
        double im = ((Number) arg2).doubleValue();
        
        return new Complex(re, im);
    }
}
