package uika806.small.inexact;

import uika806.kernel.AFn;
import uika806.objects.Complex;

/**
 *
 * @author hemmi
 */
public class MakePolar extends AFn {

    @Override
    public String getName() {
        return "make-polar";
    }

    @Override
    public Object invoke(Object arg1, Object arg2) {

        double x3 = ((Number) arg1).doubleValue();
        double x4 = ((Number) arg2).doubleValue();
        
        double re = x3 * Math.cos(x4);
        double im = x3 * Math.sin(x4);
        return new Complex(re, im);
    }
}
