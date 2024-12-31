package uika806.small.inexact;

import uika806.kernel.AFn;
import uika806.objects.Complex;

/**
 *
 * @author hemmi
 */
public class SqrtFn extends AFn {

    @Override
    public String getName() {
        return "sqrt";
    }

    @Override
    public Object invoke(Object arg1) {

        if (arg1 instanceof Number) {
            Number num = (Number) arg1;
            
            double d = num.doubleValue();
            if (d < 0) {
                double x = Math.sqrt(-d);
                
                return new Complex(0.0, x);
            } else {
                return Math.sqrt(d);
            }
        }
        throw new IllegalArgumentException("sqrt");
    }
}
