package uika806.small.inexact;

import uika806.kernel.AFn;
import uika806.objects.Complex;

/**
 *
 * @author hemmi
 */
public class MagnitudeFn extends AFn {

    @Override
    public String getName() {
        return "magnitude";
    }

    @Override
    public Object invoke(Object arg1) {

        if (arg1 instanceof Complex) {
            Complex co = (Complex) arg1;

            return Math.sqrt(co.re() * co.re() + co.im() * co.im());
        }
        throw new IllegalArgumentException("magnitude");
    }
}
