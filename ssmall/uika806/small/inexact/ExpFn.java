package uika806.small.inexact;

import uika806.kernel.AFn;

/**
 *
 * @author hemmi
 */
public class ExpFn extends AFn {

    @Override
    public String getName() {
        return "exp";
    }

    @Override
    public Object invoke(Object arg1) {

        if (arg1 instanceof Number) {
            Number num = (Number) arg1;
            double d = num.doubleValue();
            return Math.exp(d);
        }
        throw new IllegalArgumentException("exp");
    }
}
