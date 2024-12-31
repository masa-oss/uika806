package uika806.small.fn;

import uika806.kernel.AFn;
import uika806.kernel.RT;
import uika806.objects.Ratio;

/**
 * <pre>
 * gosh$ (exact 2.0)
 * 2
 * gosh$ (exact 1.5)
 * 3/2
 * gosh$ (exact 2.0)
 * 2
 * gosh$ (exact 1.4)
 * 7/5
 * </pre>
 *
 * @author hemmi
 */
public class ExactFn extends AFn {

    @Override
    public String getName() {
        return "exact";
    }

    @Override
    public Object invoke(Object arg1) {

        if (arg1 instanceof Long) {
            return arg1;
        } else if (arg1 instanceof Ratio) {
            return arg1;
        } else if (arg1 instanceof Double) {

            String s = String.format("%f", arg1);
            Ratio ra = Ratio.parseRatio(s);
            return ra.getIrreducible();
        }

        throw new IllegalArgumentException("exact " + arg1);
    }

}
