package uika806.pico.fn;

import uika806.kernel.AFn;
import uika806.objects.Complex;
import uika806.objects.Ratio;

/**
 *
 * @author hemmi
 */
public class ZeroqFn extends AFn {

    @Override
    public String getName() {
        return "zero?";
    }

    @Override
    public Object invoke(Object arg1) {

        if (arg1 instanceof Long) {
            Long ln = (Long) arg1;
            return ln == 0L;
        } else if (arg1 instanceof Double) {
            Double dbl = (Double) arg1;
            return dbl == 0.0;
        } else if (arg1 instanceof Ratio) {
            Ratio ra = (Ratio) arg1;
            return ra.getNumerator().longValueExact() == 0;
            
        } else if (arg1 instanceof Complex) {
            Complex co = (Complex) arg1;
            return co.re() == 0.0 && co.im() == 0.0;
            
        }
        return Boolean.FALSE;
    }
    
}
