package uika806.small.fn;

import uika806.kernel.AFn;
import uika806.objects.SSymbol;

/**
 *
 * @author hemmi
 */
public class EqqFn extends AFn {

    @Override
    public String getName() {
        return "eq?";
    }

    @Override
    public Object invoke(Object arg1, Object arg2) {

        if (arg1 == arg2) {
            return true;
        }
        if ((arg1 instanceof SSymbol) && (arg2 instanceof SSymbol)) {
            SSymbol s1 = (SSymbol) arg1;
            SSymbol s2 = (SSymbol) arg2;
            return s1.equals(s2);
        }

        return false;
    }
    
}
