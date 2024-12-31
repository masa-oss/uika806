package uika806.string;

import uika806.kernel.AFn;
import uika806.objects.SString;

/**
 *
 * @author hemmi
 */
public class StringLengthFn extends AFn {

    @Override
    public String getName() {
        return "string-length";
    }

    @Override
    public Object invoke(Object arg1) {

        SString str = (SString) arg1;
        long ln = str.length();
        return ln;
    }
}
