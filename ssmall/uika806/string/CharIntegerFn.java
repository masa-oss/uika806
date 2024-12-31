package uika806.string;

import uika806.objects.SChar;
import uika806.kernel.AFn;

/**
 *
 * @author hemmi
 */
public class CharIntegerFn extends AFn {

    @Override
    public String getName() {
        return "char->integer";
    }

    @Override
    public Object invoke(Object arg1) {

        SChar sc = (SChar) arg1;
        long ln = sc.getCodepoint();
        return ln;
    }
    
}
