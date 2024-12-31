package uika806.string;

import uika806.objects.SChar;
import uika806.kernel.AFn;

/**
 *
 * @author hemmi
 */
public class CharNumericFn extends AFn {

    @Override
    public String getName() {
        return "char->numeric";
    }

    @Override
    public Object invoke(Object o) {

        SChar ch = (SChar) o;
        
        return Character.isDigit(ch.getCodepoint());
    }
}
