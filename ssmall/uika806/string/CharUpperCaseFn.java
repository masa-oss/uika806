package uika806.string;

import uika806.objects.SChar;
import uika806.kernel.AFn;

/**
 *
 * @author hemmi
 */
public class CharUpperCaseFn extends AFn {

    @Override
    public String getName() {
        return "char-upper-case";
    }

    @Override
    public Object invoke(Object o) {

        SChar ch = (SChar) o;
        
        return Character.isUpperCase(ch.getCodepoint());
    }
}
