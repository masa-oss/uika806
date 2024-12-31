package uika806.string;

import uika806.objects.SChar;
import uika806.kernel.AFn;

/**
 *
 * @author hemmi
 */
public class CharLowerCaseFn extends AFn {

    @Override
    public String getName() {
        return "char-lower-case";
    }

    @Override
    public Object invoke(Object o) {

        SChar ch = (SChar) o;
        
        return Character.isLowerCase(ch.getCodepoint());
    }

}
