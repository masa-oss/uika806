package uika806.string;

import uika806.objects.SChar;
import uika806.kernel.AFn;

/**
 *
 * @author hemmi
 */
public class CharWhiteSpaceFn extends AFn {

    @Override
    public String getName() {
        return "char-white-space";
    }

    @Override
    public Object invoke(Object o) {

        SChar ch = (SChar) o;
        
        return Character.isWhitespace(ch.getCodepoint());
    }

}
