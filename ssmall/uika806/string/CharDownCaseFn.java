package uika806.string;

import uika806.objects.SChar;
import uika806.kernel.AFn;

/**
 *
 * @author hemmi
 */
public class CharDownCaseFn extends AFn {

    @Override
    public String getName() {
        return "char-down-case";
    }

    @Override
    public Object invoke(Object o) {

        SChar ch = (SChar) o;
        int n=  Character.toLowerCase(ch.getCodepoint());
        return SChar.valueOf(n);
    }

}
