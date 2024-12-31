package uika806.string;

import uika806.objects.SChar;
import uika806.kernel.AFn;

/**
 *
 * @author hemmi
 */
public class CharUpCaseFn extends AFn {

    @Override
    public String getName() {
        return "char-up-case";
    }

    @Override
    public Object invoke(Object o) {

        SChar ch = (SChar) o;
        int n = Character.toUpperCase(ch.getCodepoint());
        return SChar.valueOf(n);
    }

}
