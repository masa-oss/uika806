package uika806.string;

import uika806.objects.SChar;
import uika806.kernel.AFn;
import uika806.objects.SString;
import uika806.small.fn.UtilString;

/**
 *
 * @author hemmi
 */
public class StringEqualCiFn extends AFn {

    @Override
    public String getName() {
        return "string-ci=";
    }
    
    @Override
    public Object invoke(Object arg1, Object arg2) {
        
        SString s1 = (SString) arg1;
        SString s2 = (SString) arg2;
        
        if (s1 == s2) {
            
            return Boolean.TRUE;
        }
        
        int len1 = s1.length();
        int len2 = s2.length();
        
        if (len1 != len2) {
            return Boolean.FALSE;
        }
        
        int len = len1;
        for (int i = 0; i < len; i++) {
            SChar ch1 = s1.getNthChar(i);
            SChar ch2 = s2.getNthChar(i);
            int cmp = UtilString.compareCodePointCI(ch1.getCodepoint(), ch2.getCodepoint());
            if (cmp != 0) {
                return Boolean.FALSE;
            }
        }
        return Boolean.TRUE;
    }

}
