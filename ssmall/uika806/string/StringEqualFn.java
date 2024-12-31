package uika806.string;

import uika806.objects.SChar;
import uika806.kernel.AFn;
import uika806.objects.SString;

/**
 *
 * @author hemmi
 */
public class StringEqualFn extends AFn {

    @Override
    public String getName() {
        return "string=";
    }
    
    @Override
    public Object invoke(Object arg1, Object arg2, Object arg3) {
        
        SString s1 = (SString) arg1;
        SString s2 = (SString) arg2;
        SString s3 = (SString) arg3;
        return eq(s1, s2) && eq(s2, s3);
    }
    
    @Override
    public Object invoke(Object arg1, Object arg2) {
        
        SString s1 = (SString) arg1;
        SString s2 = (SString) arg2;
        return eq(s1, s2);
    }
    
    
    boolean eq(SString s1, SString s2) {
        
        int len1 = s1.length();
        int len2 = s2.length();
        
        if (len1 != len2) {
            return Boolean.FALSE;
        }
        
        int len = len1;
        for (int i = 0; i < len; i++) {
            SChar ch1 = s1.getNthChar(i);
            SChar ch2 = s2.getNthChar(i);
            if (ch1.getCodepoint() != ch2.getCodepoint()) {
                return Boolean.FALSE;
            }
        }
        return Boolean.TRUE;
    }
}
