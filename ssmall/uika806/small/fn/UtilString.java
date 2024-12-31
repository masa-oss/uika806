package uika806.small.fn;

import uika806.objects.SString;

/**
 *
 * @author hemmi
 */
public class UtilString {


    public static int compare(SString s1, SString s2) {
        
        int len1 = s1.length();
        int len2 = s2.length();
        int len = Math.min(len1, len2);
        for (int i = 0; i < len; i++) {
            int ch1 = s1.getNth(i);
            int ch2 = s2.getNth(i);
            if (ch1 !=  ch2) {
                return ch1 - ch2;
            }
        }
        
        return len1 - len2;
    }

    public static int compareCI(SString s1, SString s2) {
    
        int len1 = s1.length();
        int len2 = s2.length();
        int len = Math.min(len1, len2);
        for (int i = 0; i < len; i++) {
            int ch1 = s1.getNth(i);
            int ch2 = s2.getNth(i);
            int result = compareCodePointCI(ch1, ch2);
            if (result !=  0) {
                return result;
            }
        }
        
        return len1 - len2;
    }
    
    
    // Case insensitive comparison of two code points
    public static int compareCodePointCI(int cp1, int cp2) {
        
        cp1 = Character.toUpperCase(cp1);
        cp2 = Character.toUpperCase(cp2);
        if (cp1 != cp2) {
            cp1 = Character.toLowerCase(cp1);
            cp2 = Character.toLowerCase(cp2);
            if (cp1 != cp2) {
                return cp1 - cp2;
            }
        }
        return 0;
    }
    
}
