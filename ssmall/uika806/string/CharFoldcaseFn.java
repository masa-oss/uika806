package uika806.string;

import com.ibm.icu.lang.UCharacter;
import uika806.objects.SChar;
import uika806.kernel.AFn;

/**
 *
 * @author hemmi
 */
public class CharFoldcaseFn extends AFn {

    @Override
    public String getName() {
        return "char-foldcase";
    }

    /**
     * 参考
     *  toLowerCaseの落とし穴とCase Foldingの話
     *  https://engineering.linecorp.com/ja/blog/tolowercase-pitfalls-and-case-folding
     * 
     * 
     * @param arg1
     * @return 
     */
    
    
    @Override
    public Object invoke(Object arg1) {
        

        SChar sc = (SChar) arg1;
        int code = sc.getCodepoint();

        StringBuilder sb = new StringBuilder();
        sb.appendCodePoint(code);

        String fold = UCharacter.foldCase(sb.toString(), UCharacter.FOLD_CASE_DEFAULT);

        int foldCode = fold.codePointAt(0);
        
        return SChar.valueOf(foldCode);

    }
/*
    Object old_invoke(Object arg1) {
        

        SChar sc = (SChar) arg1;
        int code = sc.getCodepoint();
        int m = Character.toLowerCase(code);
        if (m != code) {
            return SChar.valueOf(m);
        }
        return sc;

    }
*/

    
}
