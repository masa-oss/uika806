/**
 * SPDX-License-Identifier: MPL-2.0
 * Author:  Masahito Hemmi.
 */
package uika806.string;

import java.util.ArrayList;
import uika806.err.BadArgumentInFunctionException;
import uika806.kernel.AFn;
import uika806.objects.SString;

/**
 *
 */
public class StringDowncaseFn extends AFn {

    @Override
    public String getName() {
        return "string-downcase";
    }

    @Override
    public Object invoke(Object arg1) {

        if (arg1 instanceof SString) {
            SString ss = (SString) arg1;
            int len = ss.length();
            ArrayList<Integer> list = new ArrayList<>();
            for (int i = 0; i < len; i++) {
                int codePoint = ss.getNth(i);
                
                list.add(Character.toLowerCase(codePoint));
                
            }
            return SString.fromList(list);
        }
        throw new BadArgumentInFunctionException("string-downcase");
    }
    
}
