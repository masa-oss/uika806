/**
 * SPDX-License-Identifier: MPL-2.0
 * Author:  Masahito Hemmi.
 */
package uika806.string;

import com.ibm.icu.lang.UCharacter;
import java.util.ArrayList;
import uika806.err.BadArgumentInFunctionException;
import uika806.kernel.AFn;
import uika806.objects.SString;

/**
 *
 */
public class StringFoldcaseFn extends AFn {

    @Override
    public String getName() {
        return "string-foldcase";
    }

    @Override
    public Object invoke(Object arg1) {

        if (arg1 instanceof SString) {
            SString ss = (SString) arg1;

            String fold = UCharacter.foldCase(ss.toString(), UCharacter.FOLD_CASE_DEFAULT);

            SString newString = SString.fromString(fold);

            return newString;
        }
        throw new BadArgumentInFunctionException("string-foldcase");
    }

}
