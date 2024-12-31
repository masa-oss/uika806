/**
 * SPDX-License-Identifier: MPL-2.0
 * Author:  Masahito Hemmi.
 */
package uika806.small.fn;

import uika806.kernel.AFn;
import uika806.kernel.RT;
import uika806.objects.SSymbol;

public class UnquoteFn extends AFn {

    
    @Override
    public String getName() {
        return "unquote";
    }

    @Override
    public Object invoke(Object arg1) {
        
        return RT.list( SSymbol.UNQUOTE,   arg1);
    }
}
