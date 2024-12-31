/**
 * SPDX-License-Identifier: MPL-2.0
 * Author:  Masahito Hemmi.
 */
package uika806.test;

import uika806.kernel.AFn;
import uika806.objects.Cell;
import uika806.kernel.RT;
import uika806.pico.macro.IMacro;
import uika806.objects.SSymbol;

/**
 * これは、 r7rsの仕様ではないが、テストで使いたいため、ここに入れる
 */
public class TestValuesMacro extends AFn implements IMacro {

    @Override
    public String getName() {
        return "test-values";
    }

    @Override
    public Object invoke(Object arg1) {

        Object car = RT.car(arg1);
        Object body = RT.cdr(arg1);
        
        Object v1 = RT.cadr(car);
        Object v2 = RT.caddr(car);
        
        Object sym1 = SSymbol.gensym();
        Object sym2 = SSymbol.gensym();

        Object list1 = new Cell( RT.list(sym1, sym2),  body    );
        Object list2 = RT.list(list1);
        
        Object t1 = RT.list(SSymbol.EQV, sym1, v1);
        Object t2 = RT.list(SSymbol.EQV, sym2, v2);
        Object an1 = RT.list(SSymbol.AND, t1, t2);
        
        Object reportError = RT.list(SSymbol.REPORT_TEST,  RT.list(SSymbol.LIST,  sym1, sym2 ));
        
        Object if1 = RT.list(SSymbol.IF, an1, Boolean.TRUE, reportError);
        
        
        return RT.list(SSymbol.LET_VALUES, list2, if1);
    }
    
}
