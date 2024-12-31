package uika806.small.macro;

import uika806.pico.macro.IMacro;
import uika806.kernel.AFn;
import uika806.kernel.RT;
import uika806.objects.SSymbol;

/**
 * (import (scheme cxr) (scheme inexact))
 *     ▼
 * (*import '((scheme cxr) (scheme inexact)))
 * 
 * 
 */
public class ImportMacro extends AFn implements IMacro {

    @Override
    public String getName() {
        return "*import";
    }

    @Override
    public Object invoke(Object arg1) {
        return  RT.list(SSymbol.SIMPORT, RT.list( SSymbol.QUOTE , arg1));
    }
}
