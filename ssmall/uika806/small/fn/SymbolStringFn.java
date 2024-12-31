package uika806.small.fn;

import uika806.kernel.AFn;
import uika806.objects.SSymbol;
import uika806.objects.SString;

/**
 *
 * @author hemmi
 */
public class SymbolStringFn extends AFn {

    @Override
    public String getName() {
        return "symbol->string";
    }

    @Override
    public Object invoke(Object arg1) {
        
        SSymbol sym = (SSymbol) arg1;
        
        String str = sym.getName();
        
        return SString.fromString(str);
    }
    
}
