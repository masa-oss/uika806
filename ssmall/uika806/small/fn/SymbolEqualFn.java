package uika806.small.fn;

import uika806.kernel.AFn;
import uika806.objects.SSymbol;

/**
 *
 * @author hemmi
 */
public class SymbolEqualFn extends AFn {

    @Override
    public String getName() {
        return "symbol-equal";
    }


    @Override
    public Object invoke(Object arg1, Object arg2) {
        
        SSymbol s1 = (SSymbol) arg1;
        SSymbol s2 = (SSymbol) arg2;
        
        return eqSym(s1, s2);
    }
    
    @Override
    public Object invoke(Object arg1, Object arg2, Object arg3) {

        SSymbol s1 = (SSymbol) arg1;
        SSymbol s2 = (SSymbol) arg2;
        SSymbol s3 = (SSymbol) arg3;

        return eqSym(s1, s2) && eqSym(s2, s3)  ;
    }
    

    boolean eqSym(SSymbol s1, SSymbol s2) {
        
        return s1.equals(s2);
    }
}
