package uika806.pico.macro;

import uika806.kernel.AFn;
import uika806.objects.Cell;
import uika806.kernel.RT;
import uika806.objects.SSymbol;

/**
 * 
 * Move uika806.small.macro to xx.pico.macro
 * 
 * <code>
 * 
 *  <構文定義> −→
(define-syntax <キーワード> <変換子仕様>)
 * 
 * 
 * <変換子仕様> −→
(syntax-rules (<識別子>*) <構文規則>*) | (syntax-rules <識別子> (<識別子>*)
<構文規則>*)
 * 
 * 
 * (define-syntax unless
 * (syntax-rules ()
 * ((unless test result1 result2 ...)
 * (if (not test)
 * (begin result1 result2 ...)))))
 *
 * </code>
 *
 * @author hemmi
 */
public class DefineSyntaxMacro extends AFn implements IMacro {

    @Override
    public String getName() {
        return "define-syntax";
    }

    @Override
    public Object invoke(Object arg1) {

        if (arg1 instanceof Cell) {
            Cell cell = (Cell) arg1;
            Object car = cell.getCar();
            if (car instanceof SSymbol) {
                SSymbol sym = (SSymbol) car;
                Object cadr = RT.car( cell.getCdr());  // may be (syntax-rules)
                
                return RT.list( SSymbol.SDEF_MACRO,  RT.list( SSymbol.QUOTE, sym ), cadr );
                
            } else {
                throw new IllegalArgumentException("define-syntax, 1st parameter must be a symbol");
                
            }
        }
        
        throw new IllegalArgumentException("define-syntax");
    }

}
