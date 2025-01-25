package uika806.pico.macro;

import uika806.kernel.AFn;
import uika806.objects.Cell;
import uika806.kernel.RT;
import uika806.objects.SSymbol;

/**
 *  マクロではなく、関数として定義する
 *
 * <code>
 *
 *  <構文定義> −→ (define-uika-syntax <キーワード> <変換子仕様>)
 *
 *
 * <変換子仕様> −→ (syntax-uika-rules (<識別子>*) <構文規則>*) | (syntax-rules <識別子> (<識別子>*)
 * <構文規則>*)
 *
 *
 * (define-syntax unless (syntax-rules () ((unless test result1 result2 ...) (if
 * (not test) (begin result1 result2 ...)))))
 *
 * </code>
 *
 */
@Deprecated
public class DefineUikaSyntaxMacro extends AFn implements IMacro {

    @Override
    public String getName() {
        return "define-uika-syntax";
    }

    @Override
    public Object invoke(Object arg1) {

        if (arg1 instanceof Cell) {
            Cell cell = (Cell) arg1;
            Object car = cell.getCar();
            if (car instanceof SSymbol) {
                SSymbol sym = (SSymbol) car;
                Object cadr = RT.car(cell.getCdr());  // may be (syntax-rules)

                return RT.list(SSymbol.DEFINE4, RT.list(SSymbol.QUOTE, sym), cadr);

                
            // (define a (lambda () ...
//            Object qSymbol = RT.list(SSymbol.QUOTE, maybeSym);
  //          return new Cell(SSymbol.DEFINE4, new Cell(qSymbol, val));
                
                
            } else {
                throw new IllegalArgumentException(  getName() + ", 1st parameter must be a symbol");

            }
        }

        throw new IllegalArgumentException(  getName() );
    }

}
