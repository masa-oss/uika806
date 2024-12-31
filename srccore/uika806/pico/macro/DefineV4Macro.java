package uika806.pico.macro;

import uika806.kernel.AFn;
import uika806.err.LispException;
import uika806.kernel.RT;
import uika806.objects.Cell;
import uika806.objects.SSymbol;

/**
 *
 * This macro is used in VM.java .
 */
public class DefineV4Macro extends AFn implements IMacro {

    @Override
    public String getName() {
        return "define";
    }

    @Override
    public Object invoke(Object form) {

        if (form instanceof Cell) {
            Cell cell = (Cell) form;
            Object maybeSym = cell.getCar();
            Object val = cell.getCdr();
            if (maybeSym instanceof SSymbol) {
                // (define a (lambda () ...
                Object qSymbol = RT.list(SSymbol.QUOTE, maybeSym);
                return new Cell(SSymbol.DEFINE4, new Cell(qSymbol, val));

            }
            if (maybeSym instanceof Cell) {
                // (define  (fun arg1 arg2 ...) ...
                Cell cell2 = (Cell) maybeSym;
                Object qSymbol = RT.list(SSymbol.QUOTE, cell2.getCar());
                Object lambda = new Cell(SSymbol.LAMBDA, new Cell(cell2.getCdr(), val));
                
                return RT.list(SSymbol.DEFINE4, qSymbol, lambda);

            }
            throw new LispException("Syntax error at define");
        }
        throw new LispException("Syntax error at define");
    }

}
