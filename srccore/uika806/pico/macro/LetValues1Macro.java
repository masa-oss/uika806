package uika806.pico.macro;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import uika806.err.LispException;
import uika806.kernel.AFn;

import uika806.objects.EmptyList;
import uika806.kernel.RT;
import uika806.objects.Cell;
import uika806.port.CurrentPort;
import static uika806.objects.SSymbol.CALL_W_VALUES;
import static uika806.objects.SSymbol.INTERNAL_LAMBDA;

/**
 * <code>
 *
 *  (let-values  (((a b) (values 1 2 3)))
 *       (list a b))
 *
 *         ▼
 *
 *  (call-with-values (lambda () (values 1 2 3))
 *       (lambda (a b) (list a b)))
 *
 * </code>
 */
public class LetValues1Macro extends AFn implements IMacro {

    @Override
    public String getName() {
        return "let-values";
    }

    private static final Logger LOG = LoggerFactory.getLogger(LetValues1Macro.class);

    /*
    *<code>
    *(let-values  (((a b) (values 1 2 3)))
    *    (list a b))
    *</code>
     */
    @Override
    public Object invoke(Object form) {

        if (form instanceof Cell) {
            Cell cell = (Cell) form;
            Object vars = cell.getCar();
            Object bodys = cell.getCdr();

            if (RT.isTail(RT.cdr(vars))) {

                Object firstVar = RT.caar(form);  //((a b) (values 1 2 3))

                LOG.info("var1={}", CurrentPort.printString(firstVar));
                LOG.info("bodys={}", CurrentPort.printString(bodys));

                Object param = RT.car(firstVar);
                Object init = RT.cadr(firstVar);

                Object lambda1 = RT.list(INTERNAL_LAMBDA, EmptyList.NIL, init);
                Object lambda2 = new Cell(INTERNAL_LAMBDA, new Cell(param, bodys));

                LOG.info("lambda1={}", CurrentPort.printString(lambda1));
                LOG.info("lambda2={}", CurrentPort.printString(lambda2));

                return RT.list(CALL_W_VALUES, lambda1, lambda2);

            } else {
                throw new LispException("Unsuported error let-values");
            }
        }
        throw new LispException("syntax error let-values");
    }

}
