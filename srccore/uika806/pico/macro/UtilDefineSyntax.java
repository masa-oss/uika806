/**
 * SPDX-License-Identifier: MPL-2.0
 * Author:  Masahito Hemmi.
 */
package uika806.pico.macro;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import uika806.objects.Cell;
import uika806.objects.SSymbol;
import uika806.port.CurrentPort;

/**
 * 内部 define-syntaxの構文をチェックする
 *
 * <code>
 *
 *     (define-syntax   myif
 *         (syntax-rules ()
 *             ((myif test exp1 exp2)
 *              (if  test exp1 exp2))
 *         )
 *     )
 *
 *
 * </code>
 */
public class UtilDefineSyntax {

    private static final Logger LOG = LoggerFactory.getLogger(UtilDefineSyntax.class);

    static class DefSyntaxResult {

        public final boolean ok;
        public final SSymbol name;
        public final Cell syntaxRules;

        DefSyntaxResult(boolean res, SSymbol sym, Cell rules) {
            this.ok = res;
            this.name = sym;
            this.syntaxRules = rules;
        }
    }

    DefSyntaxResult checkDefineSyntax(Object list) {

        if (list instanceof Cell) {
            Cell cell = (Cell) list;
            Object car = cell.getCar();
            Object cdr = cell.getCdr();

            if (SSymbol.DEFINE_SYNTAX.equals(car)) {
                if (cdr instanceof Cell) {
                    Cell cel1 = (Cell) cdr;
                    Object car1 = cel1.getCar();
                    Object cdr1 = cel1.getCdr();

                    if (car1 instanceof SSymbol) {
                        SSymbol sym = (SSymbol) car1;
                        if (cdr1 instanceof Cell) {
                            Cell cel2 = (Cell) cdr1;
                            Object car2 = cel2.getCar();
                            Object cdr2 = cel2.getCdr();

                            LOG.info("63) var={}", CurrentPort.printString(car2));
                            if (car2 instanceof Cell) {

                                return new DefSyntaxResult(false, sym, (Cell) car2);
                            }
                        }
                    }
                }
            }
        }
        // syntax error
        return new DefSyntaxResult(false, null, null);
    }

}
