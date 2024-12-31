/**
 * SPDX-License-Identifier: MPL-2.0
 * Author:  Masahito Hemmi.
 */
package uika806.pico.macro;

import java.util.ArrayList;
import java.util.List;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import uika806.kernel.AFn;
import uika806.kernel.RT;
import uika806.objects.SSymbol;
import uika806.objects.Cell;
import uika806.port.CurrentPort;

/**
 * 今のところ letrec*と同じ
 * 
 * <code>
 * 参考: Gaucheのドキュメント
 * 
 * https://practical-scheme.net/gauche/man/gauche-refj/Bian-Shu-Shu-Fu-.html
 * 
 * letrec ((var expr) …) body …
 * 各exprを(順不同で)評価します。
 *    → 関数の引数と同じで評価する順序は決まっていない（左から右に評価しても良いし、マルチスレッドで、並列に処理しても良い）
 * 
 * letrecを使うなら評価順に依存しないように、プログラマが気をつけないといけません。
 * </code>
 */
public class LetRecMacro extends AFn implements IMacro {

    private static final Logger LOG = LoggerFactory.getLogger(LetRecMacro.class);

    public static Object[] splitBind2(Object x) {

        List<Object> var = new ArrayList<>();
        List<Object> val = new ArrayList<>();

        Object binds = x;
        while (!(RT.isNull(binds))) {

            Object car = RT.car(binds);
            var.add(RT.car(car));
            val.add(RT.cadr(car));
            binds = RT.cdr(binds);
        }
        Object var2 = arrayToCell(var);
        Object val2 = arrayToCell(val);

        return new Object[]{var2, val2};
    }

    public static Object arrayToCell(List<Object> list) {

        Object var = RT.EOL;
        int len = list.size();
        for (int i = len - 1; i >= 0; i--) {
            var = new Cell(list.get(i), var);
        }
        return var;
    }

    @Override
    public String getName() {
        return "letrec";
    }

    @Override
    public Object invoke(Object body) {

        Object binds = RT.car(body);

        LOG.info("63) body={}", CurrentPort.printString(body));
        LOG.info("64) binds={}", CurrentPort.printString(binds));
        
        Object[] pair = splitBind2(binds);

        LOG.info("68) [0]={}", CurrentPort.printString(pair[0]));
        LOG.info("69) [1]={}", CurrentPort.printString(pair[1]));

        Object undefs = sameLengthUndef(pair[1]);

        Object body2 = RT.cdr(body);

        Object lambda = addSets(pair[0], pair[1], body2);

        lambda = new Cell(SSymbol.LAMBDA, new Cell(pair[0], lambda));
        LOG.info("60) λ={}", CurrentPort.printString(lambda));

        Object result = new Cell(lambda, undefs);
        LOG.info("63) result={}", CurrentPort.printString(result));

        return result;
    }

    Object addSets(Object vars, Object lambdas, Object body) {

        if (!(vars instanceof Cell)) {
            return body;
        }

        return new Cell(RT.list(SSymbol.SETQ, RT.car(vars), RT.car(lambdas)),
                addSets(RT.cdr(vars), RT.cdr(lambdas), body)
        );
    }

    Object sameLengthUndef(Object list) {

        if (!(list instanceof Cell)) {
            return RT.EOL;
        }

        return new Cell(Boolean.FALSE, sameLengthUndef(RT.cdr(list)));

    }

}
