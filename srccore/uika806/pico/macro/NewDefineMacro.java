package uika806.pico.macro;

import java.util.ArrayList;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import uika806.err.LispException;
import uika806.kernel.AFn;
import uika806.kernel.RT;
import uika806.objects.Cell;
import uika806.objects.SSymbol;
import uika806.port.CurrentPort;

/**
 *  このクラスは Compiler4.javaの時の物なので、将来的に廃止する
 *
 * <code>
 * (define (bazoo x)
 *     (define-syntax   myif
 *         (syntax-rules ()
 *             ((myif test exp1 exp2)
 *              (if  test exp1 exp2))
 *         )
 *     )
 *     (myif (>= x 0) 'plus 'minus ))
 *
 * ==>
 *
 * (macrodef myif  (syntax-rules ...)
 *    (define (bazoo x)
 *        (myif  (>= x  0 ) 'plus 'minus    ))
 *
 * </code>
 */
public class NewDefineMacro extends AFn implements IMacro {

    private static final Logger LOG = LoggerFactory.getLogger(NewDefineMacro.class);

    final UtilDefineSyntax checkLogic = new UtilDefineSyntax();
    
    
    @Override
    public String getName() {
      //  return "*define";
        return "NewDefineMacro";
    }
//    public Object invokeWithEnv(Object form, Environ env) {

    @Override
    public Object invoke(Object form) {

        if (form instanceof Cell) {
            Cell cell = (Cell) form;
            Object vars = cell.getCar();
            Object body = cell.getCdr();

            LOG.info("51) var={}", CurrentPort.printString(vars));
            LOG.info("52) body={}", CurrentPort.printString(body));
            if (body instanceof Cell) {
                Cell bodyCell = (Cell) body;
                InnerDefResult lr = checkDefineSyntax(bodyCell);

                LOG.info("57) deflist={}", lr.defList);
                if (lr.defList.size() > 0) {
                    UtilDefineSyntax.DefSyntaxResult dsr = checkLogic.checkDefineSyntax(lr.defList.get(0));
                    
                    Object def = syntaxNotExists(vars, lr.rest);
                    
               //     SyntaxRules rules = new SyntaxRules(dsr.syntaxRules.getCdr(), env);
                    
                 //   LOG.info("71) rules={}", rules);
                 //   Object res = RT.list( SSymbol.MACRO_DEF, dsr.name, rules, def);
                    Object res = RT.list( SSymbol.MACRO_DEF, dsr.name, dsr.syntaxRules, def);

                    LOG.info("74) res={}", CurrentPort.printString(res));
                    return res;
                }
                return syntaxNotExists(vars, lr.rest);

            } else {
                return syntaxNotExists(vars, body);
            }
        }
        throw new LispException("Syntax error at define");
    }
    
    /**
     * 内部 defineがない場合の、マクロ展開をする
     * 
     * 
     * @param maybeSym
     * @param val
     * @return 
     */
    Object syntaxNotExists(Object maybeSym, Object val) {

        if (maybeSym instanceof SSymbol) {
            // (define a (lambda () ...
            Object qSymbol = RT.list(SSymbol.QUOTE, maybeSym);
            return new Cell(SSymbol.DEFINE4, new Cell(qSymbol, val));

        }
        if (maybeSym instanceof Cell) {
            // (define  (fun arg1 arg2 ...) ...
            Cell cell2 = (Cell) maybeSym;
            Object qSymbol = RT.list(SSymbol.QUOTE, cell2.getCar());
            Object lambda = new Cell(SSymbol.INTERNAL_LAMBDA, new Cell(cell2.getCdr(), val));

            return RT.list(SSymbol.DEFINE4, qSymbol, lambda);

        }
        throw new LispException("Syntax error at define");
    }

    static class InnerDefResult {

        ArrayList<Object> defList = new ArrayList<>();
        Object rest;
    }

    InnerDefResult checkDefineSyntax(Cell bodyCell) {

        InnerDefResult res = new InnerDefResult();

        Object list = bodyCell;
        while (list instanceof Cell) {
            Cell cell = (Cell) list;
            Object car = cell.getCar();

            int ty = getType(car);
            if (ty == 2) {
                res.defList.add(car);
            } else {
                break;
            }
            list = cell.getCdr();
        }

        // のこったlist中に、defineがあったら、エラー
        restCheck(list);

        res.rest = list;
        return res;
    }

    void restCheck(Object bodyCell) {

        Object list = bodyCell;
        while (list instanceof Cell) {
            Cell cell = (Cell) list;
            Object car = cell.getCar();

            int ty = getType(car);
            if (ty == 2) {
                throw new LispException("Syntax error at define-syntax");
            }
            list = cell.getCdr();
        }
    }

    int getType(Object maybeList) {

        if (maybeList instanceof Cell) {
            Cell cell = (Cell) maybeList;
            Object car = cell.getCar();
            // r7rs-small  5.3.2 内部定義
            if (SSymbol.DEFINE.equals(car)) {
                return 0;
            }
            // r7rs-small  5.3.3 複数の値の定義
            if (SSymbol.DEFINE_VALUES.equals(car)) {
                return 1;
            }
            // r7rs-small  5.4 構文定義
            if (SSymbol.DEFINE_SYNTAX.equals(car)) {
                return 2;
            }

            // r7rs-small  5.5 レコード型定義
            if (SSymbol.DEFINE_RECORD_TYPE.equals(car)) {
                return 3;
            }

        }
        return -1;
    }

}
