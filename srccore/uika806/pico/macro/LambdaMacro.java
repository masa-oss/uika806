/**
 * SPDX-License-Identifier: MPL-2.0
 * Author:  Masahito Hemmi.
 */
package uika806.pico.macro;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.List;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import uika806.kernel.AFn;
import uika806.err.LispException;
import uika806.kernel.RT;
import uika806.objects.Cell;
import uika806.objects.EmptyList;
import uika806.objects.SSymbol;
import uika806.pico.fn.AppendFn;
import uika806.port.CurrentPort;

/**
 *
 */
public class LambdaMacro extends AFn implements IMacro {

    private static final Logger LOG = LoggerFactory.getLogger(LambdaMacro.class);

    static LetRecStarMacro LETREC_M = new LetRecStarMacro();
    
    
    @Override
    public String getName() {
        return "LAMBDA";
    }

    @Override
    public Object invoke(Object form) {

        if (form instanceof Cell) {
            Cell cell = (Cell) form;
            Object vars = cell.getCar();
            Object body = cell.getCdr();
            if (body instanceof Cell) {
                Cell bodyCell = (Cell) body;

                LambdaResult res = checkDefine(bodyCell);

                LOG.info("41) res.size={}", res.defList.size());
                LOG.info("42) res.rest={}", CurrentPort.printString(res.rest));

                if (res.defList.size() > 0) {

                    // r7rs-small 5.3.2の内部定義が１つでも、定義されている時
                    ExpandResult eRes = arrayToCell(res.defList);

                    Object newBody = appendBody(eRes.getUnmodifiableList()  , res.rest );
                    
                  //  Object letrec = new Cell(SSymbol.LETREC, new Cell(eRes.var  , newBody));
                    Object letrec = LETREC_M.invoke(  new Cell(eRes.var, newBody)  );

                    Object expd = RT.list(SSymbol.INTERNAL_LAMBDA, vars, letrec);
                    
                    LOG.info("61) expd={}", CurrentPort.printString(expd));
                    
                    return expd;
                } else {
                    return new Cell(SSymbol.INTERNAL_LAMBDA, new Cell(vars, res.rest));
                }

            } else {
                return new Cell(SSymbol.INTERNAL_LAMBDA, new Cell(vars, new Cell(0L, EmptyList.NIL)));
            }
        }
        throw new LispException("Syntax error at LAMBDA");
    }
    
    Object appendBody(List<Object>  list   , Object rest) {
        return Cell.fromList(list, rest);
    }
    

    public static class ExpandResult {

        public Object var;
        ArrayList<Object> forms = new ArrayList<>();

        public void add(Object o) {

            forms.add(o);
            
        }

        public void addAll(Collection<Object> coll) {
            
            forms.addAll(coll);
        }
        
        
        public List<Object> getUnmodifiableList() {

            return Collections.unmodifiableList(forms);
        }
    }

    /**
     * <code>
     * INPUT: [(define a (+ 2 3))]
     * OUT: ((a (+ 2 3)))
     *
     * INPUT: [(define (foo x) (+ 2 x))]
     * OUT: ((foo (-λ (x) (+ 2 x))))
     *
     * INPUT:
     * OUT:
     *
     * </code>
     *
     * @param list
     * @return
     */
    ExpandResult arrayToCell(List<Object> list) {

        ExpandResult result = new ExpandResult();

        Object var = EmptyList.NIL;
        int len = list.size();
        for (int i = len - 1; i >= 0; i--) {

            Object sexp = list.get(i);
            Object car = ((Cell) sexp).getCar();

            int nType = getType(sexp);

            switch (nType) {
                case 0: { // sexp=(define (foo x) (+ 1 x))
                    Object cdr = ((Cell) sexp).getCdr();
                    var = new Cell(convDefine(cdr), var);

                }
                break;
                case 1: { // sexp=(define-values (x y)
                    Object cdr = ((Cell) sexp).getCdr();

                    ExpandResult res = convDefineValues(cdr);

                    var = appendFn.invoke(res.var, var);
                    res.addAll(  res.getUnmodifiableList()   );

                }
                break;
                case 2: {  // sexp=(define-syntax swap! (syntax-rules ))
                    Object cdr = ((Cell) sexp).getCdr();
                    var = new Cell(convDefineSyntax(cdr), var);

                }
                break;
                case 3: { // define-record-type
                    throw new LispException("Syntax error at define-record-type : ");

                }
                default:
                    throw new LispException("Syntax error at LAMBDA : " + nType);
            }

        }
        result.var = var;
        return result;
    }

    AppendFn appendFn = new AppendFn();

    ExpandResult convDefineValues(Object form) {

        // (define-values (a b) exp)
        if (form instanceof Cell) {
            Cell cell = (Cell) form;
            Object maybeVars = cell.getCar(); // (a b)
            Object cdr = cell.getCdr();

            if (maybeVars instanceof Cell && cdr instanceof Cell) {
                Cell cell2 = (Cell) cdr;
                Object exp = cell2.getCar(); // exp

                SSymbol temp = SSymbol.gensym();
                
                ExpandResult result = new ExpandResult();
                ArrayList<Object> varAndValues = new ArrayList<>();
                {
                    Object lamb = RT.list(SSymbol.INTERNAL_LAMBDA, EmptyList.NIL, exp   );
                    Object mv = RT.list( temp, RT.list( SSymbol.CALL_W_VALUES, lamb, SSymbol.LIST  )   );
                    
                    //   (g001    (call-with-values (lambda () expr) list))
                    
                    String debug = CurrentPort.printString(mv);
                    LOG.info("181) {}", debug);
                    
                    varAndValues.add(mv);
                }
                
                // (a b) を ((a #f) (b #f))  と  (set! a (nth g001 idx))    に変換
                int idx = 0;
                while (maybeVars instanceof Cell) {
                    Cell cell3 = (Cell) maybeVars;
                    Object car3 = cell3.getCar();
                    Object bind = RT.list(car3, Boolean.FALSE);
                    varAndValues.add(bind);

                    Object setq = RT.list(SSymbol.SETQ, car3, 
                                    RT.list(  SSymbol.NAME_SPC, temp, (long) idx  ));
                    result.add(exp);
                    idx++;
                }
                result.var = Cell.fromList(varAndValues);
                return result;
            }
            throw new LispException("Syntax error at define-values");
        }
        throw new LispException("Syntax error at define-values");
    }

    Object convDefine(Object form) {

        if (form instanceof Cell) {
            Cell cell = (Cell) form;
            Object maybeSym = cell.getCar();
            Object cdr = cell.getCdr();

            if (maybeSym instanceof SSymbol) {
                // (define a 123)
                return form;
            }
            if (maybeSym instanceof Cell) {
                // (define  (fun arg1 arg2 ...) ...
                Cell cell2 = (Cell) maybeSym;

                Object car2 = cell2.getCar(); //fun

                Object lambda = new Cell(SSymbol.INTERNAL_LAMBDA, new Cell(cell2.getCdr(), cdr));

                return RT.list(car2, lambda);
            }

            throw new LispException("Syntax error at define");

        }
        throw new LispException("Syntax error at define");
    }

    Object convDefineSyntax(Object form) {

        if (form instanceof Cell) {
            Cell cell = (Cell) form;
            Object maybeSym = cell.getCar();
            Object cdr = cell.getCdr();

            if (maybeSym instanceof SSymbol) {
                // (define a 123)
                return form;
            }
            throw new LispException("Syntax error at define-syntax");

        }
        throw new LispException("Syntax error at define-syntax");
    }

    static class LambdaResult {

        ArrayList<Object> defList = new ArrayList<>();

        Object rest;

    }

    LambdaResult checkDefine(Cell bodyCell) {

        LambdaResult res = new LambdaResult();

        Object list = bodyCell;
        while (list instanceof Cell) {
            Cell cell = (Cell) list;
            Object car = cell.getCar();

            if (isDefine(car)) {
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

            if (isDefine(car)) {
                throw new LispException("Syntax error at LAMBDA");
            }
            list = cell.getCdr();
        }
    }

    boolean isDefine(Object maybeList) {

        if (maybeList instanceof Cell) {
            Cell cell = (Cell) maybeList;
            Object car = cell.getCar();
            // r7rs-small  5.3.2 内部定義
            if (SSymbol.DEFINE.equals(car)) {
                return true;
            }
            // r7rs-small  5.3.3 複数の値の定義
            if (SSymbol.DEFINE_VALUES.equals(car)) {
                return true;
            }
            // r7rs-small  5.4 構文定義
            if (SSymbol.DEFINE_SYNTAX.equals(car)) {
                return true;
            }

            // r7rs-small  5.5 レコード型定義
            if (SSymbol.DEFINE_RECORD_TYPE.equals(car)) {
                return true;
            }

        }
        return false;
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
