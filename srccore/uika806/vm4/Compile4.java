/**
 * SPDX-License-Identifier: MPL-2.0
 * Author:  Masahito Hemmi.
 */
package uika806.vm4;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import uika806.err.LispException;
import uika806.kernel.AFn;
import uika806.kernel.RT;
import uika806.kernel.VMLogger;
import uika806.pico.macro.IMacro;
import uika806.syntax.SyntaxRules;
import uika806.syntax.Environ;
import uika806.objects.EmptyList;
import uika806.objects.Cell;
import uika806.objects.SSymbol;
import uika806.objects.ScmUniqueSymbol;
import uika806.port.CurrentPort;

/**
 *
 */
public class Compile4 {

    private static final Logger LOG = LoggerFactory.getLogger(Compile4.class);

    Environ environ;
    VMLogger logger;

    public Compile4(VMLogger log, Environ newMap) {

        this.logger = log;
        this.environ = newMap;
    }

    public Op invoke(Object x, Op next) {

        if (logger != null) {
            LOG.warn("41) compile-x    = {}", logger.printString(x));
            LOG.warn("43) compile-next = {}", next.debugString());
        }
        Op comp = compile(x, next);
        if (logger != null) {
            LOG.warn("48) compile-result = {}", comp.debugString());
        }
        return comp;
    }

    public Op compile(Object x, Op next) {

        if (x instanceof SSymbol) {

            return Op.mkREFER((SSymbol) x, next);

        } else if (x instanceof Cell) {

            Cell seq = (Cell) x;
            Object first = seq.getCar();

            boolean isMacro = false;

            Object maybeMacro = null;
            if (first instanceof SSymbol) {

                SSymbol sym = (SSymbol) first;
                if (sym instanceof ScmUniqueSymbol) {  // FIX
                    
                    sym = ((ScmUniqueSymbol) sym).getOrigin();
                    first = sym;
                }
                
                maybeMacro = environ.get(sym);

                if (maybeMacro instanceof IMacro) {
                    isMacro = true;
                } else if (maybeMacro instanceof SyntaxRules) {
                    
                    throw new LispException("TODO マクロ展開");    //********************
                }
            }

            if (SSymbol.QUOTE.equals(first)) {

                Object arr0 = RT.cadr(seq);

                return Op.mkCONSTANT(arr0, next);

            } else if (isMacro) {
                // マクロ展開
                Object rest = seq.getCdr();

                if (maybeMacro instanceof AFn) {
                    AFn afn = (AFn) maybeMacro;
                    // 展開関数を呼ぶ
                    Object expa = afn.invokeWithEnv(rest, environ);

                    return invoke(expa, next);
                }
                if (maybeMacro instanceof SyntaxRules) {

                    SyntaxRules rules = (SyntaxRules) maybeMacro;
                    Object obj = null;
                    try {
                        obj = rules.transform(rest, environ);

                    } catch (RuntimeException re) {
                        LOG.error("SyntaxRules-error", re);
                    }
                    
                    if (logger != null) {
                        LOG.info("94)      macro-expanded = {}", logger.printString(obj));
                    }

                    return invoke(obj, next);

                }
                throw new IllegalStateException("can not process macro " + maybeMacro.getClass().getName());

            } else if (SSymbol.LAMBDA.equals(first)) {

                Object var = RT.cadr(seq);
                Object arr1 = RT.cddr(seq);

                Op compiled = null;
                if (true) {
                    // bodyが複数対応
                  //  compiled = newCompBody(arr1, Op.RET);
                  //  compiled = newCompBody(arr1, Op.mkRET());
                    
                    compiled = compBodys(arr1 , Op.mkRET());  // 2024-11-12
                    
                } else {
                    // bodyが１個
                   // compiled = invoke( RT.car(arr1)  , Op.RET );
                    compiled = invoke( RT.car(arr1)  , Op.mkRET() );
                }
                return Op.mkCLOSE(var, compiled, next);

            } else if (SSymbol.G_LAMBDA.equals(first)) {

                Object var = RT.cadr(seq);
                Object arr1 = RT.cddr(seq);

                    // bodyが１個
                Op compiled = invoke( RT.car(arr1)  , Op.mkRET() );
                return Op.mkCLOSE(var, compiled, next);

            } else if (SSymbol.IF.equals(first)) {

                Object arr0 = RT.cadr(seq);
                Object arr1 = RT.caddr(seq);
                Object arr3 = RT.cdddr(seq);
                Object elseClause = (arr3 instanceof Cell) ? RT.car(arr3) : RT.EOL;

                Op thenc = invoke(arr1, next);
                Op elsec = invoke(elseClause, next);
                return invoke(arr0, Op.mkTEST(thenc, elsec));

            } else if (SSymbol.SETQ.equals(first)) {

                Object arr0 = RT.cadr(seq);
                SSymbol sym = (SSymbol) arr0;
                Object arr1 = RT.caddr(seq);

                return invoke(arr1, Op.mkASSIGN(sym, next));

            } else if (SSymbol.SDEFINE.equals(first)) {
                // ここは *defineなので、 Compiler3 & VM3用

                Object arr0 = RT.cadr(seq);
                Object arr1 = null;
                Object cddr = RT.cddr(seq);
                if (cddr == RT.EOL) {
                    // 第二引数は省略された
                    arr1 = 0;    // (Pending)   Undefined?
                } else {
                    // 第二引数あり
                    arr1 = RT.car(cddr);
                }
                boolean bTail = this.isHalt(next);
                //   LOG.info("===================== *define :  isHalt={}", bTail);
                if (bTail) {
                    return invoke(arr1, Op.mkDEF((SSymbol) arr0, next));
                } else {
                    // 内部にdefineが書かれた時
                    //return invoke(arr1, Op.mkDEF((SSymbol) arr0, next));
                    throw new LispException("");
                }

            } else if (SSymbol.CALL_CC.equals(first) || SSymbol.CALL_CC2.equals(first)) {

                Object xx = RT.cadr(seq);

                Op c = Op.mkCONTI(
                        Op.mkARGUMENT(
                                invoke(xx, Op.APPLY))
                );

                boolean bn = isReturnNext(next);
                if (this.logger != null) {
                    LOG.info("181) {} {}", bn , logger.printString(next));
                }
                
                if (bn) {
                    return c;
                } else {
                    return Op.mkFRAME(next, c);
                }

            } else if (SSymbol.WITH_EXCEPTION.equals(first)) {

                Object handler = RT.cadr(seq);
                Object thunk = RT.caddr(seq);

                Op c = invoke(thunk, Op.APPLY);
                Op compThunk = (isReturnNext(next)) ? c : Op.mkFRAME(next, c);

                Op registHandler = Op.mkEXCEP_HN(compThunk);
                return invoke(handler, registHandler);

            } else if (SSymbol.sApply.equals(first)) {

                Object fun = RT.cadr(seq);
                Object args = RT.caddr(seq);
                if (logger != null) {
                    LOG.info("204) fun = {}, args={}    next={}",
                            logger.printString(fun),
                            logger.printString(args),
                            logger.printString(next));
                }

                Op temp = Op.mkPUSH(invoke(fun, Op.APPLY));
                Op temp2 = invoke(args, temp);
                Op c = Op.mkFRAME(next, temp2);
                return c;
                /*
            } else if (SSymbol.SLET_VAL.equals(first)) {

                // １引数のみ対応。 (let-values の試作  (*let-val ([(a b) (values ... )])  body ...)
                Object car = RT.car(RT.cadr(x));
                Object body = RT.cddr(x);
                Object vars = RT.car(car);  // (a b)
                Object exp = RT.cadr(car);  // (values ...)

                if (logger != null) {
                    LOG.info("211) car  = {}", logger.printString(car));
                    LOG.info("211) body = {}", logger.printString(body));

                    LOG.info("211) vars = {}", logger.printString(vars));
                    LOG.info("211) exp = {}", logger.printString(exp));
                }
                Op compiled = newCompBody(body, next);
                //   Op exp1 = Op.mkKEEPVALS( vars, Op.mkBINDVALS( compiled));

                throw new RuntimeException("SLET_VAL");
                */
            } else if (SSymbol.CALL_W_VALUES.equals(first)) {

                Object body = RT.cdr(x);
                Object firstLambda = RT.car(body);
                Object secondLambdaOrSymbol = RT.cadr(body);

                if (logger != null) {
                    LOG.info("266) {}",  logger.printString(secondLambdaOrSymbol));
                }

                Op second = this.invoke(secondLambdaOrSymbol, Op.APPLY);

                Op comp1st = this.invoke(firstLambda, Op.APPLY);

                Op keep = Op.mkFRAME(next,
                        Op.mkVALS_LIST(second));

                Op result = Op.mkFRAME(keep, comp1st);
                return result;

            } else if (SSymbol.SBEGIN.equals(first)) {
                // 実験 (*begin body1 body2)
                Object body = RT.cdr(x);
                Op ret = newCompBody(body, next);
                return ret;

            } else if (SSymbol.MACRO_DEF.equals(first)) {
                // defineの中に define-syntaxを書けるようにする実験。
                // (macrodef myif (syntax-rules ...) (define ...) )
                // 現在の環境に myifを定義して、 (define ...)をコンパイルする

                Object body = RT.cdr(x);
                Object name = RT.car(body);
                Cell syntaxRule = (Cell)RT.cadr(body);
                Object define = RT.caddr(body);
                if (LOG.isDebugEnabled()) {
                    /*
                    LOG.info("293) macrodef.name = {}" , logger.printString(name));
                    LOG.info("294) macrodef.syntaxRul = {}" , logger.printString(syntaxRule));
                    LOG.info("295) macrodef.define = {}" , logger.printString(define));
                    */
                    LOG.info("300) macrodef.syntaxRul = ={}", CurrentPort.printString(syntaxRule));
                    LOG.info("300) macrodef.define = ={}", CurrentPort.printString(define));
                }
                if (!(name instanceof SSymbol)) {
                    throw new LispException("Name must be a symbol");
                }
                SSymbol sname = (SSymbol)name;
                // add 
                LOG.info("304) env={}",  this.environ.printEnv()   );
                SyntaxRules compiledSyntax = new SyntaxRules(syntaxRule.getCdr(), this.environ);//  syntaxRuleをコンパイルする
                LOG.info("310) rules={}", compiledSyntax);

             //   this.environ.define(sname, syntaxRule);   
                this.environ.define(sname, compiledSyntax);   
                
                LOG.info("315) env={}", environ.printEnv());
                
                return  compile(define, next);
                
            } else {
                return procElse((Cell) x, next);

            }
        } else {
            return Op.mkCONSTANT(x, next);
        }
    }

    Op procElse(Cell x, Op next) {

        Object args = RT.cdr(x);
        //         compile
        Op c = invoke(RT.car(x), Op.APPLY);

        for (;;) {

            boolean b = isNull(args);

            if (b) {
                if (isReturnNext(next)) {
                    return c;
                } else {
                    return Op.mkFRAME(next, c);
                }
            }
            c = invoke(RT.car(args),                    Op.mkARGUMENT(c)            );
            args = RT.cdr(args);
        }
    }

    // 2024-11-12 **NEW**
    Op compBodys(Object body, Op next) {
    
        if (!(body instanceof Cell)) {
            return Op.mkCONSTANT(0L, next);
        } else {
            Cell cell = (Cell) body;
            Object car = cell.getCar();
            Object cdr = cell.getCdr();
            if (cdr instanceof Cell) {
                Op rest = compBodys(cdr, next);
                return invoke(car, rest);
            } else {
                
                return invoke(car, next);
            }
        }
    }
    
    @Deprecated
    Op newCompBody(Object body, Op next) {

        if (!(body instanceof Cell)) {
            return next;
        } else {
            Object car = RT.car(body);
            Object cdr = RT.cdr(body);

            Op suguato = invoke(car, Op.mkRET());

            Op mottoAto = newCompBody(cdr, next);

            return Op.mkFRAME(mottoAto, suguato);   //2024-10-06
/*
            if (isReturnNext(next)) {
                    return suguato;
            } else {
                    return Op.mkFRAME(mottoAto, suguato);
            }
             */
        }
    }

    private boolean isNull(Object o) {
        if (o == null) {

            return false;
        }
        if (o instanceof EmptyList) {
            return true;
        }
        return false;
    }

    private boolean isReturnNext(Op next) {

        return (next.code == 12);
    }

    private boolean isHalt(Op next) {

        return (next.code == 1);
    }

}
