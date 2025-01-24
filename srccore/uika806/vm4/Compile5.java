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

import uika806.objects.EmptyList;
import uika806.objects.Cell;
import uika806.objects.SSymbol;
import uika806.objects.ScmUniqueSymbol;
import uika806.objects.SpecialOperator;

import uika806.pico.macro.IMacro;
import uika806.port.CurrentPort;

import uika806.syntax.SyntaxRules;
import uika806.syntax.Environ;

import uika806.vm4.vm5.HletHelper;
import uika806.vm4.vm5.HletrecHelper;
import uika806.vm4.vm5.HletrecsyntaxHelper;
import uika806.vm4.vm5.HletsyntaxHelper;
import uika806.vm4.vm5.LetrecsyntaxHelper;
import uika806.vm4.vm5.Sresult;

/**
 *
 */
public class Compile5 {

    private static final Logger LOG = LoggerFactory.getLogger(Compile5.class);

    VMLogger logger;

    public Compile5(VMLogger log) {

        this.logger = log;
    }

    public Op invoke(Object x, Op next, Environ environ) {

        if (logger != null) {
            LOG.warn("41) compile-x    = {}", logger.printString(x));
            LOG.warn("43) compile-next = {}", next.debugString());
        }
        Op comp = compile(x, next, environ);
        if (logger != null) {
            LOG.warn("48) compile-result = {}", comp.debugString());
        }
        return comp;
    }

    final HletHelper hletHelper = new HletHelper();

    final HletrecHelper hletrecHelper = new HletrecHelper();

    final HletsyntaxHelper hletsyntaxHelper = new HletsyntaxHelper();
    
    final HletrecsyntaxHelper hletrecsyntaxHelper = new HletrecsyntaxHelper();
    
    
    final LetrecsyntaxHelper letrecsyntaxHelper = new LetrecsyntaxHelper();
    
    
    public Op compile(Object x, Op next, Environ environ) {

        if (x instanceof SSymbol) {

            return Op.mkREFER((SSymbol) x, next);

        } else if (x instanceof Cell) {

            Cell seq = (Cell) x;
            Object first = seq.getCar();

            boolean isMacro = false;

            Object origin = null;
            Object maybeMacro = null;
            if (first instanceof SSymbol) {

                maybeMacro = environ.get(((SSymbol) first));

                if (maybeMacro instanceof IMacro) {
                    isMacro = true;

                } else if (maybeMacro instanceof SyntaxRules) {
                    //   LOG.error("*********** 2025-01-10 SyntaxRulesを変更    {}", maybeMacro.getClass().getName());
                    isMacro = true;

                }
            }

            if (first instanceof ScmUniqueSymbol) {
                origin = ((ScmUniqueSymbol) first).getOrigin();
            }

            // SyntaxRulesを先頭に移動
            if (maybeMacro instanceof SyntaxRules) {
                Object rest = seq.getCdr();
                SyntaxRules rules = (SyntaxRules) maybeMacro;
                Object obj = null;
                try {
                    obj = rules.transform(rest, environ);
                } catch (RuntimeException re) {
                    LOG.error("SyntaxRules-error", re);
                    throw re;
                }
                if (logger != null) {
                    LOG.info("94)      macro-expanded = {}", logger.printString(obj));
                }

                return compile(obj, next, environ);

//            if (SSymbol.QUOTE.equals(first) || SSymbol.QUOTE.equals(origin)) {
            } else if (maybeMacro == SpecialOperator.QUOTE) {

                Object arr0 = RT.cadr(seq);

                return Op.mkCONSTANT(arr0, next);

                //   } else if (isMacro) {
            } else if (maybeMacro instanceof IMacro) {
                // マクロ展開
                Object rest = seq.getCdr();

                AFn afn = (AFn) maybeMacro;
                // 展開関数を呼ぶ
                Object expa = afn.invokeWithEnv(rest, environ);
                if (logger != null) {
                    LOG.info("131)      macro-expanded = {}", logger.printString(expa));
                }

                return compile(expa, next, environ);

//            } else if (SSymbol.LET.equals(first) || SSymbol.LET.equals(origin)) {
            } else if (maybeMacro == SpecialOperator.LET) {

                //  define-syntaxで、フォームを変換し、環境はJavaで、追加
                Sresult sr = hletHelper.doSpecial(seq, environ);
                LOG.info("141) $$$$$$$$$  JIKKENTEKI let   newEnv={}", sr.environ.printEnv());

                return compile(sr.form, next, sr.environ);

            } else if (maybeMacro == SpecialOperator.LET_SYNTAX) {

                //  define-syntaxで、フォームを変換し、環境はJavaで、追加
                Sresult sr = hletsyntaxHelper.doSpecial(seq, environ);
                LOG.info("149) $$$$$$$$$  JIKKENTEKI let-syntax   newEnv={}", sr.environ.printEnv());

                return compile(sr.form, next, sr.environ);
                
//            } else if (SSymbol.LETREC.equals(first) || SSymbol.LETREC.equals(origin)) {
            } else if (maybeMacro == SpecialOperator.LETREC) {

                Sresult sr = hletrecHelper.doSpecial(seq, environ);
                LOG.info("158) $$$$$$$$$  JIKKENTEKI letrec   newEnv={}", sr.environ.printEnv());

                return compile(sr.form, next, sr.environ);

            } else if (maybeMacro == SpecialOperator.LETREC_SYNTAX) {
/*                
                Sresult sr = hletrecsyntaxHelper.doSpecial(seq, environ);
                LOG.info("167) $$$$$$$$$  JIKKENTEKI letrec-syntax   newEnv={}", sr.environ.printEnv());
                
                return compile(sr.form, next, sr.environ);
                
            } else if (SSymbol.SLETREC_SYNTAX.equals(first) || SSymbol.SLETREC_SYNTAX.equals(origin)) {
*/                
                Sresult sr = letrecsyntaxHelper.doSpecial(seq, environ);
                LOG.info("179) $$$$$$$$$  JIKKENTEKI letrec-syntax   newEnv={}", sr.environ.printEnv());
                
                return compile(sr.form, next, sr.environ);
                
            } else if (SSymbol.INTERNAL_LAMBDA.equals(first) || SSymbol.INTERNAL_LAMBDA.equals(origin)) {

                Object var = RT.cadr(seq);
                Object arr1 = RT.cddr(seq);

                Op compiled = null;
                if (true) {
                    // bodyが複数対応
                    //  compiled = newCompBody(arr1, Op.RET);
                    //  compiled = newCompBody(arr1, Op.mkRET());

                    compiled = compBodys(arr1, Op.mkRET(), environ);  // 2024-11-12

                } else {
                    // bodyが１個
                    compiled = invoke(RT.car(arr1), Op.mkRET(), environ);
                }
                return Op.mkCLOSE(var, compiled, next);

//            } else if (SSymbol.IF.equals(first) || SSymbol.IF.equals(origin)) {
            } else if (maybeMacro == SpecialOperator.IF) {

                Object arr0 = RT.cadr(seq);
                Object arr1 = RT.caddr(seq);
                Object arr3 = RT.cdddr(seq);
                Object elseClause = (arr3 instanceof Cell) ? RT.car(arr3) : RT.EOL;

                Op thenc = invoke(arr1, next, environ);
                Op elsec = invoke(elseClause, next, environ);
                return invoke(arr0, Op.mkTEST(thenc, elsec), environ);

//            } else if (SSymbol.SETQ.equals(first) || SSymbol.SETQ.equals(origin)) {
            } else if (maybeMacro == SpecialOperator.SETQ) {

                Object arr0 = RT.cadr(seq);
                SSymbol sym = (SSymbol) arr0;
                Object arr1 = RT.caddr(seq);

                return invoke(arr1, Op.mkASSIGN(sym, next), environ);

                /*   2025-01-16 Compiler3 & VM3用とあったので remove
            } else if (SSymbol.SDEFINE.equals(first)) {
                // ここは *defineなので、 Compiler3 & VM3用

                Object arr0 = RT.cadr(seq);
                Object arr1 = null;
                Object cddr = RT.cddr(seq);
                if (cddr == RT.EOL) {
                    // 第二引数は省略された
                    arr1 = 0;
                } else {
                    // 第二引数あり
                    arr1 = RT.car(cddr);
                }
                boolean bTail = this.isHalt(next);
                //   LOG.info("===================== *define :  isHalt={}", bTail);
                if (bTail) {
                    return invoke(arr1, Op.mkDEF((SSymbol) arr0, next), environ);
                } else {
                    // 内部にdefineが書かれた時
                    throw new LispException("");
                }
                 */
//            } else if (SSymbol.CALL_CC.equals(first) || SSymbol.CALL_CC2.equals(first)
//                    || SSymbol.CALL_CC.equals(origin) || SSymbol.CALL_CC2.equals(origin)) {

            } else if (maybeMacro == SpecialOperator.CALL_CC) {
                
                Object xx = RT.cadr(seq);

                Op c = Op.mkCONTI(
                        Op.mkARGUMENT(
                                invoke(xx, Op.APPLY, environ))
                );

                boolean bn = isReturnNext(next);
                if (this.logger != null) {
                    LOG.info("181) {} {}", bn, logger.printString(next));
                }

                if (bn) {
                    return c;
                } else {
                    return Op.mkFRAME(next, c);
                }

//            } else if (SSymbol.WITH_EXCEPTION.equals(first) || SSymbol.WITH_EXCEPTION.equals(origin)) {
            } else if (maybeMacro == SpecialOperator.WITH_EXCEPTION) {

                Object handler = RT.cadr(seq);
                Object thunk = RT.caddr(seq);

                Op c = invoke(thunk, Op.APPLY, environ);
                Op compThunk = (isReturnNext(next)) ? c : Op.mkFRAME(next, c);

                Op registHandler = Op.mkEXCEP_HN(compThunk);
                return invoke(handler, registHandler, environ);

            } else if (SSymbol.sApply.equals(first) || SSymbol.sApply.equals(origin)) {
/*                
                
                // Compile V4
                Object fun = RT.cadr(seq);
                Object args = RT.caddr(seq);
                if (logger != null) {
                    LOG.info("204) fun = {}, args={}    next={}",
                            logger.printString(fun),
                            logger.printString(args),
                            logger.printString(next));
                }

                Op temp = Op.mkPUSH(invoke(fun, Op.APPLY, environ));

                Op temp2 = invoke(args, temp, environ);
                Op c = Op.mkFRAME(next, temp2);
                return c;

            } else if (SSymbol.APPLY.equals(first) || SSymbol.APPLY.equals(origin)) {
*/                
                
                // Compile V5

                Object fun = RT.cadr(seq);
                Object args = RT.cddr(seq); // change
                
                Op applyFn = invoke(fun, Op.APPLY, environ);

                Op op = applyBody((Cell) args, applyFn, environ);

                Op c = Op.mkFRAME(next, op);
                return c;

            } else if (SSymbol.CALL_W_VALUES.equals(first) || SSymbol.CALL_W_VALUES.equals(origin)) {

                Object body = RT.cdr(x);
                Object firstLambda = RT.car(body);
                Object secondLambdaOrSymbol = RT.cadr(body);

                if (logger != null) {
                    LOG.info("266) {}", logger.printString(secondLambdaOrSymbol));
                }

                Op second = this.invoke(secondLambdaOrSymbol, Op.APPLY, environ);

                Op comp1st = this.invoke(firstLambda, Op.APPLY, environ);

                Op keep = Op.mkFRAME(next, Op.mkVALS_LIST(second));

                Op result = Op.mkFRAME(keep, comp1st);
                return result;

            } else if (SSymbol.MACRO_DEF.equals(first)) {
                // defineの中に define-syntaxを書けるようにする実験。
                // (macrodef myif (syntax-rules ...) (define ...) )
                // 現在の環境に myifを定義して、 (define ...)をコンパイルする

                Object body = RT.cdr(x);
                Object name = RT.car(body);
                Cell syntaxRule = (Cell) RT.cadr(body);
                Object define = RT.caddr(body);
                if (LOG.isDebugEnabled()) {
                    LOG.info("300) macrodef.syntaxRul = ={}", CurrentPort.printString(syntaxRule));
                    LOG.info("300) macrodef.define = ={}", CurrentPort.printString(define));
                }
                if (!(name instanceof SSymbol)) {
                    throw new LispException("Name must be a symbol");
                }
                SSymbol sname = (SSymbol) name;
                // add 
                LOG.info("304) env={}", environ.printEnv());
                SyntaxRules compiledSyntax = new SyntaxRules(syntaxRule.getCdr(), environ);//  syntaxRuleをコンパイルする
                LOG.info("310) rules={}", compiledSyntax);

                environ.define(sname, compiledSyntax);

                LOG.info("315) env={}", environ.printEnv());

                return compile(define, next, environ);

            } else {
                // 関数呼び出し？
                return procElse((Cell) x, next, environ);
            }
        } else {
            return Op.mkCONSTANT(x, next);
        }
    }

    Op applyBody(Cell x, Op next, Environ environ) {

        Object cdr = x.getCdr();

        if (!(cdr instanceof Cell)) {
            // 最後の引数は PUSH命令
            Op push = Op.mkPUSH(next);

            Op temp = compile(x.getCar(), push, environ);
            return temp;
        } else {

            Op rest2 = Op.mkARGUMENT(next);
            
            Op car = invoke(x.getCar(), rest2, environ);
            Op rest = applyBody((Cell) cdr, car, environ);
            
            return rest;
        }

    }

    Op procElse(Cell x, Op next, Environ environ) {

        Object args = RT.cdr(x);
        //         compile
        Op c = invoke(RT.car(x), Op.APPLY, environ);

        for (;;) {

            boolean b = isNull(args);

            if (b) {
                if (isReturnNext(next)) {
                    return c;
                } else {
                    return Op.mkFRAME(next, c);
                }
            }
            c = invoke(RT.car(args), Op.mkARGUMENT(c), environ);
            args = RT.cdr(args);
        }
    }

    // 2024-11-12 **NEW**
    Op compBodys(Object body, Op next, Environ environ) {

        if (!(body instanceof Cell)) {
            return Op.mkCONSTANT(0L, next);
        } else {
            Cell cell = (Cell) body;
            Object car = cell.getCar();
            Object cdr = cell.getCdr();
            if (cdr instanceof Cell) {
                Op rest = compBodys(cdr, next, environ);
                return invoke(car, rest, environ);
            } else {

                return invoke(car, next, environ);
            }
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
