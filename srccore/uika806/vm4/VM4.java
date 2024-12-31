/**
 * SPDX-License-Identifier: MPL-2.0
 * Author:  Masahito Hemmi.
 */
package uika806.vm4;

import java.util.Stack;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import uika806.err.LispException;
import uika806.err.RaiseException;
import uika806.err.UnboundException;
import uika806.kernel.RT;
import uika806.kernel.VMLogger;
import uika806.kernel.Values;
import uika806.kernel.BuiltInFuncs;
import uika806.objects.Cell;
import uika806.objects.SSymbol;
import uika806.objects.EmptyList;
import uika806.port.CurrentPort;

import uika806.syntax.Environ;

/**
 *
 * @author hemmi
 */
public class VM4 {

    private static final Logger LOG = LoggerFactory.getLogger(VM4.class);

    private Object a;
    private Op x;
    private Environ e;
    private Object r;
    private DumpRecord s;

    private final VMLogger logger;
    BuiltInFuncs buildIn;

    public VM4(Op xx, Environ ee, VMLogger log, BuiltInFuncs buildIn) {

        this.a = RT.EOL;
        this.x = xx;
        this.e = ee;
        this.r = RT.EOL;
        this.s = null;   // ***
        this.logger = log;
        this.buildIn = buildIn;
    }

    static Op RET = Op.mkRET();

    public Object exec() {

        for (;;) {

            if (logger != null) {
                logger.writeLog(a, x, e, r, s);
            }
            switch (x.getOpcode()) {
                case 1:
                    // HALT
                    return a;
                case 2: {
                    // REFER
                    SSymbol var = x.sym;
                    Op xx = x.nextOp;
                    Object val = e.get(var);
                    if (val == null) {
                        //throw new LispException("Unbound variable " + var);
                        throw new UnboundException( var, e);
                    }
                    a = val;
                    x = xx;
                    break;
                }
                case 3: {
                    // CONSTANT
                    Object arr0 = x.obj;
                    Op arr1 = x.nextOp;
                    a = arr0;
                    x = arr1;
                    break;
                }
                case 4: {
                    // CLOSE
                    Object vars = x.obj;
                    Op compiled = x.operation1;
                    Op arr2 = x.nextOp;
                    Object closure = new Closure(compiled, e, vars);
                    a = closure;
                    x = arr2;
                    break;
                }
                case 5: {
                    // TEST
                    Op arr0 = x.operation1;
                    Op arr1 = x.nextOp;
                    boolean aa = isTrue(a);
                  //  LOG.info("------------------- VM4 TEST {}", aa);
                    Op form = (aa) ? arr0 : arr1; // then or else
                    x = form;
                    break;
                }
                case 6: {
                    // ASSIGN
                    SSymbol arr0 = x.sym; 
                    Op arr1 = x.nextOp; 
                    if (logger != null) {
                        LOG.info("108) search ... {}", logger.printString(arr0));
                    }

                    e.set(arr0, a);

                    x = arr1;
                    break;
                }
                case 13: {
                    // DEF
                    SSymbol arr0 = x.sym;   // var
                    Op arr1 = x.nextOp;  // 次の式？
                    if (logger != null) {
                        LOG.info("124) search ... {}", logger.printString(arr0));
                        //   LOG.info("190) e = {},  top?={}", e.printEnv(), e.isTop());
                    }
                    // *defineはいきなり、Mapに入れてしまえ!!
                    this.e.define(arr0, a);  // a レジスタの値をセット
                    x = arr1;
                    break;
                }
                case 7: {
                    // CONTI
                    Op arr0 = x.nextOp;
                    Object res = continuation(s);
                    a = res;
                    x = arr0;
                    break;
                }

                case 8: {
                    // NUATE   自然 or 性質
                    DumpRecord ss = x.dump;
                    SSymbol var = x.sym;

                    a = e.get(var);  // 環境から、varを検索
                    LOG.info("160)      var={}, value={}", var, a);

                    x = RET;
                    s = ss;
                    break;
                }
                case 9: {
                    // FRAME
                    Op ret = x.nextOp;
                    Op xx = x.operation1;

                    DumpRecord cf = new DumpRecord(ret, e, r, s);

                    x = xx;
                    r = RT.EOL;
                    s = cf;
                    break;
                }

                case 10: {
                    // ARGUMENT

                    x = x.nextOp;
                    if (a instanceof Values) {
                        // multiple-values
                        r = new Cell(((Values) a).getValue1(), r);
                    } else {
                        r = new Cell(a, r);
                    }
                    break;
                }
                case 11: {
                    // APPLY
                    boolean isP = this.buildIn.isProcedure(a);

                    if (isP) {

                        try {
                            a = buildIn.builtInCall(a, r, e);
                            x = Op.mkRET();   //  RET;  
                        } catch (RaiseException le) {

                            LOG.info("%%%%%%% %%%%%%%% %%%%%%%% %%%%%%%%% %%%%%%%% catch Exception");
                            lispException = le;
                            if (exceptionHandler.isEmpty()) {
                                LOG.info("          exceptionHandler.isEmpty()");
                                throw le;
                            }
                            a = exceptionHandler.pop();  // 例外ハンドラ

                          //  if (le.isContinuable()) {
                            if (true) {

                             //   Closure res = continuation(s);

                             //   Op cmd = Op.mkARGUMENT(Op.mkCONSTANT(res, Op.APPLY));
                              //  Op cmd2 = Op.mkFRAME(cmd, b);
                                
                                r = le.getArgument(); // 24-10-29 引数は、RaiseException の中から取り出す
                                
                                LOG.info("207) contents of RaiseException is {}",                          CurrentPort.printString(r));
                                
                              //  x = cmd2;
                                x = Op.APPLY;

                            } else {
                                Closure clo = (Closure) a;
                                Op b = Op.mkCONSTANT(clo, Op.APPLY);

                                Op c = Op.mkCONSTANT(le, Op.mkARGUMENT(b));

                                LOG.info("203)  {}", c.debugString());

                                Op cmd = Op.mkSecondaryException();
                                Op cmd2 = Op.mkFRAME(cmd, c);
                                x = cmd2;

                            }

                        }
                    } else if (a instanceof Closure) {
                        Closure clo = (Closure) a;

                        Object body = clo.getBody();
                        Environ ee = (Environ) clo.getEnv();
                        Object vars = clo.getVars();
                        x = (Op) body;
                        e = ee.extend(vars, r);
                        
                        r = EmptyList.NIL;

                    } else {
                        if (a instanceof Cell) {
                            String strA = CurrentPort.printString(a);
                            LOG.info("242) strA={}", strA);
                            throw new LispException("243) can not invoke : " + strA);
                            
                        }
                        String decA = (a == null) ? "null" : a.getClass().getName();
                        throw new LispException("247) can not invoke : " + decA);
                    }

            
                    break;
                }
                case 12:
                    // RETURN
                    if (s == null) {
                        throw new LispException("VM's RETURN. but Stack is empty");
                    }
                    x = s.op;
                    //   e = e.returnTo(s.env);
                    e = (s.env);    // ******************  2024-10-14
                    r = s.rr;
                    s = s.ss;   // popしなくてよかった
                    break;
                case 14:
                    // PUSH
                    Op next = x.nextOp;
                    r = a;
                    x = next;
                    break;
                case 15: // EXCEP_HN
                {
                    Op thunk = x.nextOp;

                    exceptionHandler.push(a);
                    x = thunk;
                    r = RT.EOL;
                    break;
                }
                case 16:
                    // RE-THROW
                    throw lispException;

                case 19: // VALS_LIST
                {
                    Op _next = x.nextOp;
                    Object multiValList = val2list(a);  // multiple-valueをリストに変える
                    r = multiValList;
                    x = _next;
                }
                break;

                case 20:
                    // 2ndary Exception
                    throw new LispException("SecondaryException");

                default:
                    throw new IllegalStateException("Opcode=" + x.getOpcode());
            }

        } // for
    }

    Object val2list(Object values) {

        if (values instanceof Values) {
            Values vs = (Values) values;
            int n = vs.getNum();
            switch (n) {
                case 0:
                    return EmptyList.NIL;
                case 1:
                    return new Cell(vs.getValue1(), RT.EOL);
                case 2:
                    return new Cell(vs.getValue1(), new Cell(vs.getValue2(), RT.EOL));
                case 3:
                    return new Cell(vs.getValue1(), new Cell(vs.getValue2(), new Cell(vs.getValue3(), RT.EOL)));
                case 4:
                    return new Cell(vs.getValue1(), new Cell(vs.getValue2(),
                            new Cell(vs.getValue3(), new Cell(vs.getValue4(), RT.EOL))));
                default:
                    throw new IllegalStateException("Too many multiple values : " + n);

            }
        }

        return new Cell(values, RT.EOL);
    }

    LispException lispException = null;
    Stack<Object> exceptionHandler = new Stack<>();

    // Scheme では #f 以外全て true扱い
    boolean isTrue(Object obj) {

        Object a = (obj instanceof Values)
                ? ((Values) obj).getValue1()
                : obj;

        if (a instanceof Boolean) {
            Boolean aa = (Boolean) a;
            return aa;
        }
        return Boolean.TRUE;
    }

    Closure continuation(DumpRecord s) {

        Op code = Op.mkNUATE(s, SSymbol.VV);

        return new Closure(code, e.clearBinds(), new Cell(SSymbol.VV, RT.EOL));
    }

}
