/**
 * SPDX-License-Identifier: MPL-2.0
 * Author:  Masahito Hemmi.
 */
package uika806.print;

import java.util.HashMap;
import java.util.Set;
import java.io.IOException;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;


import uika806.err.FileException;
import uika806.port.CodepointOutputPortNop;
import uika806.port.CodepointOutputPortMemory;
import uika806.port.OutputPort;

import uika806.kernel.SelfPrintable;
import uika806.kernel.IPrintable;
import uika806.kernel.PrintOption;
import uika806.kernel.AFn;

import uika806.objects.EmptyList;
import uika806.objects.Cell;
import uika806.objects.SSymbol;

import uika806.objects.SChar;
import uika806.objects.SString;

import uika806.objects.ScmUniqueSymbol;

/**
 * このクラスは、PrinterSharedの前のバージョンである。（古いバージョン）
 * 将来的に、廃止する予定のため、使わない事を推奨。
 * 
 * これを継承しているクラスがある為、修正は注意
 *
 * Created on 2024/08
 *
 */
public class PrinterSchemeEx implements IPrintable {

    private static final Logger LOG = LoggerFactory.getLogger(PrinterSchemeEx.class);

    protected boolean secondPhase = false;

    final boolean debugEnable = false;

    protected PrintOption option = PrintOption.DISPLAY;

    protected boolean readably = true;

    public PrinterSchemeEx() {
    }

    public void setSecondPhase() {

        if (this.debugEnable) {
            LOG.info("--------------dump start");

            Set<Cell> keySet = loopCheckMap.keySet();
            for (Cell c : keySet) {
                LOG.info("hash = {}", c.hashCode());
            }

            LOG.info("--------------dump  end ");
        }
        secondPhase = true;
    }

    @Override
    public String printToString(Object o) {

        SString prin1 = this.prin1(o);
        return prin1.toString();
    }

    public static class ConsInfo {

        final Cell c;
        public final int id;
        public boolean first = true;
        public int refCount = 1;

        public ConsInfo(Cell cc, int id) {
            this.c = cc;
            this.id = id;
        }

        public void inc() {
            refCount++;
        }

        @Override
        public String toString() {
            return "Pair[" + id + ", " + refCount + ", " + first + "]";
        }
    }
    protected HashMap<Cell, ConsInfo> loopCheckMap = new HashMap<>();

    protected int cellCounter = 0;

    ConsInfo checkCell(Cell cell) {

        ConsInfo found = loopCheckMap.get(cell);
        if (found != null) {
            return found;
        }
        cellCounter++;
        loopCheckMap.put(cell, new ConsInfo(cell, cellCounter));
        return null;
    }

    protected ConsInfo findCell(Cell cell) {

        ConsInfo found = loopCheckMap.get(cell);
        if (found != null) {
            return found;
        }
        return null;
    }

    public SString prin1(Object o) {

        // phase-1
        CodepointOutputPortNop outport = new CodepointOutputPortNop();
        prin1(o, outport);

        // phase-2
        setSecondPhase();

        CodepointOutputPortMemory out2 = new CodepointOutputPortMemory();
        prin1(o, out2);

        return out2.toSString();
    }

    public void prin1(Object sexp, OutputPort outport) {

        if (sexp == null) {
            return;

        } else if (sexp instanceof SelfPrintable) {
            SelfPrintable sp = (SelfPrintable) sexp;
            sp.prin1(outport, option);

        } else if (sexp instanceof EmptyList) {

          //  outport.writeString("()");
            outport.write("()");

        } else if (sexp instanceof Cell) {
            Cell cell = (Cell) sexp;

            if (secondPhase) {
                secondPhaseList(cell, outport);
            } else {
                // 第一フェーズ
                firstPhaseList(cell, outport);
            }
            return;

        } else {
            printOtherThanCells(sexp, outport);
        }
    }


    void printSChar(SChar sc, OutputPort outport) {

        try {

            outport.write('#');
            outport.write('\\');
            int c = sc.getCodepoint();

            switch (c) {
                case 7:
                 //   outport.writeString("alarm");
                    outport.write("alarm");
                    break;
                case '\b':
                 //   outport.writeString("backspace");
                    outport.write("backspace");
                    break;
                case 0x7f:
                 //   outport.writeString("delete");
                    outport.write("delete");
                    break;
                case 0x1b:
                 //   outport.writeString("escape");
                    outport.write("escape");
                    break;

                case '\n':
                 //   outport.writeString("newline");
                    outport.write("newline");
                    break;

                case 0:
                 //   outport.writeString("null");
                    outport.write("null");
                    break;
                case '\r':
                 //   outport.writeString("return");
                    outport.write("return");
                    break;
                case ' ':
                 //   outport.writeString("space");
                    outport.write("space");
                    break;

                case '\t':
                 //   outport.writeString("tab");
                    outport.write("tab");
                    break;

                default: {
                    if (c < 32) {
                        String wk = Integer.toHexString(c);

                     //   outport.writeString("x");
                        outport.write("x");
                     //   outport.writeString(wk);
                        outport.write(wk);

                    } else {
                        outport.write(c);
                    }
                }
            }

        } catch (IOException ioe) {
            throw new FileException("IOException", ioe);
        }

    }

    protected void printOtherThanCells(Object sexp, OutputPort outport) {

        if (sexp instanceof EmptyList) {

            outport.write("()");

        } else if (sexp instanceof SSymbol) {
            SSymbol atom = (SSymbol) sexp;

            String work = printAtom(atom);

            outport.write(work);

        } else if (sexp instanceof Boolean) {
            Boolean b = (Boolean) sexp;
            if (b) {
                outport.write("#t");
            } else {
                outport.write("#f");
            }

        } else if (sexp instanceof SString) {

            if (!readably) {
                // 人が読みやすい方。
                outport.write(sexp.toString());

            } else {
                // read処理にて、LispReaderが正しく読める方
                PrintUtil.printStringReadably((SString) sexp, outport);
            }

        } else if (sexp instanceof Number) {

            printNumber((Number) sexp, outport);

        } else if (sexp instanceof SChar) {

            // Unicode 2.0 Chara
            SChar sc = (SChar) sexp;
            try {
                if (!readably) {
                    outport.write(sc.getCodepoint());
                } else {
                    printSChar(sc, outport);
                }
            } catch (IOException ioe) {
                throw new FileException("IOException", ioe);
            }

        } else if (sexp instanceof AFn) {
            AFn afn = (AFn) sexp;
            //  outport.writeString("#<AFn " + afn.getName() + ">");
            outport.write("#<AFn " + afn.getName() + ">");

        } else if (sexp instanceof String) {
            outport.write("#<java String[" + sexp.toString() + "]>");
        } else {
            outport.write("#<java " + sexp.toString() + ">");
        }
    }

    public void printNumber(Number sexp, OutputPort outport) {

        //  outport.writeString(sexp.toString());
        outport.write(sexp.toString());
    }

    void firstPhaseList(Object obj, OutputPort outport) {

        if (obj == null) {
            return;
        } else if (!(obj instanceof Cell)) {
            return;
        }
        Cell cell = (Cell) obj;
        ConsInfo pair = checkCell(cell);
        if (pair != null) {
            //  LOG.info("found={}", pair.id);
            pair.inc();
            return;

        }
        Object car = cell.getCar();
        firstPhaseList(car, outport);
        Object cdr = cell.getCdr();
        firstPhaseList(cdr, outport);
    }

    private void secondPhaseList(Cell cell, OutputPort outport) {

        try {
            ConsInfo pair0 = findCell(cell);
            if (pair0 != null && pair0.refCount > 1) {
                if (pair0.first) {
                    String work = "#" + pair0.id + "=";
                    pair0.first = false;
                    //   outport.writeString(work);
                    outport.write(work);
                } else {
                    String work = "#" + pair0.id + "#";
                    //  outport.writeString(work);
                    outport.write(work);
                    return;
                }
            }

            outport.write('(');

            for (;;) {

                Object _car = cell.getCar();

                prin1(_car, outport);   // *** car を再帰呼び出し ****

                Object next = cell.getCdr();
                if (next instanceof Cell) {

                    Cell cdr = (Cell) next;
                    ConsInfo pair = findCell(cdr);
                    if (pair != null && pair.refCount > 1) {
                        outport.write(' ');
                        if (pair.first) {
                            pair.first = false;
                            outport.write('.');
                            outport.write(' ');

                            prin1(cdr, outport);   // *** cdr を再帰呼び出し ****

                        } else {

                            outport.write('.');
                            outport.write(' ');
                            //   LOG.info("found={}", pair.id);
                            String work = "#" + pair.id + "#";
                            //  outport.writeString(work);
                            outport.write(work);
                        }
                        break;
                    } else {
                        outport.write(' ');
                        cell = cdr;
                    }
                } else if (next instanceof EmptyList) {
                    break;
                } else {
                    outport.write(' ');
                    outport.write('.');
                    outport.write(' ');
                    prin1(next, outport);   // *** cdr を再帰呼び出し ****
                    break;
                }
            }
            outport.write(')');
        } catch (IOException ioe) {
            throw new FileException("IOException", ioe);
        }
    }

    public String printAtom(SSymbol sym) {

        if (sym == null) {
            return "-null-";
        }
        if (sym instanceof ScmUniqueSymbol) {
            ScmUniqueSymbol u = (ScmUniqueSymbol) sym;
            return u.getOrigin().getName() + "#" + u.getName();
        }
        return sym.getName();

    }

    String printSymbol(SSymbol sym) {
        /*
        String ns = sym.getPackageName();

        if (ns == null) {
            return "#:" + sym.getName();
        } else if ("scheme".equals(ns)) {
         */
        return sym.getName();
        /*
        } else {
            return ns + "/" + sym.getName();
        }*/
    }

}
