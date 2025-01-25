/**
 * SPDX-License-Identifier: MPL-2.0
 * Author:  Masahito Hemmi.
 */
package uika806.print;

import uika806.kernel.PrintOption;
import uika806.kernel.SelfPrintable;
import uika806.objects.SSymbol;
import uika806.objects.EmptyList;
import uika806.objects.Cell;
import uika806.objects.SChar;
import uika806.objects.SString;
import uika806.objects.ScmUniqueSymbol;
import uika806.port.StringOutputPort;
import uika806.vm4.Closure;

/**
 * depthとlengthによって、印刷が停止する印刷ロジック。
 * このクラスは、文字列を返却する事に注意。
 */
public final class PrinterSchemeLimit {

    // 文字列はエスケープして、(read port)が読めるように印字する。
    boolean readably = true;
    int dep;
    int len;

    public PrinterSchemeLimit() {
        this.dep = 10; //30;
        this.len = 10; //30;
    }

    public PrinterSchemeLimit(int depth, int length) {
        this.dep = depth;
        this.len = length;
    }

    //-----------------------------------------
    public static class NewPrintHelper {

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

        public NewPrintHelper copy() {
            return this;
        }
    }
    //-----------------------------------------

    public String prin1(Object sexp) {

        return prin1(sexp, new NewPrintHelper(), dep, len);
    }

    static final String OMIT_STRING = "#<ommit>";

    public String prin1(Object sexp, NewPrintHelper helper, int depth, int length) {

        if (depth <= 0) {
            return OMIT_STRING;
        }

        if (sexp == null) {
            return null;
        } else if (sexp instanceof EmptyList) {

            if (sexp == EmptyList.NIL) {
                return "()";
            } else {
                SSymbol atom = (SSymbol) sexp;
                return helper.printAtom(atom);
            }

        } else if (sexp instanceof SelfPrintable) {
            // add 2025-01-xx
            SelfPrintable sp = (SelfPrintable) sexp;
            
            StringOutputPort outport = new StringOutputPort();
            
            sp.prin1(outport, PrintOption.WRITE);
            return outport.getAsString();
            
        } else if (sexp instanceof Cell) {
            Cell cell = (Cell) sexp;
            StringBuilder sb = new StringBuilder();
            sb.append("(");

            NewPrintHelper newHelp = helper.copy();

            for (;;) {
                if (length <= 0) {
                    sb.append(OMIT_STRING);
                    break;
                }
                Object _car = cell.getCar();
                length--;

                sb.append(prin1(_car, newHelp, depth - 1, length));

                Object next = cell.getCdr();
                if (next instanceof Cell) {
                    sb.append(" ");
                    cell = (Cell) next;
                } else if (next instanceof EmptyList) {
                    break;
                } else {
                    sb.append(" . ");
                    sb.append(prin1(next, newHelp, depth - 1, length));
                    break;
                }
            }
            sb.append(")");
            return sb.toString();

        } else if (sexp instanceof SSymbol) {
            SSymbol atom = (SSymbol) sexp;
            return helper.printAtom(atom);

        } else if (sexp instanceof Boolean) {
            Boolean b = (Boolean) sexp;
            if (b) {
                return "#t";
            } else {
                return "#f";
            }

        } else if (sexp instanceof SString) {
            
            if (!readably) {
                // 人が読みやすい方。
              //  outport.write(sexp.toString());
                return "\"" + sexp + "\"";

            } else {
                // read処理にて、LispReaderが正しく読める方
                StringOutputPort outport = new StringOutputPort();
                PrintUtil.printStringReadably((SString) sexp, outport);

             //   return outport.toString();
                return outport.getAsString(); // FIX
            }
            
            
        } else if (sexp instanceof Number) {
            return sexp.toString();
        } else if (sexp instanceof SChar) {

            SChar sc = (SChar) sexp;

            StringBuilder sb = new StringBuilder();
            sb.append("#\\");
            sb.appendCodePoint(sc.getCodepoint());

            return sb.toString();

        } else if (sexp instanceof Closure) {
            Closure clo = (Closure) sexp;

            String var = prin1(clo.getVars(), helper, depth - 1, length - 1);
            return "#<closure " + var + ">";

        } else if (sexp instanceof String) {
            return "#<java String[" + sexp.toString() + "]>";

        } else {
            return "#<java " + sexp.toString() + ">";
        }
    }

}
