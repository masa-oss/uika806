/**
 * SPDX-License-Identifier: MPL-2.0
 * Author:  Masahito Hemmi.
 */
package uika806.vm4;

import java.io.PrintWriter;
import java.io.StringWriter;
import uika806.objects.SSymbol;
import uika806.objects.ScmUniqueSymbol;
import uika806.print.PrinterSchemeLimit;

/**
 *
 */
public final class Op {

    final int code;
    SSymbol sym;
    Op operation1;
    Op nextOp;

    Object obj;
    DumpRecord dump;

    public static final Op HALT = new Op(1);
    public static final Op APPLY = new Op(11);
//    public static final Op RET = new Op(12);

    public Op(int code) {
        this.code = code;
    }

    static Op mkREFER(SSymbol x, Op next) {
        Op op = new Op(2);
        op.sym = x;
        op.nextOp = next;
        return op;
    }

    static Op mkCONSTANT(Object obj, Op next) {
        Op op = new Op(3);
        op.obj = obj;
        op.nextOp = next;
        return op;
    }

    static Op mkCLOSE(Object varList, Op compiled, Op next) {
        Op op = new Op(4);
        op.operation1 = compiled;
        op.obj = varList;
        op.nextOp = next;
        return op;
    }

    static Op mkTEST(Op then, Op elsec) {
        Op op = new Op(5);
        op.operation1 = then;
        op.nextOp = elsec;
        return op;
    }

    static Op mkASSIGN(SSymbol toSet, Op next) {
        Op op = new Op(6);
        op.sym = toSet;
        op.nextOp = next;
        return op;
    }

    static Op mkCONTI(Op next) {
        Op op = new Op(7);
        op.nextOp = next;
        return op;
    }

    // 2024-10-29 FIX
    static Op mkNUATE(DumpRecord o, SSymbol sym) {
        Op op = new Op(8);

        op.dump = o;
    //    op.obj = o;
        op.sym = sym;
        
        return op;
    }

    static Op mkFRAME(Op next, Op op1) {
        Op op = new Op(9);
        op.nextOp = next;
        op.operation1 = op1;
        return op;
    }

    static Op mkARGUMENT(Op next) {
        Op op = new Op(10);
        op.nextOp = next;
        return op;
    }

    static Op mkRET() {
        Op op = new Op(12);
        return op;
    }

    static Op mkDEF(SSymbol s, Op next) {
        Op op = new Op(13);
        op.sym = s;
        op.nextOp = next;
        return op;
    }

    static Op mkPUSH(Op next) {
        Op op = new Op(14);
        op.nextOp = next;
        return op;
    }

    static Op mkEXCEP_HN(Op next) {
        Op op = new Op(15);
        op.nextOp = next;
        return op;
    }

    @Deprecated
    static Op mkRETHROW() {
        Op op = new Op(16);
        return op;

    }

    static Op mkVALS_LIST(Op next) {
        Op op = new Op(19);
        op.nextOp = next;
        return op;
    }
    
    
    @Deprecated
    static Op mkSecondaryException() {
        Op op = new Op(20);
        return op;
    }

    PrinterSchemeLimit prin = new PrinterSchemeLimit();

    private void printConstant(PrintWriter pw) {
        String s = prin.prin1(obj);
        pw.print(s);
    }

    public String debugString() {
        
        StringWriter sw = new StringWriter();
        PrintWriter pw = new PrintWriter(sw, true);
        debugContents(pw);
        return sw.toString();
    }
    
    
    public void debugContents(PrintWriter pw) {

        pw.print("(");

        switch (code) {
            case 1:
                pw.print("HALT");
                break;
            case 2:
                pw.print("REFER");
                pw.print(" ");
                pw.print(sym.getName());
                pw.print(" ");
                this.nextOp.debugContents(pw);
                break;
            case 3:
                pw.print("CONSTANT");
                pw.print(" ");
                printConstant(pw);
                pw.print(" ");
                this.nextOp.debugContents(pw);
                break;
            case 4:
                pw.print("CLOSE");
                pw.print(" ");
                printConstant(pw);
                pw.print(" ");
                this.operation1.debugContents(pw);
                pw.print(" ");
                this.nextOp.debugContents(pw);
                break;
            case 5:
                pw.print("TEST");
                pw.print(" ");
                this.operation1.debugContents(pw);
                pw.print(" ");
                this.nextOp.debugContents(pw);
                break;
            case 6:
                pw.print("ASSIGN");
                pw.print(" ");
                pw.print(sym.getName());
                pw.print(" ");
                this.nextOp.debugContents(pw);
                break;
            case 7:
                pw.print("CONTI");
                break;
            case 8:
                pw.print("NUATE");
                break;
            case 9:
                pw.print("FRAME");
                pw.print(" ");
                this.nextOp.debugContents(pw);
                pw.print(" ");
                this.operation1.debugContents(pw);
                break;
            case 10:
                pw.print("ARGUMENT");
                pw.print(" ");
                this.nextOp.debugContents(pw);
                break;
            case 11:
                pw.print("APPLY");
                break;
            case 12:
                pw.print("RETURN");
                break;
            case 13:
                pw.print("DEF");
                break;
            case 14:
                pw.print("PUSH");
                break;
            case 15:
                pw.print("EXCEP_HN");
                break;
            case 16:
                pw.print("RETHROW");
                break;
            case 17:
                pw.print("KEEPVALS");
                break;
            case 18:
                pw.print("BINDVALS");
                break;
            case 19:
                pw.print("VALS_LIST");
                break;
                
            case 20:
                pw.print("SecondaryException");
                break;
                
                
        }

        pw.print(")");
    }

    public String displaySimply() {
        
        StringWriter sw = new StringWriter();
        PrintWriter pw = new PrintWriter(sw, true);
        
        pw.print(decodeOpCode());
        switch (code) {
            case 2:
            case 6:
                pw.print(" ");
                pw.print(symbolToString(sym));
                break;
            case 3:
            case 4:
                pw.print(" ");
                printConstant(pw);
                break;
            
            
        }
        
        
        return sw.toString();
    }
    
    
    String symbolToString(SSymbol sym) {
        
        SSymbol a = sym;
        if (a instanceof ScmUniqueSymbol) {
            ScmUniqueSymbol uni = (ScmUniqueSymbol) a;
            return uni.getOrigin().getName() + "#" + uni.getSeq();
        }
        
        return a.getName();
    }
    
    
    
    
    private String old_displaySimply() {
        
        StringWriter sw = new StringWriter();
        PrintWriter pw = new PrintWriter(sw, true);
        displaySimply(pw);
        return sw.toString();
    }
    
    private void displaySimply(PrintWriter pw) {

        pw.print("(");

        switch (code) {
            case 1:
                pw.print("HALT");
                break;
            case 2:
                pw.print("REFER");
                pw.print(" ");
                pw.print(sym.getName());
                pw.print(" ");
             //   this.nextOp.debugContents(pw);
                break;
            case 3:
                pw.print("CONSTANT");
                pw.print(" ");
                printConstant(pw);
                pw.print(" ");
             //   this.nextOp.debugContents(pw);
                break;
            case 4:
                pw.print("CLOSE");
                pw.print(" ");
                printConstant(pw);
                pw.print(" ");
           //     this.operation1.debugContents(pw);
      //     //     pw.print(" ");
          //      this.nextOp.debugContents(pw);
                break;
            case 5:
                pw.print("TEST");
                pw.print(" ");
            //    this.operation1.debugContents(pw);
      //      //    pw.print(" ");
            //    this.nextOp.debugContents(pw);
                break;
            case 6:
                pw.print("ASSIGN");
                pw.print(" ");
                pw.print(sym.getName());
                pw.print(" ");
            //    this.nextOp.debugContents(pw);
                break;
            case 7:
                pw.print("CONTI");
                break;
            case 8:
                pw.print("NUATE");
                break;
            case 9:
                pw.print("FRAME");
                pw.print(" ");
              //  this.nextOp.debugContents(pw);
                pw.print(" ");
             //   this.operation1.debugContents(pw);
                break;
            case 10:
                pw.print("ARGUMENT");
                pw.print(" ");
             //   this.nextOp.debugContents(pw);
                break;
            case 11:
                pw.print("APPLY");
                break;
            case 12:
                pw.print("RETURN");
                break;
            case 13:
                pw.print("DEF");
                break;
            case 14:
                pw.print("PUSH");
                break;
            case 15:
                pw.print("EXCEP_HN");
                break;
            case 16:
                pw.print("RETHROW");
                break;
            case 17:
                pw.print("KEEPVALS");
                break;
            case 18:
                pw.print("BINDVALS");
                break;
            case 19:
                pw.print("VALS_LIST");
                break;
                
            case 20:
                pw.print("SecondaryException");
                break;
                
        }

        pw.print(")");
    }


    public String decodeOpCode() {

        switch (code) {
            case 1:
                return "HALT";
            case 2: {
                int hashCode = super.hashCode();
                String hex = String.format("%09x", hashCode);
                return "REFER @ " + hex;   //2024-11-11
            }
            case 3:
                return "CONSTANT";
            case 4:
                return "CLOSE";
            case 5:
                return "TEST";
            case 6:
                return "ASSIGN";
            case 7:
                return "CONTI";
            case 8:
                return "NUATE";
            case 9: {
                int hashCode = super.hashCode();
                String hex = String.format("%09x", hashCode);
                
                return "FRAME @ " + hex;   //2024-11-11
            }
            case 10:
                return "ARGUMENT";
            case 11:
                return "APPLY";
            case 12: {
                int hashCode = super.hashCode();
                String hex = String.format("%09x", hashCode);
                return "RETURN @ " + hex;   //2024-11-11
            }
            case 13:
                return "DEF";
            case 14:
                return "PUSH";
            case 15:
                return "EXCEP_HN";
            case 16:
                return "RETHROW";
            case 17:
                return "KEEPVALS";
            case 18:
                return "BINDVALS";

            case 19:
                return ("VALS_LIST");
                

        }
        return "Op[" + code + "]";
    }

    @Override
    public String toString() {
        return decodeOpCode();
    }

    /**
     * @return the opcode
     */
    public int getOpcode() {
        return code;
    }

}
