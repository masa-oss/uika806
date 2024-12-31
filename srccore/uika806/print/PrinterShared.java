/**
 * SPDX-License-Identifier: MPL-2.0
 * Author:  Masahito Hemmi.
 */
package uika806.print;

import java.io.IOException;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import uika806.pico.fn.Equal2Helper;
import uika806.objects.Cell;
import uika806.objects.EmptyList;
import uika806.kernel.SelfPrintable;
import uika806.objects.SArray;
import uika806.objects.U8Array;
import uika806.port.OutputPort;
import uika806.kernel.PrintOption;
import uika806.err.FileException;

/**
 * R7RSの 2.4章. データラベルの印刷が可能なPrint処理クラス。
 * このクラスは、write-sharedのような動作になっている。
 * 
 * R7RSより、引用「
 * write-shared 手続きは，共有構造が出力に一回以上現れる すべてのペアとベクタがデータラベルを使って
 * 表さなければ ならないことを除いて， write と同じである。」
 * 
 * 
 * ただし、R7RSのwriteに書かれている
 * 「データラベルは，環状が 無ければ使ってはならない。」の部分は実現できていない。
 * 
 * 
 * <code>
 *   #n=データ>字句構文
 *   #n# 字句構文
 *
 * 字句構文 #n# は #n= でラベル付けされたあるオブジェクトへの参照として機能する。
 * 結果は #n= と同じオブ ジェクトである 。
 *
 * これらの構文は，共有または環状の部分構造を有 する構造の表記を可能にする。
 * (let ((x (list ’a ’b ’c)))
 *     (set-cdr! (cddr x) x)
 *     x)
 *
 * ⇒ #0=(a b c . #0#)
 *
 * </code>
 * 
 * renamed PrinterSmall to PrinterShared
 * 
 */
public class PrinterShared extends PrinterSchemeEx {

    private static final Logger LOG = LoggerFactory.getLogger(PrinterShared.class);

    public PrinterShared() {

    }

    public PrinterShared(PrintOption opt) {
        super.option = opt;

        // Enumの比較は == で良い
        if (PrintOption.DISPLAY == (opt)) {
            super.readably = false;
        } else if (PrintOption.WRITE == (opt)) {
            super.readably = true;

        } else {
            throw new IllegalArgumentException();
        }

    }

    @Override
    public String printToString(Object o) {
        throw new RuntimeException();
    }

    @Override
    public void prin1(Object sexp, OutputPort outport) {

        Equal2Helper helper = new Equal2Helper();
        // 第一フェーズ
        helper.checkCircular(sexp);

        //  helper.dumpResult();
        prin2(sexp, outport, helper);
    }

    public void prin2(Object sexp, OutputPort outport, Equal2Helper helper) {

        if (sexp == null) {
            return;
        }

        Equal2Helper.ObjInfo pair0 = helper.findObject(sexp);
        if (pair0 != null && pair0.refCount > 1) {
            if (pair0.first) {
                String work = "#" + pair0.id + "=";
                pair0.first = false;
                //  outport.writeString(work);
                outport.write(work);
            } else {
                String work = "#" + pair0.id + "#";
                //   outport.writeString(work);
                outport.write(work);
                return;
            }
        }

        if (sexp instanceof SelfPrintable) {
            SelfPrintable sp = (SelfPrintable) sexp;
            sp.prin1(outport, option);

        } else if (sexp instanceof EmptyList) {

            // outport.writeString("()");
            outport.write("()");

        } else if (sexp instanceof Cell) {
            Cell cell = (Cell) sexp;

            secondPhaseList(cell, outport, helper);
            return;

        } else if (sexp instanceof SArray) {
            printArray((SArray) sexp, outport, helper);

        } else if (sexp instanceof U8Array) {
            printU8Array((U8Array) sexp, outport, helper);

        } else {
            super.printOtherThanCells(sexp, outport);
        }
    }

    private void printU8Array(U8Array array, OutputPort outport, Equal2Helper helper) {

        //  outport.writeString("#u8(");
        outport.write("#u8(");
        String spc = "";
        int len = array.length();
        for (int i = 0; i < len; i++) {

            //  outport.writeString(spc);
            outport.write(spc);

            long val = array.getNthAsInt(i);

            //  outport.writeString(String.valueOf(val));
            outport.write(String.valueOf(val));

            spc = " ";
        }
        //   outport.writeString(")");
        outport.write(")");
    }

    private void printArray(SArray array, OutputPort outport, Equal2Helper helper) {

      //  outport.writeString("#(");
        outport.write("#(");
        String spc = "";
        int len = array.length();
        for (int i = 0; i < len; i++) {
          //  outport.writeString(spc);
            outport.write(spc);
            Object obj = array.getNth(i);
            prin2(obj, outport, helper);
            spc = " ";
        }
      //  outport.writeString(")");
        outport.write(")");
    }

    private void secondPhaseList(Cell cell, OutputPort outport, Equal2Helper helper) {

        try {

            outport.write('(');

            for (;;) {

                Object _car = cell.getCar();

                prin2(_car, outport, helper);   // *** car を再帰呼び出し ****

                Object next = cell.getCdr();
                if (next instanceof Cell) {

                    Cell cdr = (Cell) next;

                    Equal2Helper.ObjInfo pair = helper.findObject(cdr);

                    if (pair != null && pair.refCount > 1) {
                        outport.write(' ');
                        if (pair.first) {
                            pair.first = false;

                            outport.write('.');
                            outport.write(' ');

                            prin2(cdr, outport, helper);   // *** cdr を再帰呼び出し ****

                        } else {

                            outport.write('.');
                            outport.write(' ');

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
                    prin2(next, outport, helper);   // *** cdr を再帰呼び出し ****
                    break;
                }
            }
            outport.write(')');

        } catch (IOException ioe) {
            throw new FileException("IOException", ioe);
        }

    }

}
