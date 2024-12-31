/**
 * SPDX-License-Identifier: MPL-2.0
 * Author:  Masahito Hemmi.
 */
package uika806.pico.test;

import java.io.IOException;

import uika806.Services;
import uika806.small.env.BuiltInFuncsImpl2;
import uika806.small.env.EnvironFactory;
import uika806.reader.LispReaderEx;
import uika806.fn011.reader.Tokenizer;
import uika806.objects.Cell;
import uika806.kernel.VMLogger;
import uika806.objects.SString;
import uika806.port.CodepointOutputPortImpl;
import uika806.pico.fn.OpenInputFileFn;
import uika806.print.PrinterShared;
import uika806.reader.LispReaderFx;
import uika806.port.InputPort;
import uika806.port.CurrentPort;
import uika806.syntax.Environ;
import uika806.vm4.Compile4;
import uika806.vm4.Op;
import uika806.vm4.VM4;

/**
 *  11/10  lambdaの中で、内部defineの処理をするように変更
 */
public class DoTest {

    //   2024-11-01   27/27
    //   2024-11-10   27/27
    //   2024-11-12   27/27   lambda body change
    //   2024-11-14   27/27   lambda body change
    //   2024-11-15   27/27   *DEFINE change
//     static String file = "gauche/test1.scm";
    
    //   2024-11-10   264行目のバッククオーとの処理に問題あり  ***** TODO[済]
    //   2024-11-11  32 / 38
    //   2024-11-12  32 / 38   lambda body change
    //   2024-11-14  32 / 38   lambda body change  (debug FIX)
    //   2024-11-15  32 / 38   define change
//       static String file = "gauche/test2.scm";

    //   2024-10-15  let-syntaxがある
    //   2024-11-10  何か、別の原因か, syntax-rulesがVMからは、関数として呼び出せない為。
    // 2024-11-12  same exception,  syntax-rulesがVMからは、関数として呼び出せない為。
    // 2024-11-14  same exception,  syntax-rulesがVMからは、関数として呼び出せない為。
    // 2024-11-15  same exception,  syntax-rulesがVMからは、関数として呼び出せない為。
//       static String file = "gauche/test-macro.scm";

    //  2024-11-01  call-with-valuesが変？
    //  2024-11-10  letの中の define-valuesが変
    //  2024-11-12  same exception
    //  2024-11-14   Line 15でエラー
    //  2024-11-15   Line 20でエラー   **** TODO
//      static String file = "gauche/test3.scm";

      //   2024-11-01  23/25
      //   2024-11-10  23/25
      //   2024-11-12  23/25   lambda body change
      //   2024-11-14  23/25   lambda body change
      //   2024-11-15  23/25   define change
 //      static String file = "gauche/test4.scm";
       
    //   2024-11-01  18/18 OK
    //   2024-11-10  18/18 OK
    //   2024-11-12  18/18 OK   lambda body change
    //   2024-11-14  18/18 OK   lambda body change
    //   2024-11-15  18/18 OK   define change
//       static String file = "gauche/test6.scm";
    
    //   2024-11-10  ratio の intValue で、error  
    //   2024-11-12  ratioを修正。 string-ci=?] で error  **** TODO
    //   2024-11-14  ratioを修正。 string-ci=?] で error  **** TODO
    //   2024-11-15  ratioを修正。 string-ci=?] で error  **** TODO
//      static String file = "gauche/test7.scm";
    
    //   2024-11-01  17/17 OK
    //   2024-11-10  17/17 OK
    //   2024-11-12  17/17 OK   lambda body change
    //   2024-11-14  17/17 OK   lambda body change
    //   2024-11-15  17/17 OK   define change
//       static String file = "gauche/test8.scm";
    
    //   2024-11-01  72/72 OK  スピードが遅い
    //   2024-11-10  72/72 OK  スピードが遅い
    //   2024-11-12  72/72 OK  スピードが遅い   lambda body change
    //   2024-11-14  72/72 OK  スピードが遅い   lambda body change
    //   2024-11-15  72/72 OK  スピードが遅い   define change
//       static String file = "gauche/test9.scm";
    
    //   2024-11-01  94/100 OK
    //   2024-11-08 117/126 OK  (= 92.8%)
    //   2024-11-10 117/126 OK  (= 92.8%)
    //   2024-11-12 117/126 OK  (= 92.8%)   lambda body change
    //   2024-11-14 117/126 OK  (= 92.8%)   lambda body change
    //   2024-11-15 117/126 OK  (= 92.8%)   define change
//    static String file = "gauche/test10.scm";
          
    //   2024-11-01  43/43 OK
    //   2024-11-10  43/43 OK
    //   2024-11-12  43/43 OK   lambda body change
    //   2024-11-14  43/43 OK   lambda body change
    //   2024-11-15  43/43 OK   define change
 //       static String file = "gauche/test11.scm";
    
    //   2024-11-01  36/39 
    //   2024-11-10  36/39 
    //   2024-11-12  36/39    lambda body change
    //   2024-11-14  36/39    lambda body change
    //   2024-11-15  36/39    define change
 //      static String file = "gauche/test12.scm";
    
    //   2024-11-01  145/147 OK
    //   2024-11-10  145/147 OK
    //   2024-11-12  145/147 OK    lambda body change
    //   2024-11-14  145/147 OK    lambda body change
    //   2024-11-15  145/147 OK    define change
//       static String file = "gauche/test5.scm";
       
    //   static String file = "gauche/test5_ng.scm";

       // 2024-11-01  13/15 OK
       // 2024-11-10  13/15 OK
       // 2024-11-12  13/15 OK    lambda body change
       // 2024-11-14  13/15 OK    lambda body change
       // 2024-11-15  13/15 OK    define change
  //      static String file = "gauche/test13.scm";
    
    // 2024-11-03    56 /  62 OK     < guard は解決 >
    // 2024-11-08    58 /  62 OK     < guard は解決 >
    // 2024-11-10    58 /  62 OK     < guard は解決 >
    // 2024-11-12    58 /  62 OK    lambda body change
    // 2024-11-14    58 /  62 OK    lambda body change
    // 2024-11-15    58 /  62 OK    define change
 //   static String file = "gauche/test16.scm";

    // 2024-11-03    21 /  26 OK。　原因不明で、削除したものアリ
    // 2024-11-10    21 /  26 OK。　原因不明で、削除したものアリ
    // 2024-11-12    21 /  26 OK。　lambda body change
    // 2024-11-14    21 /  26 OK。　lambda body change
    // 2024-11-15    21 /  26 OK。　define  change
//    static String file = "gauche/test14.scm";
    
    // 2024-11-04   53 /  65 OK。       | と \ の処理ができていない
    // 2024-11-12   53 /  65 OK。       処理が遅い　lambda body change
    // 2024-11-14   53 /  65 OK。       処理が遅い  lambda define-XXシリーズ
    // 2024-11-15   53 /  65 OK。       処理が遅い  define 変更
//    static String file = "gauche/test17.scm";
//    static String file = "gauche/test17_ng.scm";
    
    // 2024-11-08  2 / 3 OK
    // 2024-11-10  2 / 3 OK
    // 2024-11-14  2 / 3 OK
    // 2024-11-15  2 / 3 OK
    static String file = "gauche/test15.scm";
    
    
    public static void main(String args[]) {

        OpenInputFileFn inpFn = new OpenInputFileFn();
        try {
            CurrentPort.init(true);

        } catch (IOException ioe) {
            ioe.printStackTrace();
            return;

        }
        try {
            EnvironFactory factory = new EnvironFactory();
            factory.loadBase();
            Services.environFactory = factory;

            Environ lexEnv = factory.getFirstEnviron(false, false);

            Object eof = new Cell(null, null);

            SString sFile = SString.fromString(file);
            InputPort fr = (InputPort) inpFn.invoke(sFile);

            boolean compileFlag = true;
            boolean evalFlag = true;
            for (;;) {
                Tokenizer tk = new Tokenizer(fr);
                LispReaderEx reader = new LispReaderFx(tk);
                Object sexp = reader.read(false, eof);

                if (sexp == eof) {
                    break;
                }
                System.err.println("Line: " + fr.getLineNum());
                System.err.println("before eval:" + printStr(sexp));
                if (compileFlag) {
                    VMLogger log = null;
                    // VMLogger log = new VMLoggerImpl();
                    Compile4 comp = new Compile4(log, lexEnv);

                    Op op = comp.invoke(sexp, Op.HALT);

                    if (evalFlag) {

                        BuiltInFuncsImpl2 bu = new BuiltInFuncsImpl2();

                        VM4 vm = new VM4(op, lexEnv, log, bu);

                        sexp = vm.exec();
                    }

                }

                System.err.println("" + printStr(sexp));
            }

        } catch (Exception ex) {
            ex.printStackTrace();
        }
        System.out.println("-----------------------");
    }

    static String printStr(Object sexp) {

        CodepointOutputPortImpl outport = new CodepointOutputPortImpl();
        PrinterShared instance = new PrinterShared();
        instance.prin1(sexp, outport);
        return outport.getAsString();
    }

}
