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
import uika806.vm4.Compile5;

import uika806.vm4.Op;
import uika806.vm4.VM4;

/**
 *  2025-01-09  Compile4 => Compile5に変更
 */
public class DoTestV5 {

    //   2024-11-01   27/27
    //   2025-01-09   27/27   Compile5 change
    //   2025-01-16   27/27   
    //   2025-01-23   27/27   
//    static String file = "gauche/test1.scm";
    
    //   2024-11-10   264行目のバッククオーとの処理に問題あり  ***** TODO[済]
    //   2024-11-12  32 / 38   lambda body change
    //   2025-01-09  32 / 38   Compile5 change
    //   2025-01-16            case マクロ に問題あり  
    //   2025-01-23            case マクロ に問題あり  
//       static String file = "gauche/test2.scm";

    //   2024-10-15  let-syntaxがある
    // 2025-01-03  Compile4 change : same exception,  syntax-rulesがVMからは、関数として呼び出せない為。
    // 2025-01-09  Compile5 change : same exception,  syntax-rulesがVMからは、関数として呼び出せない為。
    // 2025-01-20  7 / 9
    // 2025-01-23  8 / 10
//       static String file = "gauche/test-macro.scm";

    //  2024-11-01  call-with-valuesが変？
    //  2024-11-10  letの中の define-valuesが変
    //  2024-11-12  same exception
    //  2024-11-14   Line 15でエラー
    //  2024-11-15   Line 20でエラー
    //  2025-01-03  Compile4 change :   Line 20でエラー
    //  2025-01-09  Compile5 change :   Line 20でエラー
    //  2025-01-16  letの中の define-valuesが変           [BUG]
    //  2025-01-23  letの中の define-valuesが変           [BUG]
//      static String file = "gauche/test3.scm";

      //   2024-11-01  23/25
      //   2024-11-12  23/25   lambda body change
      //   2025-01-03  23/25   Compile4 change
      //   2025-01-09  23/25   Compile5 change
      //   2025-01-16  23/25   
      //   2025-01-23  23/25   
//       static String file = "gauche/test4.scm";
       
    //   2024-11-01  18/18 OK
    //   2024-11-12  18/18 OK   lambda body change
    //   2025-01-03  18/18      Compile4 change
    //   2025-01-09  18/18      Compile5 change
    //   2025-01-16  18/18      
    //   2025-01-23  18/18      
//       static String file = "gauche/test6.scm";
    
    //   2024-11-10  ratio の intValue で、error  
    //   2025-01-09   string-ci=? で error 。  (import (scheme char)) を忘れていた
    //   2025-01-23   59 / 65
//      static String file = "gauche/test7.scm";
    
    //   2024-11-01  17/17 OK
    //   2025-01-09  17/17      Compile5 change
    //   2025-01-16  17/17      
    //   2025-01-23  17/17      
//       static String file = "gauche/test8.scm";
    
    //   2024-11-01  72/72 OK  スピードが遅い
    //   2025-01-09  72/72      Compile5 change
    //   2025-01-16  72/72      
    //   2025-01-17  72/72      
    //   2025-01-23  72/72      
 //      static String file = "gauche/test9.scm";
    
    //   2024-11-01  94/100 OK
    //   2024-11-08 117/126 OK  (= 92.8%)
    //   2025-01-09 117/126      Compile5 change
    //   2025-01-17 117/126      
    //   2025-01-23 117/126      
//    static String file = "gauche/test10.scm";
          
    //   2024-11-01  43/43 OK
    //   2025-01-09  43/43      Compile5 change
    //   2025-01-17  43/43      
    //   2025-01-17  43/43      
//        static String file = "gauche/test11.scm";
    
    //   2024-11-01  36/39 
    //   2025-01-09  36/39      Compile5 change
    //   2025-01-17  36/39      
    //   2025-01-17  36/39      
//       static String file = "gauche/test12.scm";
    
    //   2024-11-01  145/147 OK
    //   2025-01-09  145/147      Compile5 change
    //                        uika806.err.UnboundException: Unbound variable G11   ERROR
    //   2025-01-17  145/147      
    //   2025-01-23  145/147      
//    static String file = "gauche/test5.scm";
       
    //   static String file = "gauche/test5_ng.scm";

    // 2024-11-01  13/15 OK
    // 2025-01-03  13/15       Compile5 change
    // 2025-01-15  15/15       letXX change
    // 2025-01-17  15/15       
    // 2025-01-23  15/15       
//    static String file = "gauche/test13.scm";
    
    // 2024-11-03    56 /  62 OK     < guard は解決 >
    // 2025-01-03    58 /  62       Compile5 change
    // 2025-01-15                   UnboundException: Unbound variable values#25  (Pending)
    // 2025-01-17       uika806.err.UnboundException: Unbound variable values#58
    // 2025-01-17    58 /  62       
    // 2025-01-23       uika806.err.UnboundException: Unbound variable values#58
//    static String file = "gauche/test16.scm";

    // 2024-11-03    21 /  26 OK。　原因不明で、削除したものアリ
    // 2025-01-09    21 /  26       Compile5 change
    // 2025-01-15    20 /  26       letXX change
    // 2025-01-17    21 /  26       
    // 2025-01-23    20 /  26       
//    static String file = "gauche/test14.scm";
    
    // 2024-11-04   53 /  65 OK。       | と \ の処理ができていない
    // 2024-11-12   53 /  65 OK。       処理が遅い　lambda body change
    // 2025-01-09   53 /  65       Compile5 change
    // 2025-01-09   53 /  65       letXX change
    // 2025-01-17   53 /  65       
    // 2025-01-23   53 /  65       
//    static String file = "gauche/test17.scm";

//    static String file = "gauche/test17_ng.scm";
    
    // 2024-11-08  2 / 3 OK
    // 2025-01-09  2 / 3       Compile5 change
    // 2025-01-15  2 / 3       letXX change
    // 2025-01-17  2 / 3       
    // 2025-01-23  2 / 3       
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
            // ここで、startup5.scmを読む
            factory.loadBase("startup5.scm", 5);
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
                    Compile5 comp = new Compile5(log);

                    Op op = comp.invoke(sexp, Op.HALT, lexEnv);

                    if (evalFlag) {

                        BuiltInFuncsImpl2 bu = new BuiltInFuncsImpl2();

                        VM4 vm = new VM4(op, lexEnv, log, bu);

                        try {
                            sexp = vm.exec();
                        } catch (RuntimeException re) {
                            re.printStackTrace();
                            
                            System.out.println("Error Line: " + fr.getLineNum());
                            System.out.println("evalating : " + CurrentPort.printLong(sexp)
                            );
                            
                            throw re;
                        }

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
