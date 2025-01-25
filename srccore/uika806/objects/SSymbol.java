/**
 * SPDX-License-Identifier: MPL-2.0
 * Author:  Masahito Hemmi.
 */
package uika806.objects;

import java.util.Objects;

/**
 * Symbol object.
 */
public class SSymbol {

 //   public static final SSymbol Undefined = new SSymbol("Undefined");
    
    
    public static final SSymbol VV = new SSymbol("V");   // Compile4 or Compile5 が利用
    

    public static SSymbol SYN_DOT = new SSymbol("...");  // syntax-rule
    public static SSymbol SYN_GT = new SSymbol("=>");  // syntax-rule
    public static SSymbol SYN_ELSE = new SSymbol("else");  // syntax-rule
    
    // ********************** COMPILER **********************

    public static SSymbol QUOTE = new SSymbol("quote");

    public static SSymbol INTERNAL_LAMBDA = new SSymbol("-λ");

    public static SSymbol LAMBDA = new SSymbol("lambda");
    
    
  //  public static SSymbol G_LAMBDA = new SSymbol("λ");
    
    
    public static SSymbol LETREC = new SSymbol("letrec");
    public static SSymbol LETREC_SYNTAX = new SSymbol("letrec-syntax");

    public static SSymbol SLETREC_SYNTAX = new SSymbol("*letrec-syntax");
    
    
    public static SSymbol HLETREC = new SSymbol("-letrec");


    public static SSymbol LET = new SSymbol("let");
    public static SSymbol LET_SYNTAX = new SSymbol("let-syntax");
    
    public static SSymbol LET_VALUES = new SSymbol("let-values");
    
    public static SSymbol HLET = new SSymbol("-let");
    
    
    public static SSymbol IF = new SSymbol("if");

    public static SSymbol SETQ = new SSymbol("set!");

    public static SSymbol CALL_CC = new SSymbol("call/cc");
    public static SSymbol CALL_CC2 = new SSymbol("call-with-current-continuation");

    public static SSymbol MACRO_DEF = new SSymbol("macrodef");

    public static SSymbol WITH_EXCEPTION = new SSymbol("with-exception-handler");
    
    
    // ******************** macro
    
    public static SSymbol SDEFINE = new SSymbol("*define");
    
    public static SSymbol DEFINE4 = new SSymbol("--define4");

    public static SSymbol DEFINE = new SSymbol("define");
    
    public static SSymbol DEFINE_VALUES = new SSymbol("define-values");

    public static SSymbol DEFINE_RECORD_TYPE = new SSymbol("define-record-type");

    public static SSymbol AND = new SSymbol("and");
    
    public static final String NAME_SPC = "scheme";
    
    
    public static SSymbol IMPORT = new SSymbol("import" );
    public static SSymbol SIMPORT = new SSymbol("*import" );
    


    // Compile Ver4
    public static SSymbol sApply = new SSymbol("apply");

    // Compile Ver5
    public static SSymbol S_APPLY = new SSymbol("*apply");
    
    
    


    public static SSymbol SBEGIN = new SSymbol("*begin");
    
    public static SSymbol CALL_W_VALUES = new SSymbol("call-with-values");

    // macro
    public static SSymbol SYNTAX_RULES = new SSymbol("syntax-rules");
    
    // macro
    public static SSymbol DEFINE_SYNTAX = new SSymbol("define-syntax");
    

    // *****

    // function
    public static SSymbol SDEF_MACRO = new SSymbol("*defmacro");
    
    public static SSymbol TEST_VALUES = new SSymbol("test-values");
    
    
    // ******************** package ********************
    
    public static final SSymbol LIB_UIKA806 = new SSymbol("uika806");
    public static final SSymbol LIB_TEST = new SSymbol("test");
    
    
    public static final SSymbol LIB_SCHEME = new SSymbol("scheme");
    
    //(import (scheme base))
    public static final SSymbol LIB_BASE = new SSymbol("base");

    //(import (scheme case-lambda))

    //(import (scheme char))
    public static final SSymbol LIB_CHAR = new SSymbol("char");

    //(import (scheme complex))
    public static final SSymbol LIB_COMPLEX = new SSymbol("complex");
    
    //(import (scheme eval))
    public static final SSymbol LIB_EVAL = new SSymbol("eval");
    
    //(import (scheme file))
    public static final SSymbol LIB_FILE = new SSymbol("file");
    //(import (scheme inexact))
    public static final SSymbol LIB_INEXACT = new SSymbol("inexact");

    //(import (scheme lazy))
    
    //(import (scheme load))
    public static final SSymbol LIB_LOAD = new SSymbol("load");

    //(import (scheme process-context))
    public static final SSymbol LIB_PROC = new SSymbol("process-context");


    //(import (scheme read))
    public static final SSymbol LIB_READ = new SSymbol("read");
    //(import (scheme repl))

    //(import (scheme time))
    public static final SSymbol LIB_TIME = new SSymbol("time");

    //(import (scheme write))
    public static final SSymbol LIB_WRITE = new SSymbol("write");
    
    public static final SSymbol LIB_CXR = new SSymbol("cxr");
    
    
    
    public static SSymbol EQV = new SSymbol("eqv?");

    
    public static SSymbol NTH = new SSymbol("nth");
    
    
    public static SSymbol NOT = new SSymbol("not");
    
    
    public static SSymbol REPORT_TEST = new SSymbol("*report-test");

    



    public static SSymbol LT = new SSymbol("<");
    public static SSymbol GT = new SSymbol(">");


    
    public static SSymbol BY_VEC = new SSymbol("bytevector");



    // *****
    public static SSymbol CAAAR = new SSymbol("caaar");
    public static SSymbol CAADR = new SSymbol("caadr");

    public static SSymbol CADAR = new SSymbol("cadar");
    public static SSymbol CADDR = new SSymbol("caddr");


    public static SSymbol CDAAR = new SSymbol("cdaar");
    public static SSymbol CDADR = new SSymbol("cdadr");

    public static SSymbol CDDAR = new SSymbol("cddar");
    public static SSymbol CDDDR = new SSymbol("cdddr");
    
    
    

    
    public static SSymbol OIF = new SSymbol("open-input-file");
    public static SSymbol OOF = new SSymbol("open-output-file");



    
    public static SSymbol CUR_SEC = new SSymbol("current-second");
    public static SSymbol CUR_JIF = new SSymbol("current-jiffy");
    public static SSymbol JIF_PER_SEC = new SSymbol("current-jiffy");
    
    
    // *** READER ***
    public static final SSymbol UNQUOTE = new SSymbol("unquote");

    public static final SSymbol UNQUOTE_SPLICE = new SSymbol("unquote-splice");

    // QUASI = 疑似(言語)
    public static final SSymbol QUASIQUOTE = new SSymbol("quasiquote");

    public static final SSymbol APPEND = new SSymbol("append");
    public static final SSymbol LIST = new SSymbol("list");

    public static final SSymbol CONS = new SSymbol("cons");

    
    
    // *** FEATURES ***
    public static final SSymbol FEAT_R7RS = new SSymbol("r7rs");

    public static final SSymbol FEAT_UNICODE = new SSymbol("full-unicode");

        
    public static final SSymbol FEAT_JVM = new SSymbol("jvm");
        
    public static final SSymbol FEAT_UIKA = new SSymbol("uika806");
        
    public static final SSymbol FEAT_UIKA_VER = new SSymbol("uika806-0.1");
        
    // **********************************************************
    
    protected final String name;
/*
    @Deprecated
    public SSymbol(String name, String pkgName) {

        if (name == null) {
            throw new NullPointerException();
        }
        this.name = name;
    }
*/
    /**
     * Creates a new instance
     *
     * @param name
     */
    public SSymbol(String name) {

        if (name == null) {
            throw new NullPointerException();
        }
        this.name = name;
    }


    
    static int genSymCount = 10;

    
    public static SSymbol gensym() {

        genSymCount++;
        return new SSymbol("G" + genSymCount);
    }
    
    public final String getName() {
        return name;
    }
    
    @Override
    public String toString() {
        return "SSymbol[" +  name + "]";
    }
    
    
    public String getReadableName() {
        return name;
    }
    
    

    @Override
    public int hashCode() {

        return Objects.hashCode(this.name);
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj) {
            return true;
        }
        if (obj == null) {
            return false;
        }
        if (getClass() != obj.getClass()) {
            return false;
        }
        final SSymbol other = (SSymbol) obj;
        return Objects.equals(this.name, other.name);
    }
    
}
