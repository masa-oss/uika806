/**
 * SPDX-License-Identifier: MPL-2.0
 * Author:  Masahito Hemmi.
 */
package uika806.objects;

import java.util.Objects;

public class SpecialOperator {
    
    public static final SpecialOperator SETQ = new SpecialOperator("set!");
    
    public static final SpecialOperator QUOTE = new SpecialOperator("quote");

    public static final SpecialOperator SYN_DOT = new SpecialOperator("...");

    public static final SpecialOperator SYN_ELSE = new SpecialOperator("else");

    public static final SpecialOperator SYN_GT = new SpecialOperator(">=");
    
    public static final SpecialOperator CALL_CC = new SpecialOperator("call/cc");
    
    public static final SpecialOperator IF = new SpecialOperator("if");
    
    public static final SpecialOperator WITH_EXCEPTION = new SpecialOperator("with-exception-handler");


    public static final SpecialOperator LET = new SpecialOperator("let");
    public static final SpecialOperator LETREC = new SpecialOperator("letrec");
    public static final SpecialOperator LET_SYNTAX = new SpecialOperator("let-syntax");
    public static final SpecialOperator LETREC_SYNTAX = new SpecialOperator("letrec-syntax");

    
// TODO    
  //  public static final SpecialOperator LAMBDA = new SpecialOperator("lambda");
    
    
    
    
    
    final String name;
    
    private SpecialOperator(String name) {
        if (name == null) throw new NullPointerException();
        this.name = name;
    }



    @Override
    public String toString() {
        return "SpecialOperator[" + name + "]";
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
        final SpecialOperator other = (SpecialOperator) obj;
        return Objects.equals(this.name, other.name);
    }

}
