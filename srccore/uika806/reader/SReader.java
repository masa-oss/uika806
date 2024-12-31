/**
 * SPDX-License-Identifier: MPL-2.0
 * Author:  Masahito Hemmi.
 */
package uika806.reader;

/**
 *
 * @author hemmi
 */
public class SReader {
    
    
    Object read() {
        
        return null;
    }
    
    
    public Object match(String s) {
        
        if ("+inf.0".equals(s)) {
            return Double.POSITIVE_INFINITY;
        } else if ("-inf.0".equals(s)) {
            return Double.NEGATIVE_INFINITY;
        }
        
        return null;
    }


    public Object matchSharp(String s) {
        
        if ("t".equals(s)) {
            return Boolean.TRUE;
        } else if ("true".equals(s)) {
            return Boolean.TRUE;
        } else if ("false".equals(s)) {
            return Boolean.FALSE;

        } else if ("!fold-case".equals(s)) {
            return "fold-case";

        } else if ("!no-fold-case".equals(s)) {
            return "no-fold-case";
        }
        
        return null;
    }

    
    
}
