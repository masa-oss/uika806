/**
 * SPDX-License-Identifier: MPL-2.0
 * Author:  Masahito Hemmi.
 */

package uika806.fn011.reader;

import java.util.regex.Matcher;
import java.util.regex.Pattern;

public final class NumParser {

    final String maybeNum;
    final int len;
    int state = 0;
    int idx = 0;

    Pattern complexPattern = Pattern.compile("^[+-]?(\\d+\\.*\\d*)|(nan\\.0)[+-]((\\d+\\.*\\d*)|inf\\.0)i"); //複素数

    Pattern ratioPattern = Pattern.compile("^[+-]?\\d+/\\d+"); //分数

    /**
     * Creates a new instance of NumParser
     *
     * @param str
     */
    public NumParser(String str) {
        this.maybeNum = str;
        this.len = str.length();
        _float_check();
    }

    static final int LEAD_INT = 1;
    static final int DOT_CHAR = 2;
    static final int TRAIL_INT = 4;
    static final int E_CHAR = 8;
    static final int EXP_INT = 16;

    private boolean isdigit(char c) {
        return ('0' <= c && c <= '9');
    }

    private char charAt(int idx) {

        if (idx < len) {
            return maybeNum.charAt(idx);
        }
        return '\0';
    }
    
    public boolean isRatio() {
        
        Matcher m = ratioPattern.matcher(maybeNum);
        if (m.find()) {
            return true;
        } else {
            return false;
        }
    }

    public boolean isComplex() {

        Matcher m = complexPattern.matcher(maybeNum);

        if (m.find()) {
            return true;
        } else {
            return false;
        }
    }

    public boolean isFloat() {
        return (idx == len
                && (state == (LEAD_INT | DOT_CHAR) // Add
                || state == (DOT_CHAR | TRAIL_INT) // Add
                || state == (LEAD_INT | DOT_CHAR | TRAIL_INT)
                || state == (DOT_CHAR | TRAIL_INT)
                || state == (LEAD_INT | E_CHAR | EXP_INT)
                || state == (LEAD_INT | DOT_CHAR | TRAIL_INT | E_CHAR | EXP_INT)
                || state == (DOT_CHAR | TRAIL_INT | E_CHAR | EXP_INT)));
    }

/*
    public boolean isFloat() {
        return (idx == len
                && (state == (LEAD_INT | DOT_CHAR | TRAIL_INT)
                || state == (DOT_CHAR | TRAIL_INT)
                || state == (LEAD_INT | E_CHAR | EXP_INT)
                || state == (LEAD_INT | DOT_CHAR | TRAIL_INT | E_CHAR | EXP_INT)
                || state == (DOT_CHAR | TRAIL_INT | E_CHAR | EXP_INT)));
    }
*/


    public boolean isInt() {
        return (idx == len
                && (state == LEAD_INT));
    }

    private void _float_check() {

        char ch = charAt(idx);
        if (ch == '+' || ch == '-') {
            idx++;
        }

        if (isdigit(charAt(idx))) {
            state |= LEAD_INT;
            while (isdigit(charAt(idx))) {
                idx++;
            }
        }
        if (charAt(idx) == '.') {
            state |= DOT_CHAR;
            idx++;
        }
        if (isdigit(charAt(idx))) {
            state |= TRAIL_INT;
            while (isdigit(charAt(idx))) {
                idx++;
            }
        }
        if (charAt(idx) == 'e') {
            state |= E_CHAR;
            idx++;
        }
        if ((charAt(idx) == '+') || (charAt(idx) == '-')) {
            idx++;
        }

        if (isdigit(charAt(idx))) {
            state |= EXP_INT;
            while (isdigit(charAt(idx))) {
                idx++;
            }
        }
    }
}
