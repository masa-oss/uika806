/**
 * SPDX-License-Identifier: MPL-2.0
 * Author:  Masahito Hemmi.
 */
package uika806.fn011.reader;

public final class CFlags {

    short[] chars;

    public CFlags() {
        this.chars = new short[128];
        initR5();
    }

    /* 頭文字 */
    static short K = 1;
    /* 継続文字 */
    static short J = 2;
    /* 空白文字 */
    static short W = 8;

    final void initR5() {

        chars[0] = W;
        chars[1] = 0;
        chars[2] = 0;
        chars[3] = 0;
        chars[4] = 0;
        chars[5] = 0;
        chars[6] = 0;
        chars[7] = 0;
        chars[8] = 0;
        chars[9] = W;
        chars[10] = W;
        chars[11] = 0;
        chars[12] = 0;
        chars[13] = W;
        chars[14] = 0;
        chars[15] = 0;
        chars[16] = 0;
        chars[17] = 0;
        chars[18] = 0;
        chars[19] = 0;
        chars[20] = 0;
        chars[21] = 0;
        chars[22] = 0;
        chars[23] = 0;
        chars[24] = 0;
        chars[25] = 0;
        chars[26] = 0;
        chars[27] = 0;
        chars[28] = 0;
        chars[29] = 0;
        chars[30] = 0;
        chars[31] = 0;

        chars[' '] = W;
        chars['!'] = K;
        chars['"'] = 0;
        chars['#'] = 0;
        chars['$'] = K;
        chars['%'] = K;
        chars['&'] = K;
        chars['\''] = 0;
        chars['('] = 0;
        chars[')'] = 0;
        chars['*'] = K;
        chars['+'] = K;
        chars[','] = 0;
        chars['-'] = K;
        chars['.'] = K;
        chars['/'] = K;
        chars['0'] = J;
        chars['1'] = J;
        chars['2'] = J;
        chars['3'] = J;
        chars['4'] = J;
        chars['5'] = J;
        chars['6'] = J;
        chars['7'] = J;
        chars['8'] = J;
        chars['9'] = J;
        chars[':'] = K;
        chars[';'] = 0;
        chars['<'] = K;
        chars['='] = K;
        chars['>'] = K;
        chars['?'] = K;

        chars['@'] = K;
        chars['A'] = K;
        chars['B'] = K;
        chars['C'] = K;
        chars['D'] = K;
        chars['E'] = K;
        chars['F'] = K;
        chars['G'] = K;
        chars['H'] = K;
        chars['I'] = K;
        chars['J'] = K;
        chars['K'] = K;
        chars['L'] = K;
        chars['M'] = K;
        chars['N'] = K;
        chars['O'] = K;
        chars['P'] = K;
        chars['Q'] = K;
        chars['R'] = K;
        chars['S'] = K;
        chars['T'] = K;
        chars['U'] = K;
        chars['V'] = K;
        chars['W'] = K;
        chars['X'] = K;
        chars['Y'] = K;
        chars['Z'] = K;
        chars['['] = 0;
        chars['\\'] = 0;
        chars[']'] = 0;
        chars['^'] = K;
        chars['_'] = K;

        chars['`'] = 0;
        chars['a'] = K;
        chars['b'] = K;
        chars['c'] = K;
        chars['d'] = K;
        chars['e'] = K;
        chars['f'] = K;
        chars['g'] = K;
        chars['h'] = K;
        chars['i'] = K;
        chars['j'] = K;
        chars['k'] = K;
        chars['l'] = K;
        chars['m'] = K;
        chars['n'] = K;
        chars['o'] = K;
        chars['p'] = K;
        chars['q'] = K;
        chars['r'] = K;
        chars['s'] = K;
        chars['t'] = K;
        chars['u'] = K;
        chars['v'] = K;
        chars['w'] = K;
        chars['x'] = K;
        chars['y'] = K;
        chars['z'] = K;
        chars['{'] = 0;
        chars['|'] = 0;
        chars['}'] = 0;
        chars['~'] = K;
        chars[127] = 0;

        /*        
        chars[''] = K;
        chars[''] = 0;
        chars[''] = 0;
         */
    }

    // 許可された　第一の文字（種類）
    public boolean isAllowedFirstCharOfId(int code) {

        if (0 <= code && code < 128) {
            short kind = chars[code];

            return (kind == K);

        }
        return false;
    }

    // 許可された　第二の文字（種類）
    public boolean isAllowedRestCharOfId(int code) {

        if (0 <= code && code < 128) {
            short kind = chars[code];

            return (kind == K) || (kind == J);

        }
        return false;
    }

    /**
     * 0, 9(TAB), 10(LF), 13(CR) and SPACE
     *
     *
     * @param code
     * @return
     */
    public boolean isSpace(int code) {

        if (0 <= code && code < 128) {
            short kind = chars[code];

            return (kind == W);

        }
        return false;
    }

    public boolean isTerm(int code) {

        if (0 <= code && code < 128) {
            short kind = chars[code];

            return (kind == W) || (kind == 0);

        }

        if (code < 0) {
            return true;
        }
        return false;
    }

    public boolean isEOL(int code) {

        if (code == '\r' || code == '\n') {

            return true;
        }
        return false;
    }

    public short get(int code) {

        if (0 <= code && code < 128) {
            return chars[code];
        }
        return 0;
    }

}
