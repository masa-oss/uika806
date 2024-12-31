/**
 * SPDX-License-Identifier: MPL-2.0
 * Author:  Masahito Hemmi.
 */
package uika806.reader;

import uika806.fn011.reader.Tokenizer;
import uika806.fn011.reader.Token;
import java.util.ArrayList;
import uika806.port.InputPort;

/**
 * # のトークナイザ
 *
 * @author hemmi
 */
public class SharpReader {

    TokenFn[] chars;

    public SharpReader() {

        this.chars = new TokenFn[128];
        initR5();
    }

    final void initR5() {

        chars[0] = new TokenFn(); //Error
        chars[1] = new TokenFn();
        chars[2] = new TokenFn();
        chars[3] = new TokenFn();
        chars[4] = new TokenFn();
        chars[5] = new TokenFn();
        chars[6] = new TokenFn();
        chars[7] = new TokenFn();
        chars[8] = new TokenFn();
        chars[9] = new TokenFn();
        chars[10] = new TokenFn();
        chars[11] = new TokenFn();
        chars[12] = new TokenFn();
        chars[13] = new TokenFn();
        chars[14] = new TokenFn();
        chars[15] = new TokenFn();
        chars[16] = new TokenFn();
        chars[17] = new TokenFn();
        chars[18] = new TokenFn();
        chars[19] = new TokenFn();
        chars[20] = new TokenFn();
        chars[21] = new TokenFn();
        chars[22] = new TokenFn();
        chars[23] = new TokenFn();
        chars[24] = new TokenFn();
        chars[25] = new TokenFn();
        chars[26] = new TokenFn();
        chars[27] = new TokenFn();
        chars[28] = new TokenFn();
        chars[29] = new TokenFn();
        chars[30] = new TokenFn();
        chars[31] = new TokenFn();

        chars[' '] = new TokenFn();
        chars['!'] = new NCharTokenFn();
        chars['"'] = new NCharTokenFn();
        chars['#'] = new TokenFn();
        chars['$'] = new NCharTokenFn();
        chars['%'] = new NCharTokenFn();
        chars['&'] = new NCharTokenFn();
        chars['\''] = new NCharTokenFn();
        chars['('] = new OneCharTokenFn();
        chars[')'] = new TokenFn();         // Error
        chars['*'] = new NCharTokenFn();
        chars['+'] = new NCharTokenFn();
        chars[','] = new NCharTokenFn();
        chars['-'] = new NCharTokenFn();
        chars['.'] = new NCharTokenFn();
        chars['/'] = new NCharTokenFn();
        chars['0'] = new NCharTokenFn();
        chars['1'] = new NCharTokenFn();
        chars['2'] = new NCharTokenFn();
        chars['3'] = new NCharTokenFn();
        chars['4'] = new NCharTokenFn();
        chars['5'] = new NCharTokenFn();
        chars['6'] = new NCharTokenFn();
        chars['7'] = new NCharTokenFn();
        chars['8'] = new NCharTokenFn();
        chars['9'] = new NCharTokenFn();
        chars[':'] = new NCharTokenFn();
        chars[';'] = new NCharTokenFn();
        chars['<'] = new NCharTokenFn();
        chars['='] = new NCharTokenFn();
        chars['>'] = new NCharTokenFn();
        chars['?'] = new NCharTokenFn();

        chars['@'] = new TokenFn(); // Error
        chars['A'] = new TokenFn();
        chars['B'] = new TokenFn();
        chars['C'] = new TokenFn();
        chars['D'] = new TokenFn();
        chars['E'] = new TokenFn();
        chars['F'] = new TokenFn();
        chars['G'] = new TokenFn();
        chars['H'] = new TokenFn();
        chars['I'] = new TokenFn();
        chars['J'] = new TokenFn();
        chars['K'] = new TokenFn();
        chars['L'] = new TokenFn();
        chars['M'] = new TokenFn();
        chars['N'] = new TokenFn();
        chars['O'] = new TokenFn();
        chars['P'] = new TokenFn();
        chars['Q'] = new TokenFn();
        chars['S'] = new TokenFn();
        chars['T'] = new TokenFn();
        chars['U'] = new TokenFn();
        chars['V'] = new TokenFn();
        chars['W'] = new TokenFn();
        chars['X'] = new NCharTokenFn();
        chars['Y'] = new TokenFn();
        chars['Z'] = new TokenFn();
        chars['['] = new TokenFn();
        chars['\\'] = new NCharTokenFn();
        chars[']'] = new TokenFn();
        chars['^'] = new NCharTokenFn();
        chars['_'] = new NCharTokenFn();

        chars['`'] = new NCharTokenFn();
        chars['a'] = new TokenFn();       // #a  - gosh でエラー
        chars['b'] = new NCharTokenFn(); // #b 許可されている
        chars['c'] = new TokenFn();
        chars['d'] = new NCharTokenFn(); // TokenFn();
        chars['e'] = new NCharTokenFn();  // #e 許可されている
        chars['f'] = new TokenFn();
        chars['g'] = new TokenFn();
        chars['h'] = new TokenFn();
        chars['i'] = new NCharTokenFn();  // TokenFn();
        chars['j'] = new TokenFn();
        chars['k'] = new TokenFn();
        chars['l'] = new TokenFn();
        chars['m'] = new TokenFn();
        chars['n'] = new TokenFn();
        chars['o'] = new NCharTokenFn(); // TokenFn();
        chars['p'] = new TokenFn();
        chars['q'] = new TokenFn();
        chars['r'] = new TokenFn();
        chars['s'] = new TokenFn();
        chars['t'] = new TokenFn();
        chars['u'] = new UcharTokenFn();  // #u8( ...)
        chars['v'] = new TokenFn();
        chars['w'] = new TokenFn();
        chars['x'] = new NCharTokenFn(); //TokenFn();
        chars['y'] = new TokenFn();
        chars['z'] = new TokenFn();
        chars['{'] = new TokenFn();
        chars['|'] = new MCommentTokenFn();  // #| comment |#
        chars['}'] = new TokenFn();
        chars['~'] = new NCharTokenFn();
        chars[127] = new TokenFn();
    }

    public Token read(Tokenizer toknzr, InputPort in) {

        ArrayList<Integer> buf = new ArrayList<>();

        int ch = in.peekCodepoint();
        if (ch == 't' || ch == 'f') {
            // #t or #f --> Boolean

            in.readCodepoint();
            buf.add(ch);
            return new Token('#', buf);

        } else if (ch == ';') {
            in.readCodepoint();
            return new Token(0xa6, buf);
            
        } else if (0 <= ch && ch <= 127) {
            TokenFn fn = chars[ch];
            return fn.read(toknzr, in, true);

        } else {
            int line = in.getLineNum();
            int nchar = in.getNthChar();
            throw new RuntimeException("? " + String.valueOf(ch) + ", line =" + line
                    + ", char=" + nchar);
        }

    }

}
