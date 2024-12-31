/**
 * SPDX-License-Identifier: MPL-2.0
 * Author:  Masahito Hemmi.
 */
package uika806.reader;

import uika806.err.ReaderException;
import uika806.fn011.reader.Tokenizer;
import uika806.fn011.reader.Token;
import uika806.port.InputPort;

/**
 * １文字読む
 *
 *
 * @author hemmi
 */
public class UcharTokenFn extends TokenFn {

    @Override
    public Token read(Tokenizer toknzr, InputPort in, boolean error) {

        // peekの後呼ばれるので、それを読む
        int ch = in.peekCodepoint();
        if (ch != 'u') {
            throw new ReaderException("#の次がuでない",
                    toknzr.in.getLineNum(),
                    toknzr.in.getNthChar()
            );
        }
        in.readCodepoint();  // u
        int ch2 = in.peekCodepoint();  // may be 8
        if (ch2 != '8') {
            throw new ReaderException("#の次がu8でない",
                    toknzr.in.getLineNum(),
                    toknzr.in.getNthChar()
            );
        }
        in.readCodepoint();  // 8
        
        return new Token(0xa5, null);
    }

}
