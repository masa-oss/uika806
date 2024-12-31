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
public class OneCharTokenFn extends TokenFn {

    @Override
    public Token read(Tokenizer toknzr, InputPort in, boolean error) {

        // peekの後呼ばれるので、それを読む
        int ch = in.peekCodepoint();
        if (ch != 0x28) {
            throw new ReaderException("#の次が(でない",
                    toknzr.in.getLineNum(),
                    toknzr.in.getNthChar()
            );
        }
        return new Token(0xa4, null);
    }

}
