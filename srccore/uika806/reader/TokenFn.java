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
 *
 * @author hemmi
 */
public class TokenFn {

    public Token read(Tokenizer toknzr, InputPort in, boolean error) {

        int ch = in.peekCodepoint();
        int lin = in.getLineNum();
        int pos = in.getNthChar();
        throw new ReaderException("Unsupport " + ch, lin, pos);
    }

}
