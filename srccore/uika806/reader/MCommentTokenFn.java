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
 *
 * @author hemmi
 */
public class MCommentTokenFn extends TokenFn {
 
    
    @Override
    public Token read( Tokenizer toknzr,  InputPort in, boolean error) {
        
        in.readCodepoint();

        ArrayList<Integer> list = new ArrayList<>();
        for (;;) {
            int ch = toknzr.in.peekCodepoint();

            if ('|' == (ch)) {
                in.readCodepoint();
                int ch2 = toknzr.in.peekCodepoint();
                if (ch2 == '#') {
                    in.readCodepoint();
                    break;
                } else {
                    list.add(ch);
                    ch = ch2;
                }
            }
            list.add(ch);
            in.readCodepoint();
        }

        return new Token(0xa3, list);
    }
    
    
}
