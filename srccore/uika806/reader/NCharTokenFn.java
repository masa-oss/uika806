/**
 * SPDX-License-Identifier: MPL-2.0
 * Author:  Masahito Hemmi.
 */
package uika806.reader;

import uika806.fn011.reader.Tokenizer;
import uika806.fn011.reader.Token;
import java.util.List;
import uika806.port.InputPort;

/**
 *  区切り文字まで、一気に読んでしまう
 * 
 * 
 * @author hemmi
 */
public class NCharTokenFn extends TokenFn {
 
    
    @Override
    public Token read( Tokenizer toknzr,  InputPort in, boolean error) {
        
        int ch = in.readCodepoint();

        List<Integer> list = toknzr.readToTerm(ch);

        return new Token('#', list);
    }
}
