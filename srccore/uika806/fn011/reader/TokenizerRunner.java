/**
 * SPDX-License-Identifier: MPL-2.0
 * Author:  Masahito Hemmi.
 */

package uika806.fn011.reader;

import uika806.port.CodepointLispStream;
import java.io.FileInputStream;
import java.io.InputStreamReader;
import java.io.StringWriter;

public class TokenizerRunner {

    public static void main(String[] argv) {

    //    String file = "Rust/test_pr7rs";
        String file = "gauche/test17_ng_token.txt";

        try {
            FileInputStream fis = new FileInputStream(file);
            try {
                InputStreamReader isr = new InputStreamReader(fis, "UTF-8");
                StringWriter sb = new StringWriter();
                isr.transferTo(sb);

                String str = sb.toString();

                CodepointLispStream stream = CodepointLispStream.fromUtf8(str);

                Tokenizer tk = new Tokenizer(stream);

                for (int i = 0; i < 10000; i++) {

                    Token to = tk.next();

                    int id = to.getId();
                    if (id != '\n') {

                        System.out.println(" " + to);
                    }
                    if (id == -1) {
                        break;
                    }
                }

            } finally {
                fis.close();
            }
        } catch (Exception ex) {
            ex.printStackTrace();
        }
        System.out.println("----------- END -----------");
    }

}
