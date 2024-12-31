/**
 * SPDX-License-Identifier: MPL-2.0
 * Author:  Masahito Hemmi.
 */
package uika806.port;

import java.io.IOException;

public interface OutputPort extends IPort {

    void write(int codePoint) throws IOException;

    void flush();


    void writeByte(int by);


    void write(String str);
    
    /* Deprecated
    default void writeString(String str) {

        OutputPort outport = this;
        try {
            int n = str.length();
            for (int i = 0; i < n; i++) {

                int cc = str.codePointAt(i);
                char c = str.charAt(i);

                if (Character.isSurrogate(c)) {
                    i++;
                }
                outport.write(cc);
            }
        } catch (IOException ioe) {
            throw new FileException("IOException", ioe);
        }
    }
*/

}
