/**
 * SPDX-License-Identifier: MPL-2.0
 * Author:  Masahito Hemmi.
 */
package uika806.port;

/**
 * Move uika806.kernel to uika806.small.port
 */
public interface IPort {

    //  (textual-port? obj)
    boolean isTextualPort();
    
    //  (binary-port? obj)
    boolean isBinaryPort();

    void close();

    boolean isOpen();

}
