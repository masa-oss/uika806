/**
 * SPDX-License-Identifier: MPL-2.0
 * Author:  Masahito Hemmi.
 */
package uika806.port;

/**
 *   LispReaderが文字を読むためのストリーム
 *
 */
public interface InputPort extends IPort {
    
    /**
     *  次の文字を覗き見る。
     * @return  -1 if end-of-file
     */
    int peekCodepoint();

    /**
     *  次のバイトを覗き見る。
     * @return  -1 if end-of-file
     */
    int peekU8();
    
    
    /**
     * 次の１文字を得る。
     * 通常Unicode文字なので、
     * 返される値は、0 〜 0xD7FF か E000〜 0x10FFFFの範囲となる。
     * 
     *  EOF では -1が帰る。
     * @return Codepoint
     */
    int readCodepoint();
    
    /**
     *  現在読み込んでいる行番号。先頭は１
     * @return 
     */
    int getLineNum();

    /**
     *  現在読み込んでいる文字位置。先頭は１
     * @return 
     */
    int getNthChar();
    
    
    boolean ready();
    
    
    int readByte();
    
}
