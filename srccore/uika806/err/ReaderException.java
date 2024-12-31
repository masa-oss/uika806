/**
 * SPDX-License-Identifier: MPL-2.0
 * Author:  Masahito Hemmi.
 */
package uika806.err;

/**
 * 【開発メモ】このクラスは削除しても良いかも知れない。
 *           RaiseException の中に FileExceptionというフラグを持たせれば、事たりる。
 *
 */
//public class ReaderException extends LispException {
public class ReaderException extends RaiseException {

    private final int line;
    private final int charPos;
    final Throwable throwObj;

    public ReaderException(String msg) {
        super(msg, true);
        this.line = -1;
        this.charPos = -1;
        this.throwObj = null;
    }

    public ReaderException(String msg, int line, int charPos) {
        super(msg, true);
        this.line = line;
        this.charPos = charPos;
        this.throwObj = null;
    }

    public ReaderException(String message, Throwable cause, int line, int charPos) {
        super(message, true);
        this.line = line;
        this.charPos = charPos;
        
        this.throwObj = cause;
    }
    
    @Override
    public synchronized Throwable getCause() {
        return throwObj;
    }
    
    @Override
    public String toString() {
        return "ReaderException[" + super.getMessage() + ", lin=" + getLine()
                 + ", charPos=" + getCharPos() + "]";
    }

    /**
     * @return the line
     */
    public int getLine() {
        return line;
    }

    /**
     * @return the charPos
     */
    public int getCharPos() {
        return charPos;
    }
    
}
