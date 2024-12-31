/**
 * SPDX-License-Identifier: MPL-2.0
 * Author:  Masahito Hemmi.
 */
package uika806.err;

/**
 * 【開発メモ】このクラスは削除しても良いかも知れない。
 *           RaiseException の中に FileExceptionというフラグを持たせれば、事たりる。
 * 
 * 
 * ファイルの入力または出力ポートを開けないこ とによって発生したオブジェクト
 *
 */
// public class FileException extends LispException {
public class FileException extends RaiseException {

    final Throwable throwObj;

    public FileException(String msg) {
        super(msg, true);
        this.throwObj = null;
    }

    public FileException(String msg, Throwable th) {
        super(msg, true);
        this.throwObj = th;
    }

    @Override
    public synchronized Throwable getCause() {
        return throwObj;
    }
    
}
