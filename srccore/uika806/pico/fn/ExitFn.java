/**
 * SPDX-License-Identifier: MPL-2.0
 * Author:  Masahito Hemmi.
 */
package uika806.pico.fn;

import uika806.kernel.AFn;

/**
 *
 * @author hemmi
 */
public class ExitFn extends AFn {

    @Override
    public String getName() {
        return "exit";
    }

    @Override
    public Object invoke(Object arg1) {
        
        if (arg1 instanceof Boolean) {
            Boolean b = (Boolean ) arg1;
            
            // TODO    すべての未解決の dynamic-wind after 手続きを実行し
            
            int code = (b) ? 0 : 1;
            System.exit(code);
            
        }
        
        return Boolean.TRUE;
    }

    /*
    
(exit) (exit obj)
process-context ライブラリ手続き process-context ライブラリ手続き
すべての未解決の dynamic-wind after 手続きを実行し，実 行中のプログラムを終了し，オペレーティングシステムに終 了値を通信する。引数が指定されていない，または obj が #t の場合，exit 手続きはプログラムが正常終了したことを オペレーティングシステムに通信する必要がある。もし obj が #f の場合は，exit 手続きはプログラムが異常終了した ことをオペレーティングシステムに通信する必要がある。そ うでない場合は，可能であれば exit は obj をオペレーティ ングシステムの適切な終了値に変換する必要がある。
exit 手続きは，例外通知または継続への返却をしてはなら ない。
注: ハンドラを実行するための要件のためであって，この手続き は単なるオペレーティングシステムの終了処理ではない。    
    */
    @Override
    public Object invoke() {
        return invoke(Boolean.TRUE);
    }
    
}
