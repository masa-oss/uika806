package uika806.pico.fn;

import uika806.kernel.AFn;

/**
 * <code>
 * すべての未解決の dynamic-wind after 手続きを実行せずに プログラムを終了し，exit と同様にオペレーティングシス テムに終了値を通信する。
 *
 * </code>
 *
 * @author hemmi
 */
public class EmergencyFn extends AFn {

    @Override
    public String getName() {
        return "emergency";
    }

    @Override
    public Object invoke(Object arg1) {

        if (arg1 instanceof Boolean) {
            Boolean b = (Boolean) arg1;
            int code = (b) ? 0 : 1;
            System.exit(code);

        }
        return Boolean.TRUE;
    }

    @Override
    public Object invoke() {
        return invoke(Boolean.TRUE);
    }

}
